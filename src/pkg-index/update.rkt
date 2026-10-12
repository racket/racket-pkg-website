#lang racket/base
(require racket/list
         racket/function
         racket/string
         racket/system
         racket/package
         racket/match
         pkg/private/stage
         (prefix-in pkg: pkg/lib)
         "common.rkt"
         "notify.rkt"
         "static.rkt"
         "../readme.rkt")

(define (update-all [force? #t])
  (update-checksums force? (package-list)))
(define (update-pkgs pkgs)
  (update-checksums #t pkgs))

;; When an update is not forced, then a package is checked only
;; if it has been this long since the last check:
(define non-force-wait-seconds (* 1 60 60))

(define (update-checksums force? pkgs)
  (define cache (make-hash))
  (define len (length pkgs))
  (for/list ([pkg-name (in-list pkgs)]
             [i (in-naturals 1)]
             #:when
             (cond
               [(package-exists? pkg-name)
                (update-checksum force? pkg-name cache i len)]
               [else (log! "update-checksums: invariant broken; ~a doesn't exist" pkg-name)
                     ;; considered not update
                     #f]))
    pkg-name))

;; precondition: pkg-name must exist
(define (update-checksum force? pkg-name cache i len)
  (log! "update-checksum ~v ~v (~a of ~a)" force? pkg-name i len)
  (with-handlers
      ([exn:fail?
        (λ (x)
          (with-handlers
              ([exn:fail?
                (λ (x2)
                  ((error-display-handler)
                   (format 
                    "second error (~v) while catching error (~v) while updating (~v)"
                    (exn-message x2)
                    (exn-message x)
                    pkg-name)
                   x2))])
            (package-begin
             (define* i (package-info pkg-name))
             (define checksum-error/unredacted
               (let ([the-string-port (open-output-string)])
                 (parameterize ([current-error-port the-string-port])
                   ((error-display-handler)
                    (exn-message x)
                    x))
                 (get-output-string the-string-port)))
             (define checksum-error
               (let ([secret (github-client_secret)])
                 (if secret
                     (regexp-replace* (regexp secret)
                                      checksum-error/unredacted
                                      "REDACTED")
                     checksum-error/unredacted)))
             (define updated?
               (or (not (equal? (package-ref i 'checksum-error) checksum-error))
                   (eq? -inf.0 (package-ref i 'last-updated))))
             (define now (current-seconds))
             (define* i
               (hash-set i 'last-checked now))
             (define* i
               (if updated?
                   (hash-set i 'last-updated now)
                   i))
             (package-info-set!
              pkg-name
              (hash-set i 'checksum-error checksum-error))
             updated?)))])
    (define i (package-info pkg-name))
    (define old-checksum (package-ref i 'checksum))
    (define now (current-seconds))
    (define last (hash-ref i 'last-checked -inf.0))
    (define changed? #f)
    (when (or force?
              (>= (- now last) non-force-wait-seconds)
              (not old-checksum))
      (log! "\tupdating ~a" pkg-name)
      (define new-checksum
        (package-url->checksum
         (package-ref i 'source)
         #:pkg-name pkg-name
         #:cache cache
         #:download-printf log!/no-newline))
      (unless (equal? new-checksum old-checksum)
        (log! "\told: ~v" old-checksum)
        (log! "\tnew: ~v" new-checksum)
        (set! changed? #t))
      (package-begin
       (define* i
         (hash-set i 'checksum
                   (or new-checksum
                       old-checksum)))
       (define* i
         (hash-set i 'last-checked now))
       (define* i
         (if (and (equal? new-checksum old-checksum)
                  (hash-has-key? i 'readme-url))
             i
             (hash-set i 'readme-url (source->readme-url (package-ref i 'source)))))
       (define* i
         (hash-update i 'versions
                      (λ (v-ht)
                        (for/hash ([(v vi) (in-hash v-ht)])
                          (define old-checksum (hash-ref vi 'checksum ""))
                          (define new-checksum
                            (package-url->checksum
                             (hash-ref vi 'source "")
                             #:pkg-name pkg-name
                             #:cache cache
                             #:download-printf log!/no-newline))
                          (unless (equal? new-checksum old-checksum)
                            (log! "\t~a old: ~v" vi old-checksum)
                            (log! "\t~a new: ~v" vi new-checksum)
                            (set! changed? #t))
                          (values v
                                  (hash-set vi 'checksum
                                            (or new-checksum
                                                old-checksum)))))
                      hash))
       (define* i
         (cond
           [(not new-checksum)
            i]
           [(and (equal? new-checksum old-checksum)
                 ;; update if essential fields are not present
                 (and (hash-has-key? i 'modules)
                      (hash-has-key? i 'implies)
                      (hash-has-key? i 'collection)
                      (hash-has-key? i 'rt-dependencies)
                      (hash-has-key? i 'license)
                      ;; a non-empty description is essential too; refresh
                      ;; content so it can be filled in from package metadata
                      (let ([d (hash-ref i 'description #f)])
                        (and (string? d) (positive? (string-length d))))))
            i]
           [else
            (define next-i (update-from-content i))
            (if (equal? new-checksum old-checksum)
              next-i
              (hash-set next-i 'last-updated now))]))
       (define* i
         (hash-set i 'checksum-error #f))
       (log! "\twriting with checksum ~v" (hash-ref i 'checksum))
       (package-info-set! pkg-name i)))
    changed?))

(define missing (gensym 'missing))

(define (update-from-content i)
  (log! "\tgetting package content for ~v" (hash-ref i 'name))
  (match-define-values
   (checksum module-paths (list deps rt-deps license implies collection
                                umbrella language-families build-platforms
                                content-desc))
   (pkg:get-pkg-content
    (pkg:pkg-desc (hash-ref i 'source)
                  #f
                  (hash-ref i 'name)
                  (hash-ref i 'checksum)
                  #f)
    #:extract-info
    (λ (get-info)
      (if get-info
        (list (pkg:extract-pkg-dependencies get-info)
              (pkg:extract-pkg-dependencies get-info #:build-deps? #f)
              (get-info 'license (λ () missing))
              (get-info 'implies (λ () empty))
              (get-info 'collection (λ () #f))
              (get-info 'umbrella (λ () #f))
              (get-info 'language-families (λ () #f))
              (get-info 'build-platforms (λ () #f))
              (content-description get-info))
        ;; the fallback list must match the destructuring above
        (list empty empty missing empty #f #f #f #f #f)))))

  (define (guard v pred alt-v) (if (pred v) v alt-v))

  (package-begin
   (define* i (hash-set i 'modules module-paths))
   (define* i (hash-set i 'dependencies deps))
   (define* i (hash-set i 'rt-dependencies rt-deps))
   (define* i (hash-set i 'license
                        (cond
                          [(eq? license missing) #f]
                          [else (format "~s" license)])))
   (define* i (hash-set i 'implies (guard implies
                                          (lambda (v) (or (string? v) (eq? v 'core)))
                                          empty)))
   ;; avoid conflation of symbols and strings in JSON
   (define* i (hash-set i 'collection (if (eq? collection 'multi) (list 'multi) collection)))
   ;; fill in an empty registration description from the package's own
   ;; metadata; never overwrite a description already set by the author
   (define* i
     (hash-set i 'description
               (let ([cur (hash-ref i 'description #f)])
                 (if (and (string? content-desc)
                          (not (and (string? cur) (positive? (string-length cur)))))
                     content-desc
                     cur))))
   (define* i (hash-set i 'umbrella (guard umbrella
                                           string?
                                           #f)))
   (define* i (hash-set i 'language-families (guard language-families
                                                    (lambda (l) (and (list? l)
                                                                     (andmap string? l)))
                                                    '("Racket"))))
   (define* i (hash-set i 'build-platforms (guard build-platforms
                                                  (lambda (l)
                                                    (or (not l)
                                                        (and (list? l)
                                                             (andmap (lambda (i) (or (string? i) (symbol? i)))
                                                                     l))))
                                                  #f)))
   i))

;; Extracts a catalog description from package metadata. Only used to fill
;; in an empty registration description. `blurb` may be a string or a list
;; of strings; `pkg-desc` is a plain string.
(define (content-description get-info)
  (define v (get-info 'blurb (λ () (get-info 'pkg-desc (λ () #f)))))
  (cond
    [(string? v) v]
    [(and (pair? v) (andmap string? v)) (string-join v " ")]
    [else #f]))

(define (do-update! pkgs)
  (notify! "package sources being checked for updates")
  (define start-seconds (current-seconds))
  (log! "update: checking ~v" pkgs)
  (define changed
    (cond
     [(eq? pkgs 'all)
      (update-all #f)]
     [(eq? pkgs 'all/force)
      (update-all #t)]
     [else
      (update-pkgs pkgs)]))
  (log! "update: changes ~v" changed)
  (log! "update: took ~a seconds" (- (current-seconds) start-seconds))
  (signal-static! changed))
(define (run-update! pkgs beat?)
  (run! do-update! pkgs)
  (when beat?
    (heartbeat (get-config beat-update-task-name "pkgd-update"))))
(define run-sema (make-semaphore 1))
(define (signal-update!* pkgs beat?)
  (safe-run! run-sema (λ () (run-update! pkgs beat?))))
(define (signal-update! pkgs)
  (signal-update!* pkgs #f))
(define (signal-update!/beat pkgs)
  (signal-update!* pkgs #t))

(provide do-update!
         signal-update!
         signal-update!/beat)

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "update"
   #:args pkgs
   (do-update! (if (null? pkgs)
                   'all
                   pkgs))))
