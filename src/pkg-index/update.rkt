#lang racket/base
(require racket/list
         racket/function
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
  (filter (λ (pkg-name)
            (cond
              [(package-exists? pkg-name)
               (update-checksum force? pkg-name)]
              [else (log! "update-checksums: invariant broken; ~a doesn't exist" pkg-name)
                    ;; considered not update
                    #f]))
          pkgs))

;; precondition: pkg-name must exist
(define (update-checksum force? pkg-name)
  (log! "update-checksum ~v ~v" force? pkg-name)
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
         #:pkg-name pkg-name))
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
                             #:pkg-name pkg-name))
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
                      (hash-has-key? i 'license)))
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
   (checksum module-paths (list deps rt-deps license implies collection))
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
              (get-info 'collection (λ () #f)))
        (list empty empty missing empty #f)))))

  (package-begin
   (define* i (hash-set i 'modules module-paths))
   (define* i (hash-set i 'dependencies deps))
   (define* i (hash-set i 'rt-dependencies rt-deps))
   (define* i (hash-set i 'license
                        (cond
                          [(eq? license missing) #f]
                          [else (format "~s" license)])))
   (define* i (hash-set i 'implies implies))
   ;; avoid conflation of symbols and strings in JSON
   (define* i (hash-set i 'collection (if (eq? collection 'multi) (list 'multi) collection)))
   i))

(define (do-update! pkgs)
  (notify! "package sources being checked for updates")
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
