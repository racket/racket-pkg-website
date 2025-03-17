#lang racket/base
(require racket/file
         racket/runtime-path
         racket/match
         racket/list
         racket/date
         racket/string
         pkg/private/stage
         plt-service-monitor/beat
         infrastructure-userdb
         "config.rkt"
         "../default.rkt")

;; This f o f^-1 is applied because it throws an error if file is not
;; a single path element. This causes things like "../../etc/passwd"
;; to throw errors and thus be protected.
(define (build-path^ base file)
  (build-path base (path-element->string (string->path-element file))))

(define-runtime-path src* ".")

(define src (get-config src src*))
(define root (get-config root default-root))

(make-directory* root)
(define users.new-path (get-config users.new-path (default-users root)))
(define userdb (userdb-config users.new-path
                              #f ;; write not permitted. The racket-pkg-website does all writes.
                              ))

;; Since package downloads don't normally use the GitHub API anymore,
;; allow the GitHub options to be #f and make the default load strings
;; only if default files exist
(let ((check+load-file (lambda (filename)
                         (if (file-exists? filename)
                             (file->string filename)
                             (begin
                               #;(raise-user-error 'pkg-index "Cannot find file ~a" filename)
                               #f)))))
  (github-client_id (get-config github-client_id
                                (check+load-file (build-path root "client_id"))))
  (github-client_secret (get-config github-client_secret
                                    (check+load-file (build-path root "client_secret")))))

(define cache-path (get-config cache-path (build-path root "cache")))
(make-directory* cache-path)

(define SUMMARY-NAME "summary.rktd")
(define SUMMARY-PATH (build-path cache-path SUMMARY-NAME))

(define pkgs-path (get-config pkgs-path (build-path root "pkgs")))
(make-directory* pkgs-path)

(define static.src-path (get-config static.src-path (build-path src "static")))
(define static-path (get-config static-path default-static-gen))
(define notice-path (get-config notice-path (build-path static-path "notice.json")))
(make-directory* static-path)

(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))

(define (package-exists? pkg-name)
  (file-exists? (build-path^ pkgs-path pkg-name)))

(define (read-package-info pkg-name)
  (with-handlers ([exn:fail?
                   (λ (x)
                     ((error-display-handler)
                      (exn-message x)
                      x)
                     (hasheq))])
    (define p
      (build-path^ pkgs-path pkg-name))
    (define v
      (if (package-exists? pkg-name)
          (file->value p)
          (hasheq)))
    (define ht
      (if (hash? v)
          v
          (hasheq)))
    ht))

(define (package-info pkg-name #:version [version #f])
  (define ht (read-package-info pkg-name))
  (define no-version
    (hash-set ht 'name pkg-name))
  (cond
   [(and version
         (hash-has-key? no-version 'versions)
         (hash? (hash-ref no-version 'versions #f))
         (hash-has-key? (hash-ref no-version 'versions) version)
         (hash? (hash-ref (hash-ref no-version 'versions) version #f)))
    =>
    (λ (version-ht)
      (hash-merge version-ht no-version))]
   [else
    no-version]))

(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                [(or 'author 'source)
                 (error 'pkg "Package ~e is missing a required field: ~e"
                        (hash-ref pkg-info 'name) key)]
                ['checksum
                 ""]
                ['ring
                 2]
                ['checksum-error
                 #f]
                ['tags
                 empty]
                ['versions
                 (hash)]
                [(or 'last-checked 'last-edit 'last-updated)
                 -inf.0]))))

(define (package-info-set! pkg-name i)
  (call-with-atomic-output-file
   (build-path^ pkgs-path pkg-name)
   (lambda (out)
     (write i out))))

(define (hash-merge from to)
  (for/fold ([to to])
            ([(k v) (in-hash from)])
    (hash-set to k v)))

(define (author->list as)
  (string-split as))

(define (valid-name? t)
  (not (regexp-match #rx"[^a-zA-Z0-9_\\-]" t)))

(define (valid-author? a)
  (not (regexp-match #rx"[ :]" a)))

(define valid-tag?
  valid-name?)

(define (log!* args suffix)
  (parameterize ([date-display-format 'iso-8601])
    (printf "~a: ~a~a" (date->string (current-date) #t)
            (apply format args)
            suffix)
    (flush-output)))

(define (log! . args)
  (log!* args "\n"))

(define (log!/no-newline . args)
  (log!* args ""))

(define (run! f args)
  (log! "START ~a ~v" f args)
  (f args)
  (log! "END ~a ~v" f args))

(define (safe-run! run-sema t)
  (thread
   (λ ()
     (call-with-semaphore run-sema
       (λ ()
         (with-handlers ([exn:fail? (λ (x) ((error-display-handler)
                                            (exn-message x)
                                            x))])
           (t)))))))

(define s3-bucket (get-config s3-bucket #f))
(define s3-bucket-region (get-config s3-bucket-region #f))

(define beat-s3-bucket (get-config beat-s3-bucket #f))
(define (heartbeat task)
  (when beat-s3-bucket
    (beat beat-s3-bucket task)))

(provide (all-defined-out))
(provide (all-from-out "config.rkt"))
