#lang racket/base
(require racket/list
         racket/match
         racket/file
         racket/port
         net/http-client
         (prefix-in pkg: pkg/lib)
         "common.rkt"
         "config.rkt"
         "notify.rkt"
         "../send-email.rkt")

(define SUMMARY-HOST "pkg-build.racket-lang.org")
(define SUMMARY-URL (string-append "/" SUMMARY-NAME))
(define SUMMARY-ETAG-PATH (build-path cache-path (format "~a.etag" SUMMARY-NAME)))

(define (extract-tag hs)
  (or
   (for/or ([h (in-list hs)])
     (match h
       [(regexp
         #rx#"^(?i:ETag): (.*?)$"
         (list _ tag-bys))
        tag-bys]
       [_
        #f]))
   #""))

(define (file->bytes* p d)
  (if (file-exists? p)
    (file->bytes p)
    d))

(define (file->value* p d)
  (if (file-exists? p)
      (with-handlers ([exn:fail? (λ (x) d)])
        (file->value p))
      d))

(define (build-update!)
  (define cur-version
    (file->bytes* SUMMARY-ETAG-PATH #""))
  (log! "build-update: Current: ~v\n" cur-version)

  (define-values
    (_0 head-headers _1)
    (http-sendrecv
     SUMMARY-HOST SUMMARY-URL
     #:ssl? #t
     #:method #"HEAD"))
  (define head-version
    (extract-tag head-headers))
  (log! "build-update: Head: ~v\n" head-version)

  (unless (bytes=? cur-version head-version)
    (define-values
      (_2 get-headers get-ip)
      (http-sendrecv
       SUMMARY-HOST SUMMARY-URL
       #:ssl? #t
       #:method #"GET"))
    (define get-version
      (extract-tag get-headers))
    (log! "build-update: Get: ~v\n" get-version)

    (define new-file
      (make-temporary-file "summary-~a.rktd" #f cache-path))
    (call-with-output-file new-file
      #:exists 'truncate/replace
      (λ (new-op)
        (copy-port get-ip new-op)))

    (with-output-to-file SUMMARY-ETAG-PATH
      #:exists 'truncate/replace
      (λ ()
        (write-bytes get-version)))

    (define old-summary (file->value* SUMMARY-PATH #f))
    (define new-summary
      (with-handlers ([exn:fail? (λ (x) #f)])
        (call-with-input-file new-file file->value)))
    (with-handlers ([exn:fail?
                     (λ (x)
                       (log! "build-update: notification pass failed: ~a"
                             (exn-message x)))])
      (notify-build-transitions! old-summary new-summary))

    (rename-file-or-directory new-file SUMMARY-PATH #t)))

;; Derives a coarse status from a summary entry (a hash of build log
;; paths), or 'unknown for an absent entry. Dependency failures are
;; deliberately not a failing status: they are usually caused by
;; another package, so this package's authors cannot fix them.
(define (build-status bs)
  (cond
    [(not bs) 'unknown]
    [(hash-ref bs 'failure-log #f) 'build-failure]
    [(hash-ref bs 'test-failure-log #f) 'test-failure]
    [else 'ok]))

(define (failing? status)
  (memq status '(build-failure test-failure)))

;; Extracts plausible email addresses from an author string such as
;; "Ryan Culpepper <ryanc@example.org>" or "user@example.org".
(define (author-emails author-str)
  (remove-duplicates
   (regexp-match* #rx"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+" (or author-str ""))))

;; Returns the sorted names of packages that are failing in
;; `new-summary` but were not failing in `old-summary`.
(define (newly-failing old-summary new-summary)
  (sort
   (filter values
           (for/list ([(name new-bs) (in-hash new-summary)])
             (define name-str (if (symbol? name) (symbol->string name) name))
             (and (failing? (build-status new-bs))
                  (not (failing? (build-status (hash-ref old-summary name #f))))
                  name-str)))
   string<?))

;; Emails the authors of packages whose build status newly became a
;; failure. Only ok->failing transitions notify, so a package that
;; keeps failing does not re-notify on every run. Without a previous
;; summary on disk (cold start) nothing is notified; a server that
;; already has one compares against it, so the first run after
;; deploying this code notifies only transitions since that snapshot.
(define (notify-build-transitions! old-summary new-summary)
  (unless (and (hash? old-summary) (hash? new-summary))
    (log! "build-update: no comparable previous summary; skipping notifications")
    (void))
  (when (and (hash? old-summary) (hash? new-summary))
    (define sender
      (or (hash-ref (config) 'email-sender-address #f)
          "pkgs@racket-lang.org"))
    (for ([name (in-list (newly-failing old-summary new-summary))])
      (define emails
        (author-emails (hash-ref (read-package-info name) 'author #f)))
      (cond
        [(null? emails)
         (log! "build-update: ~a is newly failing but no author email was found" name)]
        [else
         (log! "build-update: notifying authors of ~a about a new build failure" name)
         (define log-path
           (or (hash-ref (hash-ref new-summary name #f) 'failure-log #f)
               (hash-ref (hash-ref new-summary name #f) 'test-failure-log #f)))
         (with-handlers ([exn:fail?
                          (λ (x)
                            (log! "build-update: notification for ~a failed: ~a"
                                  name (exn-message x)))])
           (send-email
            sender
            (format "New Racket package build failure: ~a" name)
            emails
            (list
             (format "The package ~a has started failing on the Racket"
                     name)
             "package build farm."
             ""
             (format "Status: ~a" (build-status (hash-ref new-summary name #f)))
             (format "Build log: ~a"
                     (if log-path
                         (format "https://~a/~a" SUMMARY-HOST log-path)
                         "N/A"))
             ""
             "This message is sent when the package's build status changes"
             "from passing to failing; it does not repeat while the failure"
             "continues.")))]))))

(define (do-build-update! l)
  (notify! "package build status being checked for updates")
  (build-update!)
  (notify! ""))
(define (run-build-update!)
  (run! do-build-update! empty))
(define run-sema (make-semaphore 1))
(define (signal-build-update!)
  (safe-run! run-sema (λ () (run-build-update!))))

(provide do-build-update!
         signal-build-update!)

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "build-update"
   #:args ()
   (do-build-update! empty)))
