#lang racket/base
(require racket/list
         racket/file
         racket/port
         (prefix-in http: net/http-easy)
         (prefix-in pkg: pkg/lib)
         "common.rkt"
         "notify.rkt")

(define SUMMARY-URL (string-append "https://pkg-build.racket-lang.org/" SUMMARY-NAME))
(define SUMMARY-ETAG-PATH (build-path cache-path (format "~a.etag" SUMMARY-NAME)))

(define (file->bytes* p d)
  (if (file-exists? p)
    (file->bytes p)
    d))

(define (check-summary-response response)
  (define status (http:response-status-code response))
  (unless (<= 200 status 299)
    (error 'build-update! "unexpected HTTP status ~a for ~a" status SUMMARY-URL)))

(define (build-update!)
  (define cur-version
    (file->bytes* SUMMARY-ETAG-PATH #""))
  (log! "build-update: Current: ~v\n" cur-version)

  (define head (http:head SUMMARY-URL #:max-redirects 0))
  (check-summary-response head)
  (define head-version (or (http:response-headers-ref head 'etag) #""))
  (log! "build-update: Head: ~v\n" head-version)

  (unless (bytes=? cur-version head-version)
    (define response (http:get SUMMARY-URL #:max-redirects 0 #:stream? #t))
    (define get-version
      (dynamic-wind
        void
        (lambda ()
          (check-summary-response response)
          (call-with-atomic-output-file
           SUMMARY-PATH
           (lambda (out _)
             (copy-port (http:response-output response) out)))
          (or (http:response-headers-ref response 'etag) #""))
        (lambda () (http:response-close! response))))

    (with-output-to-file SUMMARY-ETAG-PATH
      #:exists 'truncate/replace
      (λ ()
        (write-bytes get-version)))))

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
