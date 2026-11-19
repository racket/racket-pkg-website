#lang racket/base
(require racket/list
         racket/file
         (prefix-in http: net/http-easy)
         "common.rkt"
         "notify.rkt")

(define SUMMARY-URL (string-append "https://pkg-build.racket-lang.org/" SUMMARY-NAME))
(define SUMMARY-ETAG-PATH (build-path cache-path (format "~a.etag" SUMMARY-NAME)))

(define (build-update!)
  (define cur-version
    (if (file-exists? SUMMARY-ETAG-PATH)
        (file->bytes SUMMARY-ETAG-PATH)
        #""))
  (log! "build-update: Current: ~v\n" cur-version)

  (define head (http:head SUMMARY-URL #:max-redirects 0))
  (define head-version (or (http:response-headers-ref head 'etag) #""))
  (log! "build-update: Head: ~v\n" head-version)

  (unless (bytes=? cur-version head-version)
    (define response (http:get SUMMARY-URL #:max-redirects 0))
    (define get-version (or (http:response-headers-ref response 'etag) #""))
    (log! "build-update: Get: ~v\n" get-version)
    (call-with-atomic-output-file
     SUMMARY-PATH
     (lambda (out _)
       (write-bytes (http:response-body response) out)))
    (call-with-atomic-output-file
     SUMMARY-ETAG-PATH
     (lambda (out _)
       (write-bytes get-version out)))))

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
