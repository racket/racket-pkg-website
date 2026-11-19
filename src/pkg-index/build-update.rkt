#lang racket/base
(require racket/list
         racket/file
         (prefix-in http: net/http-easy)
         "common.rkt"
         "notify.rkt")

(define SUMMARY-URL (string-append "https://pkg-build.racket-lang.org/" SUMMARY-NAME))
(define SUMMARY-ETAG-PATH (build-path cache-path (format "~a.etag" SUMMARY-NAME)))

(define (build-update!)
  (define headers
    (if (and (file-exists? SUMMARY-PATH) (file-exists? SUMMARY-ETAG-PATH))
        (hasheq 'if-none-match (file->bytes SUMMARY-ETAG-PATH))
        (hasheq)))
  (define response (http:get SUMMARY-URL #:headers headers #:max-redirects 0))
  (define status (http:response-status-code response))
  (unless (= status 304)
    (unless (<= 200 status 299)
      (error 'build-update! "unexpected HTTP status ~a for ~a" status SUMMARY-URL))
    (call-with-atomic-output-file
     SUMMARY-PATH
     (lambda (out _)
       (write-bytes (http:response-body response) out)))
    (call-with-atomic-output-file
     SUMMARY-ETAG-PATH
     (lambda (out _)
       (write-bytes (or (http:response-headers-ref response 'etag) #"") out)))))

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
