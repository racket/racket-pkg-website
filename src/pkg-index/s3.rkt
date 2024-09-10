#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         file/gzip
         racket/match
         aws/s3
         s3-sync
         "common.rkt"
         "notify.rkt")

(define (upload-all)
  (log! "upload: doing gzip")
  (gzip (format "~a/pkgs-all.json" static-path)
        (format "~a/pkgs-all.json.gz" static-path))

  (delete-file (format "~a/pkgs-all.json" static-path))

  (if (eq? s3-bucket #f)
      (log! "upload: S3 synchronization is disabled by s3-bucket = #f in config")
      (begin
        (notify! "update upload in progress: there may be inconsistencies below")
        (log! "upload: uploading everything")
        (parameterize ([s3-region s3-bucket-region])
          (s3-sync static-path
                   s3-bucket
                   #f
                   #:jobs 32
                   #:upload? #t
                   #:delete? #t
                   #:acl "public-read"
                   #:log (lambda (str) (log! "~a" str))
                   #:upload-metadata-mapping
                   (hash "pkgs-all.json.gz"
                         (hash 'Content-Type "application/javascript"
                               'Content-Encoding "gzip"))))
        (log! "upload: done with upload")
        (notify! "")))

  (void))

(define (upload-pkgs pkgs)
  ;; XXX make this more efficient
  (upload-all))
(define (run-s3! pkgs)
  (run! upload-pkgs pkgs))
(define run-sema (make-semaphore 1))
(define (signal-s3! pkgs)
  (safe-run! run-sema (λ () (run-s3! pkgs))))

(provide upload-pkgs
         signal-s3!)

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "s3"
   #:args pkgs
   (cond
     [(empty? pkgs)
      (upload-all)]
     [else
      (upload-pkgs pkgs)])))
