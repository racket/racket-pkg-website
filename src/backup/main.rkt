#lang racket/base
(require racket/runtime-path
         racket/date
         racket/file
         file/zip
         s3-sync
         plt-service-monitor/beat
         "../config.rkt"
         "../default.rkt"
         "../hash-utils.rkt")

(provide go)

;; files/directories to back up:
(define backups
  (list "pkgs"
        "users.new"))

(define-runtime-path main.rkt "main.rkt")

(define (go [config (config)])
  (define root (or (@ config root) default-root))
  (define backup-stamp (build-path root "backup-stamp"))
  (define s3-bucket (@ config s3-bucket))
  (define beat-s3-bucket (@ config beat-s3-bucket))
  (when s3-bucket
    (let loop ()
      (when (> (current-seconds)
               (+ (file-or-directory-modify-seconds backup-stamp #f (lambda () 0))
                  (or (@ config backup-seconds) default-backup-seconds)))
        (thread-wait
         (thread (lambda ()
                   (define zip-dir (make-temporary-directory))
                   (define now (seconds->date (current-seconds)))
                   (define (pad n)
                     (define s (format "~a" n))
                     (string-append (make-string (- 2 (string-length s)) #\0) s))
                   (define fn (format "backup-~a~a~a~a~a.zip"
                                      (date-year now)
                                      (pad (date-month now))
                                      (pad (date-day now))
                                      (pad (date-hour now))
                                      (pad (date-minute now))))
                   (printf "Generating backup ~s\n" fn)
                   (define zip-path (path->complete-path (build-path zip-dir fn)))
                   (parameterize ([current-directory root])
                     (apply zip zip-path backups))
                   (s3-sync zip-path
                            s3-bucket
                            fn)
                   (delete-directory/files zip-dir)
                   (call-with-output-file backup-stamp
                     #:exists 'truncate
                     (lambda (o)
                       (println (current-seconds) o)))
                   (when beat-s3-bucket
                     (beat beat-s3-bucket (or (@ config beat-backup-task-name) "pkgd-backup")))
                   (printf "Completed backup ~s\n" fn)))))
      (sleep (* 60 60))
      (loop))))

(define (main [configuration (hash)])
  ((dynamic-require main.rkt 'go) configuration))

(module+ main
  (require "../command-line.rkt")
  (handle-command-line main))
