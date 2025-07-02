#lang racket/base

;; shared default configurations (suitable for test mode)

(provide default-port
         default-pkg-index-port
         default-ssl?
         default-root
         default-static-gen
         default-htdocs-gen
         default-s3-mock
         default-users
         default-email-transport
         default-smtp-server
         default-smtp-port
         default-smtp-sending-server
         default-smtp-user+password-file
         default-backup-seconds
         extract-pkg-index-config
         extract-backup-config)

(define default-port (or (let ((port-str (getenv "SITE_PORT")))
                           (and port-str (string->number port-str)))
                         7443))
(define default-pkg-index-port (or (let ((port-str (getenv "SITE_PKG_INDEX_PORT")))
                                     (and port-str (string->number port-str)))
                                   9004))

(define default-ssl? (not (getenv "PKG_SERVER_HTTP")))

;; and put generated files (especially) in "compiled" to make it a
;; reliably excluded directory
(define (generated dir)
  (build-path (current-directory) "compiled" dir))

(define default-root (generated "root"))
(define default-static-gen (generated "static-gen"))
(define default-htdocs-gen (generated "generated-htdocs"))

(define default-s3-mock (generated "mock/aws-s3"))

(define (default-users root) (build-path root "users.new"))

(define default-email-transport 'smtp)
(define default-smtp-server "smtp-relay.gmail.com")
(define default-smtp-port 465)  
(define default-smtp-sending-server "racket-lang.org")
(define default-smtp-user+password-file (build-path (find-system-path 'home-dir) ".email_key"))

(define default-backup-seconds (* 60 60 24 3)) ; 3 days

;; extract 'pkg-index config table, propagating some keys from the
;; enclosing table to other keys in the extracted table
(define (extract-pkg-index-config config)
  (let ([pi-config (hash-ref config 'pkg-index (hash))]
        [root (hash-ref config 'root default-root)]
        [port (hash-ref config 'pkg-index-port default-pkg-index-port)]
        [ssl? (hash-ref config 'ssl? default-ssl?)]
        [static-path (hash-ref config 'pkg-index-generated-directory default-static-gen)]
        [beat-s3-bucket (hash-ref config 'beat-s3-bucket #f)])
    (define (hash-default pi-config key val)
      (if (hash-ref pi-config key #f)
          pi-config
          (hash-set pi-config key val)))
    (let* ([pi-config (hash-default pi-config 'root root)]
           [pi-config (hash-default pi-config 'users.new-path
                                    (hash-ref config 'user-directory (default-users root)))]
           [pi-config (hash-default pi-config 'port port)]
           [pi-config (hash-default pi-config 'ssl? ssl?)]
           [pi-config (hash-default pi-config 'static-path static-path)]
           [pi-config (hash-default pi-config 'beat-s3-bucket beat-s3-bucket)])
      pi-config)))

;; extract 'backup config table, propagating some keys
(define (extract-backup-config config)
  (let ([bu-config (hash-ref config 'backup (hash))]
        [root (hash-ref config 'root default-root)]
        [beat-s3-bucket (hash-ref config 'beat-s3-bucket #f)])
    (define (hash-default bu-config key val)
      (if (hash-ref bu-config key #f)
          bu-config
          (hash-set bu-config key val)))
    (let* ([bu-config (hash-default bu-config 'root root)]
           [bu-config (hash-default bu-config 'beat-s3-bucket beat-s3-bucket)])
      bu-config)))
