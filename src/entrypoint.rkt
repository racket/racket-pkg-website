#lang racket/base

(provide (struct-out entry-point) ;; from reload.rkt
         make-entry-point ;; from reload.rkt
         start-service)

(require web-server/servlet-env)
(require web-server/managers/lru)
(require "signals.rkt")
(require "reload.rkt")

(define (start-service* #:port [port 8443]
                        #:ssl? [ssl? #t]
                        request-handler-function
                        on-continuation-expiry)
  (start-restart-signal-watcher)
  (serve/servlet request-handler-function
		 #:launch-browser? #f
		 #:quit? #f
		 #:listen-ip #f
		 #:port port
                 #:manager (make-threshold-LRU-manager
                            on-continuation-expiry
                            ;; This value is copied from web-server/servlet-env.rkt:
                            (* 128 1024 1024))
		 #:extra-files-paths (list (build-path (current-directory)
						       "../static"))
                 #:ssl? ssl?
                 #:ssl-cert (and ssl? (build-path (current-directory) "../server-cert.pem"))
                 #:ssl-key (and ssl? (build-path (current-directory) "../private-key.pem"))
		 #:servlet-regexp #rx""))

(define (start-service #:port [port 8443]
                       #:ssl? [ssl? #t]
                       #:reloadable? [reloadable? #t]
                       request-handler-entry-point
                       on-continuation-expiry-entry-point)
  (when (not reloadable?)
    (set-reload-poll-interval! #f))
  (reload!)
  (start-service* #:port port
                  #:ssl? ssl?
                  (lambda (req) ((entry-point-value request-handler-entry-point) req))
                  (lambda (req) ((entry-point-value on-continuation-expiry-entry-point) req))))
