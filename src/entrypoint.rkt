#lang racket/base

(provide start-service)

(require web-server/servlet-env)
(require web-server/managers/lru)

(define (start-service #:port [port 8443]
                       #:ssl? [ssl? #t]
		       request-handler-function
                       on-continuation-expiry)
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
