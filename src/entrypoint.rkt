#lang racket/base

(provide start-service)

(require web-server/servlet-env)
(require "signals.rkt")

(define (start-service #:port [port 8443]
                       #:ssl? [ssl? #t]
		       request-handler-function)
  (start-restart-signal-watcher)
  (serve/servlet request-handler-function
		 #:launch-browser? #f
		 #:quit? #f
		 #:listen-ip #f
		 #:port port
		 #:extra-files-paths (list (build-path (current-directory)
						       "../static"))
                 #:ssl? ssl?
                 #:ssl-cert (and ssl? (build-path (current-directory) "../server-cert.pem"))
                 #:ssl-key (and ssl? (build-path (current-directory) "../private-key.pem"))
		 #:servlet-regexp #rx""))
