#lang racket/base

(provide start-service)

(require web-server/servlet-env)
(require web-server/managers/lru)
(require web-server/http/request-structs)
(require net/url)
(require "signals.rkt")
(require "bootstrap.rkt")

(define (strip-parameters u)
  (struct-copy url u
               [path (map (lambda (element)
                            (struct-copy path/param element
                                         [param '()]))
                          (url-path u))]))

(define (default-expiry-handler request)
  (bootstrap-redirect (url->string (strip-parameters (request-uri request)))))

(define (start-service #:port [port 8443]
                       #:ssl? [ssl? #t]
                       #:on-continuation-expiry [on-continuation-expiry default-expiry-handler]
		       request-handler-function)
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
