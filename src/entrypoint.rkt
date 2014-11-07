#lang racket/base

(provide start-service)

(require web-server/servlet-env)
(require "signals.rkt")

(define (start-service #:port [port 8000]
		       request-handler-function)
  (start-restart-signal-watcher)
  (serve/servlet request-handler-function
		 #:launch-browser? #f
		 #:quit? #f
		 #:listen-ip #f
		 #:port port
		 #:extra-files-paths (list (build-path (current-directory)
						       "../static"))
		 #:servlet-regexp #rx""))
