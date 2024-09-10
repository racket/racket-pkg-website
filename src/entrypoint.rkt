#lang racket/base

(provide (struct-out reloadable-entry-point) ;; from reloadable
         make-reloadable-entry-point ;; from reloadable
         start-service)

(require web-server/servlet-env)
(require web-server/managers/lru)
(require reloadable)
(require "signals.rkt")
(require "daemon.rkt")
(require "default.rkt")

(define (start-service* #:port [port 7443]
                        #:ssl? [ssl? default-ssl?]
                        #:root [root default-root]
                        request-handler-function
                        on-continuation-expiry
                        extra-files-paths)
  (start-restart-signal-watcher)
  ((daemonize-thunk
    'main-web-server-thread
    (lambda ()
      (serve/servlet request-handler-function
                     #:launch-browser? #f
                     #:quit? #f
                     #:listen-ip #f
                     #:port port
                     #:manager (make-threshold-LRU-manager
                                on-continuation-expiry
                                ;; The production service averages 200 - 350 MiB.
                                ;; Using 512 MiB ensures e.g. forms live for a while without creating
                                ;; too much memory pressure.
                                ;; TODO: Make this configurable...?
                                (* 512 1024 1024))
                     #:extra-files-paths (extra-files-paths)
                     #:ssl? ssl?
                     #:ssl-cert (and ssl? (build-path root "server-cert.pem"))
                     #:ssl-key (and ssl? (build-path root "private-key.pem"))
                     #:servlet-regexp #rx"")))))

(define (start-service #:port [port default-port]
                       #:ssl? [ssl? default-ssl?]
                       #:root [root default-root]
                       #:reloadable? [reloadable? #t]
                       request-handler-entry-point
                       on-continuation-expiry-entry-point
                       extra-files-paths-entry-point)
  (when (not reloadable?)
    (set-reload-poll-interval! #f))
  (reload!)
  (start-service* #:port port
                  #:ssl? ssl?
                  #:root root
                  (reloadable-entry-point->procedure request-handler-entry-point)
                  (reloadable-entry-point->procedure on-continuation-expiry-entry-point)
                  (reloadable-entry-point->procedure extra-files-paths-entry-point)))
