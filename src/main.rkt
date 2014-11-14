#lang racket/base

(module+ main
  (require "entrypoint.rkt")
  (void (make-reloadable-entry-point 'refresh-packages! "packages.rkt"))
  (void (make-reloadable-entry-point 'rerender-all! "site.rkt"))
  (start-service #:reloadable? (getenv "SITE_RELOADABLE")
                 #:port (let ((port-str (getenv "SITE_PORT")))
                          (if port-str (string->number port-str) 8443))
                 (make-reloadable-entry-point 'request-handler "site.rkt")
                 (make-reloadable-entry-point 'on-continuation-expiry "site.rkt")))
