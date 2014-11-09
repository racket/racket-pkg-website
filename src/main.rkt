#lang racket/base

(module+ main
  (require "entrypoint.rkt")
  (void (make-entry-point 'refresh-packages! "packages.rkt"))
  (start-service #:reloadable? (getenv "SITE_RELOADABLE")
                 (make-entry-point 'request-handler "site.rkt")
                 (make-entry-point 'on-continuation-expiry "site.rkt")))
