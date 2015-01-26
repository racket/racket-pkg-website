#lang racket/base

(provide main)

(require reloadable)
(require "entrypoint.rkt")

(define (main [config (hash)])
  (make-persistent-state '*config* (lambda () config))
  (void (make-reloadable-entry-point 'refresh-packages! "packages.rkt"))
  (void (make-reloadable-entry-point 'rerender-all! "site.rkt"))
  (start-service #:port (hash-ref config 'port (lambda ()
                                                 (let ((port-str (getenv "SITE_PORT")))
                                                   (if port-str (string->number port-str) 8443))))
                 #:ssl? (hash-ref config 'ssl? (lambda () #t))
                 #:reloadable? (hash-ref config 'reloadable? (lambda () (getenv "SITE_RELOADABLE")))
                 (make-reloadable-entry-point 'request-handler "site.rkt")
                 (make-reloadable-entry-point 'on-continuation-expiry "site.rkt")
                 (make-reloadable-entry-point 'extra-files-paths "static.rkt")))
