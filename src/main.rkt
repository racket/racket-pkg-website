#lang racket/base

(module+ main
  (require "entrypoint.rkt")
  (require "signals.rkt")
  (start-restart-signal-watcher)
  ;; (start-reloadable-service "site.rkt"
  ;;                           'request-handler
  ;;                           'on-continuation-expiry)
  (require "site.rkt")
  (start-service request-handler on-continuation-expiry)
  )
