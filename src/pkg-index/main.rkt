#lang racket/base

(provide main)

(require "config.rkt")

(require racket/runtime-path)
(define-runtime-path here ".")

(define (main [configuration (hash)])
  (config configuration)
  ((dynamic-require (build-path here "dynamic.rkt") 'go)))

(module+ main
  (require "../command-line.rkt")
  (handle-command-line main))
