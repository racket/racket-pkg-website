#lang racket/base

(provide config
         config-path
         var-path)

(require reloadable)
(require racket/runtime-path)
(require "hash-utils.rkt")

(define *config* (make-persistent-state '*config* (lambda () (hash))))

(define (config) (*config*))

(define-runtime-path here ".")
(define (config-path str)
  (define p (if (relative-path? str)
                (build-path here str)
                str))
  (if (path? p) (path->string p) p))

(define (var-path)
  (config-path (or (@ (config) var-path)
                   "../var")))
