#lang racket/base

(provide config
         config-path)

(require reloadable)
(require racket/runtime-path)
(require "hash-utils.rkt")
(require "default.rkt")

(define *config* (make-persistent-state '*config* (lambda () (hash))))

(define (config) (*config*))

(define (config-path str)
  (unless (path-string? str)
    (error 'config-path "Not given path string: ~e" str))
  (define p (if (relative-path? str)
                (path->complete-path str)
                str))
  (if (path? p) (path->string p) p))
