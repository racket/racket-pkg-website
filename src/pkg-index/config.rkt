#lang racket/base

(provide config
         get-config)

(define config (make-parameter (hash)))

(define-syntax get-config
  (syntax-rules ()
    [(_ key) (hash-ref (config) 'key #f)]
    [(_ key default) (hash-ref (config) 'key (lambda () default))]))
