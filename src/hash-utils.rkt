#lang racket/base

(provide @
         @ref)

(define-syntax @
  (syntax-rules ()
    [(_ v) v]
    [(_ v k rest ...) (@ (@ref v 'k) rest ...)]))

(define (@ref v k)
  (and v (hash-ref v k (lambda () #f))))
