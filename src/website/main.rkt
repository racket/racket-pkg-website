#lang racket/base
(require (prefix-in inner: "../main-inner.rkt"))

(provide main)

(define (main [config (hash)])
  (inner:main (hash-set config 'pkg-index #f)))

(module+ main
  (require "../command-line.rkt")
  (handle-command-line main))
