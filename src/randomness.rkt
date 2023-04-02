#lang racket/base

(provide random-bytes
         random-bytes/base64)

(require (rename-in racket/random
                    [crypto-random-bytes random-bytes])
         net/base64)

(define (random-bytes/base64 n)
  (base64-encode (random-bytes n) #""))
