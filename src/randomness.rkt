#lang racket/base

(provide random-bytes
         random-bytes/base64)

(require net/base64)

(define (random-bytes n)
  (with-input-from-file "/dev/urandom"
    (lambda ()
      (read-bytes n))))

(define (random-bytes/base64 n)
  (base64-encode (random-bytes n) #""))
