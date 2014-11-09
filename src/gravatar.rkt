#lang racket/base

(provide gravatar-hash
         gravatar-image-url)

(require file/md5)
(require racket/string)

(define (gravatar-hash email)
  (bytes->string/utf-8
   (md5
    (string-downcase
     (string-trim email)))))

(define (gravatar-image-url email [size 80] #:extension [extension ""] #:default [default "identicon"])
  (format "http://www.gravatar.com/avatar/~a~a?s=~a&d=~a"
          (gravatar-hash email)
          extension
          size
          default))
