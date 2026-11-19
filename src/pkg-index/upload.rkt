#lang racket/base
(require net/url
         (prefix-in http: net/http-easy)
         racket/port
         "../http-post.rkt")

(define (upload! the-email the-password the-url the-post)
  (displayln the-email)
  (displayln the-post)
  (define data
    (with-output-to-bytes
      (lambda ()
        (write (list the-email (string->bytes/utf-8 the-password) the-post)))))
  (define response (post-once the-url data))
  (define v (bytes->string/utf-8 (http:response-body response) #\uFFFD))
  (displayln v)
  v)

(module+ main
  (require racket/cmdline)
  (command-line #:program "upload"
                #:args (email password [urlstr "https://pkgd.racket-lang.org/api/upload"])
                (if (upload! email
                             password
                             (string->url urlstr)
                             (read (current-input-port)))
                  (exit 0)
                  (exit 1))))
