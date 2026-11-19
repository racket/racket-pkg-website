#lang racket/base
(require net/url
         (prefix-in http: net/http-easy)
         json
         "../http-post.rkt")

(define (upload! the-email the-password the-url the-post)
  (displayln the-email)
  (println the-post)
  (define response
    (post-once the-url (jsexpr->bytes the-post)
               #:auth (http:basic-auth the-email the-password)))
  (define v (bytes->string/utf-8 (http:response-body response) #\uFFFD))
  (displayln v)
  v)

(module+ main
  (require racket/cmdline)
  (command-line #:program "upload"
                #:args (email password [urlstr "https://pkgd.racket-lang.org/api/package/modify-all"])
                (if (upload! email
                             password
                             (string->url urlstr)
                             (read (current-input-port)))
                  (exit 0)
                  (exit 1))))
