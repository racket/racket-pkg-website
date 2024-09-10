#lang racket/base
(require racket/list
         net/url
         racket/port
         net/base64
         json)

(define (upload! the-email the-password the-url the-post)
  (displayln the-email)
  (println the-post)
  (define bs
    (call/input-url the-url
                    (λ (url)
                      (post-pure-port the-url
                                      (jsexpr->bytes the-post)
                                      ;; Adding an `Authorization` header lets this
                                      ;; tool be used for "/api/package/modify-all", too
                                      (list (string-append
                                             "Authorization: Basic "
                                             (bytes->string/utf-8
                                              (base64-encode (string->bytes/utf-8
                                                              (string-append
                                                               the-email
                                                               ":"
                                                               the-password))
                                                             #""))))))
                    (lambda (p) (begin0 (port->bytes p) (close-input-port p)))))
  (define v (port->string (open-input-bytes bs)))
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
