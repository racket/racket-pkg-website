#lang racket/base
(require racket/list
         net/url
         racket/port)

(define (upload! the-email the-password the-url the-post)
  (displayln the-email)
  (displayln the-post)
  (define bs
    (call/input-url the-url
                    (λ (url)
                      (post-pure-port the-url
                                      (with-output-to-bytes
                                       (λ ()
                                         (write (list the-email
                                                      (string->bytes/utf-8 the-password)
                                                      the-post))))))
                    (lambda (p) (begin0 (port->bytes p) (close-input-port p)))))
  (define v (port->string (open-input-bytes bs)))
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
