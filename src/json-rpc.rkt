#lang racket/base
;; Trivially simple authenticated JSON-over-HTTPS RPC.

(provide simple-json-rpc!)

(require (prefix-in http: net/http-easy)
         json
         "http-post.rkt"
         "sessions.rkt")

(define (simple-json-rpc! #:sensitive? [sensitive? #f]
                          #:include-credentials? [include-credentials? #t]
                          baseurl
                          site-relative-url
                          jsexpr-to-send)
  (define s (current-session))
  (if sensitive?
      (log-info "simple-json-rpc: sensitive request ~v" site-relative-url)
      (log-info "simple-json-rpc: request ~v params ~v~a"
                site-relative-url
                jsexpr-to-send
                (if include-credentials?
                    (if s
                        " +creds"
                        " +creds(missing)")
                    "")))
  (define request-url (format "~a~a" baseurl site-relative-url))
  (define response
    (post-once request-url (jsexpr->bytes jsexpr-to-send)
               #:auth (and include-credentials?
                           (http:basic-auth (session-email s) (session-password s)))))
  (define reply (http:response-json response))
  (unless sensitive? (log-info "simple-json-rpc: reply ~v" reply))
  reply)
