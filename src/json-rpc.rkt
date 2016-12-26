#lang racket/base
;; Trivially simple authenticated JSON-over-HTTPS RPC.

(provide simple-json-rpc!)

(require racket/port)
(require net/url)
(require net/base64)
(require json)
(require "sessions.rkt")

(define (make-basic-auth-credentials-header username password)
  (define token
    (base64-encode (string->bytes/utf-8 (string-append username ":" password)) #""))
  (string-append "Authorization: Basic " (bytes->string/utf-8 token)))

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
  (define request-urls (format "~a~a" baseurl site-relative-url))
  (define request-url (string->url request-urls))
  (define post-data (string->bytes/utf-8 (jsexpr->string jsexpr-to-send)))
  (define req-headers
    (if include-credentials?
        (list (make-basic-auth-credentials-header (session-email s)
                                                  (session-password s)))
        '()))
  (define response-port (post-pure-port request-url post-data req-headers))
  (define raw-response (port->string response-port))
  (close-input-port response-port)
  (define reply (string->jsexpr raw-response))
  (unless sensitive? (log-info "simple-json-rpc: reply ~v" reply))
  reply)
