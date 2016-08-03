#lang racket/base

(provide jsonp-baseurl
         jsonp-rpc!
         simple-json-rpc!)

(require racket/match)
(require racket/format)
(require racket/port)
(require net/url)
(require net/uri-codec)
(require net/base64)
(require json)
(require "sessions.rkt")

(define jsonp-baseurl (make-parameter #f))

(define (make-basic-auth-credentials-header username password)
  (define token
    (base64-encode (string->bytes/utf-8 (string-append username ":" password)) #""))
  (string-append "Authorization: Basic " (bytes->string/utf-8 token)))

(define (jsonp-rpc! #:sensitive? [sensitive? #f]
                    #:include-credentials? [include-credentials? #t]
                    #:post-data [post-data #f]
                    site-relative-url
                    original-parameters)
  (define s (current-session))
  (if sensitive?
      (log-info "jsonp-rpc: sensitive request ~a" site-relative-url)
      (log-info "jsonp-rpc: request ~a params ~a~a~a"
                site-relative-url
                original-parameters
                (if post-data
                    (format " post-data: ~v" post-data)
                    "")
                (if include-credentials?
                    (if s
                        " +creds"
                        " +creds(missing)")
                    "")))
  (define stamp (~a (inexact->exact (truncate (current-inexact-milliseconds)))))
  (define callback-label (format "callback~a" stamp))
  (define extraction-expr (format "^callback~a\\((.*)\\);$" stamp))
  (define parameters (cons (cons 'callback callback-label) original-parameters))
  (define baseurl (or (jsonp-baseurl) (error 'jsonp-rpc! "jsonp-baseurl is not set")))
  (define request-url
    (string->url
     (format "~a~a?~a"
             baseurl
             site-relative-url
             (alist->form-urlencoded parameters))))
  (define req-headers
    (if include-credentials?
        (list
         (make-basic-auth-credentials-header (session-email s)
                                             (session-password s)))
        null))
  (define-values (body-port response-headers)
    (if post-data
        (values (post-pure-port request-url
                                post-data
                                req-headers)
                'unknown-response-headers-because-post-pure-port-doesnt-return-them)
        (get-pure-port/headers request-url
                               req-headers)))
  (define raw-response (port->string body-port))
  (match-define (pregexp extraction-expr (list _ json)) raw-response)
  (define reply (string->jsexpr json))
  (unless sensitive? (log-info "jsonp-rpc: reply ~a" reply))
  reply)

(define (simple-json-rpc! #:sensitive? [sensitive? #f]
                          #:include-credentials? [include-credentials? #t]
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
  (define baseurl
    (or (jsonp-baseurl) (error 'simple-json-rpc! "jsonp-baseurl is not set")))
  (define request-urls (format "~a~a" baseurl site-relative-url))
  (define request-url (string->url request-urls))
  (define post-data (string->bytes/utf-8 (jsexpr->string jsexpr-to-send)))
  (define req-headers
    (if include-credentials?
        (list (make-basic-auth-credentials-header (session-email s)
                                                  (session-password s)))
        '()))
  (define raw-response (port->string (post-pure-port request-url post-data req-headers)))
  (define reply (string->jsexpr raw-response))
  (unless sensitive? (log-info "simple-json-rpc: reply ~v" reply))
  reply)
