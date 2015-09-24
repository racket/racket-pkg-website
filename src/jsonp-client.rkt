#lang racket/base

(provide jsonp-baseurl
         jsonp-rpc!)

(require racket/match)
(require racket/format)
(require racket/port)
(require net/url)
(require net/uri-codec)
(require json)
(require "sessions.rkt")

(define jsonp-baseurl (make-parameter #f))

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
  (define (add-param ps name val) (cons (cons name val) ps))
  (let* ((parameters original-parameters)
         (parameters (if (and include-credentials? s)
                         (add-param (add-param parameters 'email (session-email s))
                                    'passwd (session-password s))
                         parameters))
         (parameters (add-param parameters 'callback callback-label)))
    (define baseurl (or (jsonp-baseurl) (error 'jsonp-rpc! "jsonp-baseurl is not set")))
    (define request-url (string->url (format "~a~a?~a"
                                             baseurl
                                             site-relative-url
                                             (alist->form-urlencoded parameters))))
    (define-values (body-port response-headers)
      (if post-data
          (values (post-pure-port request-url post-data)
                  'unknown-response-headers-because-post-pure-port-doesnt-return-them)
          (get-pure-port/headers request-url)))
    (define raw-response (port->string body-port))
    (match-define (pregexp extraction-expr (list _ json)) raw-response)
    (define reply (string->jsexpr json))
    (unless sensitive? (log-info "jsonp-rpc: reply ~a" reply))
    reply))
