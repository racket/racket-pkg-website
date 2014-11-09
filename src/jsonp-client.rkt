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
                    site-relative-url
                    original-parameters)
  (define s (current-session))
  (if sensitive?
      (log-info "jsonp-rpc: sensitive request ~a" site-relative-url)
      (log-info "jsonp-rpc: request ~a params ~a~a"
                site-relative-url
                original-parameters
                (if include-credentials?
                    (if s
                        " +creds"
                        " +creds(missing)")
                    "")))
  (define stamp (~a (inexact->exact (truncate (current-inexact-milliseconds)))))
  (define callback-label (format "callback~a" stamp))
  (define extraction-expr (format "^callback~a\\((.*)\\);$" stamp))
  (let* ((parameters original-parameters)
         (parameters (if (and include-credentials? s)
                         (append (list (cons 'email (session-email s))
                                       (cons 'passwd (session-password s)))
                                 parameters)
                         parameters))
         (parameters (cons (cons 'callback callback-label) parameters)))
    (define request-url
      (string->url
       (format "~a~a?~a"
               (or (jsonp-baseurl) (error 'jsonp-rpc! "jsonp-baseurl is not set"))
               site-relative-url
               (alist->form-urlencoded parameters))))
    (define-values (body-port response-headers) (get-pure-port/headers request-url))
    (define raw-response (port->string body-port))
    (match-define (pregexp extraction-expr (list _ json)) raw-response)
    (define reply (string->jsexpr json))
    (unless sensitive? (log-info "jsonp-rpc: reply ~a" reply))
    reply))
