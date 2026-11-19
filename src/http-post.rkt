#lang racket/base

(require (prefix-in http: net/http-easy))

(provide post-once)

;; A failed response can come after the server has already applied a mutation.
(define (disable-retries url continue
                         #:method method #:headers headers #:params params
                         #:auth auth #:data data #:history history
                         #:attempts _attempts #:redirects redirects)
  (continue url #:method method #:headers headers #:params params
            #:auth auth #:data data #:history history
            #:attempts 0 #:redirects redirects))

(define session (http:make-session #:middleware disable-retries))

(define (post-once url data #:auth [auth #f])
  (http:session-request session url #:method 'post #:data data #:auth auth
                        #:max-redirects 0))
