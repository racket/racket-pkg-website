#lang racket/base

(require web-server/servlet)
(require "bootstrap.rkt")

(define-values (request-handler named-url)
  (dispatch-rules
   [("") main-page]
   ))

(module+ main
  (require "entrypoint.rkt")
  (start-service request-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-page request)
  (parameterize ((bootstrap-active-navigation "Home"))
    (bootstrap-response "Hello World"
			`(p "Hi there!"))))

