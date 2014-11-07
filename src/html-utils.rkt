#lang racket/base
;; A utilities module :-/

(require web-server/servlet)

(provide maybe-splice
	 define-form-bindings)

;; Boolean XExpr ... -> (Listof XExpr)
;; Useful for optionally splicing in some contents to a list.
;; If the guard is true, returns the contents; otherwise returns the empty list.
(define (maybe-splice guard . contents)
  (if guard contents '()))

;; Extracts named single-valued bindings from the given request.
;; If a given binding is missing, the extracted value will be #f.
(define-syntax-rule (define-form-bindings req (name ...))
  (begin (define bs (request-bindings req))
	 (define name (and (exists-binding? 'name bs) (extract-binding/single 'name bs)))
	 ...))
