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
(define-syntax-rule (define-form-bindings req (specs ...))
  (begin (define bs (request-bindings req))
         (define-form-bindings* bs (specs ...))))

(define-syntax define-form-bindings*
  (syntax-rules ()
    [(_ bs ())
     (begin)]
    [(_ bs ([name fieldname defaultval] rest ...))
     (begin (define name (if (exists-binding? 'fieldname bs)
                             (extract-binding/single 'fieldname bs)
                             defaultval))
            (define-form-bindings* bs (rest ...)))]
    [(_ bs ([name defaultval] rest ...))
     (define-form-bindings* bs ([name name defaultval] rest ...))]
    [(_ bs (name rest ...))
     (define-form-bindings* bs ([name #f] rest ...))]))
