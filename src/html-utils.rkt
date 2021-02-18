#lang racket/base
;; A utilities module :-/

(provide maybe-splice
         define-form-bindings/xform
	 define-form-bindings
         define-form-bindings/trim)

(require web-server/servlet)
(require (only-in racket/string string-trim))

;; Boolean XExpr ... -> (Listof XExpr)
;; Useful for optionally splicing in some contents to a list.
;; If the guard is true, returns the contents; otherwise returns the empty list.
(define-syntax-rule (maybe-splice guard contents ...)
  (if guard (list contents ...) '()))

(define-syntax define-form-bindings*
  (syntax-rules ()
    [(_ bs xform ())
     (begin)]
    [(_ bs xform ([name fieldname defaultval] rest ...))
     (begin (define name (if (exists-binding? 'fieldname bs)
                             (xform (extract-binding/single 'fieldname bs))
                             defaultval))
            (define-form-bindings* bs xform (rest ...)))]
    [(_ bs xform ([name defaultval] rest ...))
     (define-form-bindings* bs xform ([name name defaultval] rest ...))]
    [(_ bs xform (name rest ...))
     (define-form-bindings* bs xform ([name #f] rest ...))]))

;; Extracts named single-valued bindings from the given request.
;; If a given binding is missing, the extracted value will be #f.
(define-syntax-rule (define-form-bindings/xform req xform (specs ...))
  (begin (define bs (request-bindings req))
         (define-form-bindings* bs xform (specs ...))))

(define-syntax-rule (define-form-bindings req (specs ...))
  (define-form-bindings/xform req values (specs ...)))

(define-syntax-rule (define-form-bindings/trim req (specs ...))
  (define-form-bindings/xform req string-trim (specs ...)))
