#lang racket/base
;; Outer startup module - delegates to main-inner.rkt after installing a custodian

(provide main
         outermost-custodian)

(define *outermost-custodian* (current-custodian))
(define (outermost-custodian) *outermost-custodian*)

(define (main [config (hash)])
  (parameterize ((current-custodian (make-custodian (outermost-custodian))))
    ((dynamic-require "main-inner.rkt" 'main) config)))
