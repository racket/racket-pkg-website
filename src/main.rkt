#lang racket/base
(require racket/runtime-path)
;; Outer startup module - delegates to main-inner.rkt after installing a custodian

(provide main
         outermost-custodian)

(define *outermost-custodian* (current-custodian))
(define (outermost-custodian) *outermost-custodian*)

(define-runtime-module-path-index main-inner "main-inner.rkt")

(define (main [config (hash)])
  (parameterize ((current-custodian (make-custodian (outermost-custodian))))
    ((dynamic-require main-inner 'main) config)))

(module+ main
  (require "command-line.rkt")
  (handle-command-line main))
