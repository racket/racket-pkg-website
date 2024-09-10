#lang racket/base

(provide main)

(require "config.rkt")

(require racket/runtime-path)
(define-runtime-path here ".")

(define (main [configuration (hash)])
  (config configuration)
  ((dynamic-require (build-path here "dynamic.rkt") 'go)))

(module+ main
  (require racket/cmdline)
  (define configuration (hash))
  (command-line
   #:once-each
   ["--config" file "Require `config` from module <file>"
               (set! configuration
                     (dynamic-require `(file ,file) 'config))]
   #:args ()
   (void))
  (main configuration))
