#lang racket/base
(require racket/cmdline)

(provide handle-command-line)

(define (handle-command-line main)
  (define configuration (hash))
  (command-line
   #:once-each
   ["--config" file "Require `config` from module <file>"
               (set! configuration
                     (dynamic-require `(file ,file) 'config))]
   #:args ()
   (void))
  (main configuration))
