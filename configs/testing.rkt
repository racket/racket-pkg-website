#lang racket/base
;; Configuration for development setup.
(require "../src/main.rkt"
         "../src/command-line.rkt")

;; All defaults should be suitable for testing
(handle-command-line main)
