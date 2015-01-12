#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")
(main (hash 'port 8444
            'reloadable? #t
            ))
