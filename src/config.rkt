#lang racket/base

(provide config)

(require reloadable)

(define *config* (make-persistent-state '*config* (lambda () config)))

(define (config) (*config*))
