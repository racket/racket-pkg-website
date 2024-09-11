#lang racket/base
(require plt-service-monitor/beat
         "hash-utils.rkt"
         "config.rkt")

(provide heartbeat)

(define beat-s3-bucket (@ (config) beat-s3-bucket))

(define (heartbeat task)
  (when beat-s3-bucket
    (beat beat-s3-bucket task)))
