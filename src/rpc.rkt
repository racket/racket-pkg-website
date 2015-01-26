#lang racket/base

(provide rpc-request-evt
         rpc-handler
         rpc-call
         rpc-cast!)

(require racket/match)

(define (rpc-request-evt)
  (handle-evt (thread-receive-evt)
              (lambda (_) (thread-receive))))

(define-syntax-rule (rpc-handler ch-and-req [(argpat ...) body ...] ...)
  (match ch-and-req
    [(cons ch request)
     (define-values (reply-value new-state)
       (match request
         [(list argpat ...) body ...]
         ...))
     (when ch (channel-put ch reply-value))
     new-state]))

(define (rpc-call thread . request)
  (define ch (make-channel))
  (thread-send thread (cons ch request))
  (channel-get ch))

(define (rpc-cast! thread . request)
  (thread-send thread (cons #f request)))
