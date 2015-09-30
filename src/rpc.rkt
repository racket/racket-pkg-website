#lang racket/base

(provide (struct-out exn:fail:rpc)
         rpc-request-evt
         rpc-handler
         rpc-call
         rpc-cast!)

(require racket/match)
(require racket/exn)

(struct exn:fail:rpc exn:fail (inner-exn) #:transparent)

(define (rpc-request-evt)
  (handle-evt (thread-receive-evt)
              (lambda (_) (thread-receive))))

(define-syntax-rule (rpc-handler ch-and-req [(argpat ...) body ...] ...)
  (match ch-and-req
    [(cons ch request)
     (define-values (reply-value new-state)
       (with-handlers [(exn:fail? (lambda (e)
                                    (channel-put ch e)
                                    (raise e)))]
         (match request
           [(list argpat ...) body ...]
           ...)))
     (when ch (channel-put ch reply-value))
     new-state]))

(define (rpc-call thread . request)
  (define ch (make-channel))
  (thread-send thread (cons ch request))
  (define result
    (sync (handle-evt thread
                      (lambda (_)
                        (raise (exn:fail:rpc "Server thread terminated unexpectedly"
                                             (current-continuation-marks)
                                             #f))))
          ch))
  (when (exn? result)
    (raise (exn:fail:rpc (format "RPC exception:\n~a" (exn->string result))
                         (current-continuation-marks)
                         result)))
  result)

(define (rpc-cast! thread . request)
  (thread-send thread (cons #f request)))
