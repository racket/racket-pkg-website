#lang racket/base

(provide daemonize-thunk
         daemon-thread

         rpc-request-evt
         rpc-handler
         rpc-call
         rpc-cast!)

(require (only-in web-server/private/util exn->string))
(require racket/match)

(define (daemonize-thunk name boot-thunk)
  (lambda ()
    (let reboot ()
      ;; We would catch exn:fail? here, but exn:pretty in the web
      ;; server is a subtype of exn, not of exn:fail, and that causes
      ;; spurious permanent daemon exits.
      (with-handlers* ((exn? (lambda (e)
                               (log-error "*** DAEMON CRASHED: ~a ***\n~a"
                                          name
                                          (exn->string e))
                               (sleep 5)
                               (reboot))))
        (define result (boot-thunk))
        (log-warning "Daemon thread ~a exited normally (returning ~v)" name result)))))

(define (daemon-thread name boot-thunk)
  (thread (daemonize-thunk name boot-thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
