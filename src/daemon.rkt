#lang racket/base

(provide daemonize-thunk
         daemon-thread)

(require (only-in web-server/private/util exn->string))

(define (daemonize-thunk name boot-thunk)
  (lambda ()
    (let reboot ()
      (with-handlers* ((exn:fail? (lambda (e)
                                    (log-error "*** DAEMON CRASHED: ~a ***\n~a"
                                               name
                                               (exn->string e))
                                    (sleep 5)
                                    (reboot))))
        (define result (boot-thunk))
        (log-warning "Daemon thread ~a exited normally (returning ~v)" name result)))))

(define (daemon-thread name boot-thunk)
  (thread (daemonize-thunk name boot-thunk)))
