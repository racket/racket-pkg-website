#lang racket/base

(provide daemonize-thunk
         daemon-thread)

(require (only-in web-server/private/util exn->string))

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
