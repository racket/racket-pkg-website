#lang racket/base

(provide (struct-out entry-point)
         reload-poll-interval
         set-reload-poll-interval!
         reload-failure-retry-delay
         reload!
         make-entry-point
         lookup-entry-point
         make-persistent-state)

(require racket/match)
(require racket/rerequire)
(require (only-in web-server/private/util exn->string))

(define reload-poll-interval 0.5) ;; seconds
(define reload-failure-retry-delay (make-parameter 10)) ;; seconds

(struct entry-point (name module-path identifier-symbol [value #:mutable]) #:prefab)

(define entry-points (make-hash))
(define persistent-state (make-hash))

(define (set-reload-poll-interval! v)
  (set! reload-poll-interval v))

(define (reloader-main)
  (let loop ()
    (match (sync (handle-evt (thread-receive-evt)
                             (lambda (_) (thread-receive)))
                 (if reload-poll-interval
                     (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                               (* reload-poll-interval 1000)))
                                 (lambda (_) (list #f 'reload)))
                     never-evt))
      [(list ch 'reload)
       (define result (do-reload!))
       (when (not result) (sleep (reload-failure-retry-delay)))
       (when ch (channel-put ch result))])
    (loop)))

(define reloader-thread (thread reloader-main))

(define (reloader-rpc . request)
  (define ch (make-channel))
  (thread-send reloader-thread (cons ch request))
  (channel-get ch))

(define (reload!) (reloader-rpc 'reload))

;; Only to be called from reloader-main
(define (do-reload!)
  (with-handlers ((exn:fail?
                   (lambda (e)
                     (log-error "*** WHILE RELOADING CODE***\n~a" (exn->string e))
                     #f)))
    (for ((e (in-hash-values entry-points)))
      (match-define (entry-point _ module-path identifier-symbol _) e)
      (dynamic-rerequire module-path #:verbosity 'all)
      (set-entry-point-value! e (dynamic-require module-path identifier-symbol)))
    #t))

(define (make-entry-point name module-path [identifier-symbol name])
  (when (hash-has-key? entry-points name)
    (error 'make-entry-point "Duplicate entry-point name ~a" name))
  (define e (entry-point name module-path identifier-symbol #f))
  (hash-set! entry-points name e)
  e)

(define (lookup-entry-point name)
  (hash-ref entry-points name))

(define (make-persistent-state name initial-value-thunk)
  (hash-ref persistent-state
            name
            (lambda ()
              (define value (initial-value-thunk))
              (define handler
                (case-lambda
                  [() value]
                  [(new-value)
                   (set! value new-value)
                   value]))
              (hash-set! persistent-state name handler)
              handler)))
