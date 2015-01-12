#lang racket/base

(provide current-session
         current-email
         session-lifetime
         (struct-out session)
         create-session!
         destroy-session!
         lookup-session/touch!
         lookup-session)

(require "randomness.rkt")
(require "config.rkt")
(require reloadable)

(define current-session (make-parameter #f))
(define session-lifetime
  (* (or (hash-ref (config) 'session-lifetime-seconds #f)
         (* 7 24 60 60)) ;; one week in seconds
     1000)) ;; convert to milliseconds

(struct session (key expiry email password) #:prefab)

(define sessions (make-persistent-state 'session-store (lambda () (make-hash))))

(define (current-email)
  (define s (current-session))
  (and s (session-email s)))

(define (expire-sessions!)
  (define now (current-inexact-milliseconds))
  (define ss (sessions))
  (for ((session-key (hash-keys ss)))
    (define s (hash-ref ss session-key (lambda () #f)))
    (when (and s (<= (session-expiry s) now))
      (hash-remove! ss session-key))))

(define (create-session! email password)
  (expire-sessions!)
  (define session-key (bytes->string/utf-8 (random-bytes/base64 32)))
  (hash-set! (sessions)
             session-key
             (session session-key
                      (+ (current-inexact-milliseconds) (session-lifetime))
                      email
                      password))
  session-key)

(define (destroy-session! session-key)
  (hash-remove! (sessions) session-key))

(define (lookup-session/touch! session-key)
  (define s (hash-ref (sessions) session-key (lambda () #f)))
  (and s
       (let ((s1 (struct-copy session s [expiry (+ (current-inexact-milliseconds)
                                                   (session-lifetime))])))
         (hash-set! (sessions) session-key s1)
         s1)))

(define (lookup-session session-key)
  (hash-ref (sessions) session-key (lambda () #f)))
