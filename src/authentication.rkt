#lang racket/base

(require racket/class)

(provide userdb<%>)

(define userdb<%>
  (interface ()
    user-exists? ;; String -> Boolean
    create-user! ;; String String -> Void
    reset-user-password! ;; String -> Void
    credentials-valid? ;; String String -> Boolean
    ))
