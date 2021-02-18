#lang racket/base
;; User management - userdb, plus registration and emailing

(provide login-password-correct?
         send-registration-or-reset-email!
         registration-code-correct?
         register-or-update-user!)

(require net/sendmail)
(require reloadable)
(require infrastructure-userdb)
(require "config.rkt")
(require "hash-utils.rkt")

(define-logger racket-pkg-website/users)

(define userdb (userdb-config (@ (config) user-directory)
                              #t ;; writeable!
                              ))

(define *codes*
  (make-persistent-state '*codes* (lambda () (make-registration-state))))

(define (login-password-correct? email given-password)
  (log-racket-pkg-website/users-info "Checking password for ~v" email)
  (user-password-correct? (lookup-user userdb email) given-password))

(define (send-registration-or-reset-email! email)
  (if (user-exists? userdb email)
      (send-password-reset-email! email)
      (send-account-registration-email! email)))

(define (sender-address)
  (or (@ (config) user-directory)
      "pkgs@racket-lang.org"))

(define (send-password-reset-email! email)
  (log-racket-pkg-website/users-info "Sending password reset email to ~v" email)
  (send-mail-message
   (sender-address)
   "Account password reset for Racket Package Catalog"
   (list email)
   '()
   '()
   (list
    "Someone tried to login with your email address for an account on the Racket Package Catalog, but failed."
    "If this was you, please use this code to reset your password:"
    ""
    (generate-registration-code! (*codes*) email)
    ""
    "This code will expire, so if it is not available, you'll have to try again.")))

(define (send-account-registration-email! email)
  (log-racket-pkg-website/users-info "Sending account registration email to ~v" email)
  (send-mail-message
   (sender-address)
   "Account confirmation for Racket Package Catalog"
   (list email)
   '()
   '()
   (list
    "Someone tried to register your email address for an account on the Racket Package Catalog."
    "If you want to proceed, use this code:"
    ""
    (generate-registration-code! (*codes*) email)
    ""
    "This code will expire, so if it is not available, you'll have to try to register again.")))

(define (registration-code-correct? email given-code)
  (log-racket-pkg-website/users-info "Checking registration code for ~v" email)
  (check-registration-code (*codes*)
                           email
                           given-code
                           (lambda () #t)
                           (lambda () #f)))

(define (register-or-update-user! email password)
  (log-racket-pkg-website/users-info "Updating user record ~v" email)
  (save-user! userdb
              (user-password-set (or (lookup-user userdb email)
                                     (make-user email password))
                                 password)))
