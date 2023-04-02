#lang racket/base

;; This module extends infrastructure-userdb/display-name with functions
;; for rendering pretty HTML. They are designed to respect a potential
;; future user preference for opting out of obfuscation.

(require infrastructure-userdb/display-name
         xml
         racket/contract)

(provide (all-from-out infrastructure-userdb/display-name)
         (contract-out
          [display-name->preferred-tag
           (->* [display-name?]
                [#:obfuscate? any/c]
                symbol?)]
          [display-name->xexpr
           (->* [display-name?]
                [#:obfuscate? any/c]
                xexpr/c)]))

(define (obfuscate-email-address? dn)
  ;; TODO: implement a preference via infrastructure-userdb
  #true)

(define (display-name->preferred-tag dn #:obfuscate? [obfuscate? (obfuscate-email-address? dn)])
  (if obfuscate?
      (display-name-obfuscated-tag dn)
      (display-name-plain-tag dn)))

(define (display-name->xexpr dn  #:obfuscate? [obfuscate? (obfuscate-email-address? dn)])
  (cond
    [obfuscate?
     `(span ()
            ,(display-name-local-part dn)
            (span ([class "text-muted"]
                   [title "This author’s email address has been obfuscated."]
                   [style "cursor: help;"])
                  (b ([style "font: monospace;"])
                     "λ")
                  ,(display-name-short-hash dn)))]
    [else
     (display-name-email dn)]))
