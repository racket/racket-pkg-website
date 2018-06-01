#lang racket/base
;; Code for debugging a live server.

(provide debug-information-dump!)

(require "main.rkt")
(require racket/exn)
(require racket/tcp)
(require (only-in racket/string string-join))

(define (format-path path)
  (string-join (map number->string (reverse path)) "."))

(define (enumerate-custodian-managed-items cust super path)
  (for [(index (in-naturals))
        (item (custodian-managed-list cust super))]
    (eprintf "\nItem ~a.\n~v\n" (format-path (cons index path)) item)
    (cond
      [(thread? item)
       (eprintf "~a" (exn->string (exn "Stack snapshot:" (continuation-marks item))))]
      [(tcp-port? item)
       (eprintf "TCP port: (addresses ~v)\n"
                (call-with-values (lambda () (tcp-addresses item #t)) list))]
      [(custodian? item)
       (enumerate-custodian-managed-items item cust (cons index path))]
      [else (void)])))

(define (debug-information-dump!)
  (eprintf "===========================================================================\n")
  (eprintf "======================================================================\n")
  (eprintf "=================================================================")
  (collect-garbage)
  (enumerate-custodian-managed-items (current-custodian) (outermost-custodian) '())
  (eprintf "=================================================================\n")
  (eprintf "======================================================================\n")
  (eprintf "===========================================================================\n"))
