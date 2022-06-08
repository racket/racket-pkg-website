#lang racket/base

(require "spdx/get.rkt"
         net/url-string
         racket/symbol
         racket/string
         racket/match
         json)

(provide parse-license-jsexpr)

(module+ test
  (require rackunit))

;; https://spdx.github.io/spdx-spec/SPDX-license-expressions/#d2-case-sensitivity
;;  - Operators (e.g. "WITH") are case-sensitive.
;;  - License and exception identifiers are NOT case-sensitive:
;;    the canonical case can matter for some purposes, but currently not ours.
;;    We convert them into case-folded symbols for comparison.

(define (parse-spdx-json pth list-key id-key url-key)
  (for/hasheq ([hsh (hash-ref (call-with-input-file* pth read-json) list-key)]
               #:unless (hash-ref hsh 'isDeprecatedLicenseId #f))
    (values (string->symbol (string-foldcase (hash-ref hsh id-key)))
            (let ([u (hash-ref hsh url-key)])
              (cond
                [(string-prefix? u "https://")
                 u]
                [(string-prefix? u "./")
                 (url->string (spdx-url (substring u 2)))]
                [else
                 (error 'parse-spdx-json "bad url: ~s" u)])))))

(define licenses
  (parse-spdx-json licenses.json 'licenses 'licenseId 'reference))

(define exceptions
  (parse-spdx-json exceptions.json 'exceptions 'licenseExceptionId 'detailsUrl))

(define custom-license-rx
  #px"^(?i:(?:DocumentRef-[-.[:alpha:][:digit:]]+:)?LicenseRef-[-.[:alpha:][:digit:]]+)$")


(define memo (make-ephemeron-hasheq))

;; parse-license-jsexpr : any/c -> (or/c 'missing
;;                                       (cons/c (or/c 'valid 'innvalid 'ill-formed)
;;                                               (listof xexpr/c)))
;; INVARIANT: The argument should be interned in the sense of `datum-intern-literal`.
;; If the argument is not (or/c #f string?), we've gotten bad data from the back end.
(define (parse-license-jsexpr js)
  (cond
    [(string? js)
     (hash-ref! memo
                js
                (λ ()
                  (call-with-values (λ ()
                                      (let/ec fail-k
                                        (check-license-sexp
                                         (call-with-default-reading-parameterization
                                          (λ ()
                                            (with-handlers ([exn:fail? (λ (e)
                                                                         (fail-k))])
                                              (read (open-input-string js)))))
                                         fail-k)))
                    (case-lambda
                      [()
                       `(ill-formed (code ,js))]
                      [(valid? xs)
                       (cons (if valid? 'valid 'invalid)
                             xs)]))))]
    [js
     (log-error "parse-license-jsexpr: ~a\n  expected: (or/c #f string?)\n  given: ~e"
                "unexpected argument;\n please fix pkg-index"
                js)
    `(ill-formed (code ,(format "~v" js)))]
    [else
     'missing]))


;; check-license-sexp : any/c (-> none/c) -> (values boolean? (listof xexpr/c))
;; Given an alleged license S-expression and a thunk that aborts the continuation,
;; either calls the thunk (if the license-sexp is "ill-formed") or returns two values:
;; a boolean indicating if the license-sexp is "valid" (i.e. all license and exception IDs
;; are recognized) and its rendering as a list of x-expressions.
(define (check-license-sexp v fail-k)
  (let loop ([v v])
    (match v
      [(? symbol?)
       (check-license-id v)]
      [`(,(? symbol? (app check-license-id license-valid? license-xs))
         WITH
         ,(? symbol? (app check-exception-id exception-valid? exception-xs)))
       (values (and license-valid? exception-valid?)
               `("(" ,@license-xs " WITH " ,@exception-xs ")"))]
      [`(,(app loop lhs-valid? lhs-xs)
         ,(and (or 'AND 'OR) rator)
         ,(app loop rhs-valid? rhs-xs))
       (values (and lhs-valid? rhs-valid?)
               `("(" ,@lhs-xs
                     " "
                     ,(symbol->immutable-string rator)
                     " "
                     ,@rhs-xs
                     ")"))]
      [_
       (fail-k)])))


;; check-exception-id : symbol? -> (values boolean? (listof xexpr/c))
(define (check-exception-id sym)
  (define str (symbol->immutable-string sym))
  (cond
    [(hash-ref exceptions (string->symbol (string-foldcase str)) #f)
     => (λ (url)
          (values #t `((a ([href ,url]) ,str))))]
    [else
     (values #f (invalid-id 'exception str))]))


;; check-license-id : symbol? -> (values boolean? (listof xexpr/c))
(define (check-license-id sym)
  (define (check-sans-+ str)
    (cond
      [(hash-ref licenses (string->symbol (string-foldcase str)) #f)
       => (λ (url)
            (values #t `((a ([href ,url]) ,str))))]
      [(regexp-match? custom-license-rx str)
       (values #t (list str))]
      [else
       (values #f (invalid-id 'license str))]))
  (define str (symbol->immutable-string sym))
  (cond
    [(string-suffix? str "+")
     (define-values [valid? xs]
       (check-sans-+ (substring str 0 (sub1 (string-length str)))))
     (values valid? `(,@xs "+"))]
    [else
     (check-sans-+ str)]))


;; invalid-id : (or/c 'license 'exception) string? -> (listof xexpr/c)
(define (invalid-id kind str)
  `((s ([class "text-danger"]
        [aria-description ,(match kind
                             ['license
                              "Invalid SPDX license identifier."]
                             ['exception
                              "Invalid SPDX exception identifier."])])
       ,str)))

(module+ test
  (define-binary-check (check-pairof actual expected)
    (equal? actual (cons expected expected)))
  (define (status v)
    (cons (match (parse-license-jsexpr
                  (datum-intern-literal (format "~s" v)))
            [(cons ret _)
             ret]
            [ret
             ret])
          (call-with-values (λ ()
                              (let/ec fail-k
                                (check-license-sexp v fail-k)))
            (case-lambda
              [()
               'ill-formed]
              [(valid? xs)
               (if valid? 'valid 'invalid)]))))
  (check-pairof
   (status 'SchemeReport)
   'valid)
  (check-pairof
   (status '(SchemeReport WITH Font-exception-2.0))
   'valid)
  (check-pairof
   (status '((SchemeReport WITH Font-exception-2.0) OR MIT))
   'valid)
  (check-pairof
   (status '((SchemeReport WITH Font-exception-2.0) OR (MIT AND XYZ)))
   'invalid)
  (check-pairof
   (status '((SchemeReport WITH Font-exception-2.0) OR (MIT AND LicenseRef-MIT-Style-1)))
   'valid)
  (check-pairof
   (status 'DocumentRef-spdx-tool-1.2:LicenseRef-MIT-Style-2)
   'valid)
  (check-pairof
   (status '((SchemeReport WITH Font-exception-2.0) OR))
   'ill-formed))
