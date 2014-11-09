#lang racket/base
;; XExpr utilities

(provide xexpr-case)
(module+ test (require rackunit))

(require racket/match)

(define (xexpr-case x content element)
  (match x
    [(? string? s) (content s)]
    [(list tag (list (list key value) ...) body ...) (element tag (map list key value) body)]
    [(list tag body ...) (element tag '() body)]
    [else (error 'xexpr-case "Invalid xexpr: ~a" x)]))

(module+ test
  (define (x xexpr) (xexpr-case xexpr values list))
  (check-equal? (x "hello") "hello")

  (check-equal? (x `(a)) (list 'a '() '()))
  (check-equal? (x `(a ())) (list 'a '() '()))
  (check-equal? (x `(a ((href "hello")))) (list 'a '((href "hello")) '()))
  (check-equal? (x `(a ((href "hello") (class "foo"))))
                (list 'a '((href "hello") (class "foo")) '()))

  (check-equal? (x `(div "content" (b "bold")))
                (list 'div '() '("content" (b "bold"))))
  (check-equal? (x `(div () "content" (b "bold")))
                (list 'div '() '("content" (b "bold"))))
  (check-equal? (x `(div ((id "hello")) "content" (b "bold")))
                (list 'div '((id "hello")) '("content" (b "bold"))))
  (check-equal? (x `(div ((id "hello") (class "foo")) "content" (b "bold")))
                (list 'div '((id "hello") (class "foo")) '("content" (b "bold"))))

  )
