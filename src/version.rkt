#lang racket/base
(require version/utils)

(provide sort-version-symbols)

(define (sort-version-symbols versions)
  (sort versions (lambda (a b)
                   ;; 'default, then Racket-style versions, then other
                   (cond
                     [(eq? a 'default) #t]
                     [(eq? b 'default) #f]
                     [else
                      (define a-str (symbol->string a))
                      (define b-str (symbol->string b))
                      (cond
                        [(valid-version? a-str)
                         (if (valid-version? b-str)
                             (version<? a-str b-str)
                             #t)]
                        [(valid-version? b-str) #f]
                        [else (string<? a-str b-str)])]))))
