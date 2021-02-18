#lang racket/base

(provide (struct-out challenge)
         generate-challenge
         challenge-passed?)

(require racket/pretty)

(struct challenge (expr question answer) #:transparent)

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (generate-expr)
  (if (>= (random) 0.5)
      `(,(random-element '(car cadr caddr))
        (map (lambda (v) (+ ,(random 4) (* v ,(random 4))))
             (list ,(random 4) ,(random 4) ,(random 4))))
      (let ()
        (define (random-op) (random-element '(+ * -)))
        (define (e fuel)
          (if (zero? fuel)
              (random 10)
              (cons (random-op)
                    (for/list [(i (in-range (+ 1 (random 2))))]
                      (e (- fuel 1))))))
        (e 2))))

(define (generate-challenge)
  (define expr (generate-expr))
  (challenge expr
             `(div
               (p (b "What is the result of evaluating:"))
               (pre (code ,(pretty-format expr 40 #:mode 'write))))
             (eval expr (make-base-namespace))))

(define (safe-string->value str)
  (parameterize (;; Hmm, this is a big list. Did I miss any important ones?
                 (read-accept-box #f)
                 (read-accept-compiled #f)
                 (read-accept-graph #f)
                 (read-accept-reader #f)
                 (read-accept-lang #f))
    (read (open-input-string str))))

(define (challenge-passed? challenge response-str)
  (define response (safe-string->value response-str))
  (equal? response (challenge-answer challenge)))
