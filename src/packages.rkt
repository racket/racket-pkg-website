#lang racket/base

(provide @
         @ref
         all-package-names
         all-tags
         sorted-package-names
         package-detail
         package-search
         refresh-packages!)

(require json)
(require racket/set)
(require racket/match)
(require racket/file)
(require racket/string)
(require racket/list)
(require web-server/private/gzip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax @
  (syntax-rules ()
    [(_ v) v]
    [(_ v k rest ...) (@ (@ref v 'k) rest ...)]))

(define (@ref v k)
  (and v (hash-ref v k (lambda () #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define packages (hash))
(define all-tags* (set))

(define (all-package-names)
  (hash-keys packages))

(define (all-tags)
  all-tags*)

(define (sort-package-names names)
  (sort names (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (sorted-package-names)
  (sort-package-names (all-package-names)))

(define (package-detail package-name)
  (hash-ref packages package-name (lambda () #f)))

(define (pkg->searchable-text pkg)
  (string-join (flatten (list (or (@ pkg authors) '())
                              (map (match-lambda
                                    [(list _ path) path]
                                    [_ '()])
                                   (or (@ pkg modules) '()))
                              (or (@ pkg name) '())
                              (or (@ pkg description) '())
                              (or (@ pkg source) '())
                              (or (@ pkg tags) '())
                              (map (match-lambda
                                    [(list _ n _) n]
                                    [_ '()])
                                   (or (@ pkg build docs) '()))))))

(define ((package-text-matches? pkg) re)
  (regexp-match? re (@ pkg _SEARCHABLE-TEXT_)))

(define (package-search text tags)
  (define res (map (lambda (r) (pregexp (format "(?i:~a)" r))) (string-split text)))
  (sort-package-names
   (filter (lambda (package-name)
             (define pkg (hash-ref packages package-name))
             (andmap (package-text-matches? pkg) res))
           (hash-keys
            (for/fold ((ps packages)) ((tag-spec tags))
              (match-define (list tag-name include?) tag-spec)
              (for/hash (((package-name pkg) (in-hash ps))
                         #:when ((if include? values not) (@ref (@ pkg search-terms) tag-name)))
                (values package-name pkg)))))))

(define (refresh-packages!)
  (set! packages
        (for/hash (((package-name pkg)
                    (in-hash (bytes->jsexpr (gunzip/bytes (file->bytes "../pkgs-all.json.gz"))))))
          (values package-name
                  (hash-set pkg '_SEARCHABLE-TEXT_ (pkg->searchable-text pkg)))))
  (set! all-tags*
        (for/fold ((ts (set))) ((pkg (in-hash-values packages)))
          (set-union ts (list->set (or (@ pkg tags) '()))))))

(refresh-packages!)

