#lang racket/base

(provide all-package-names
         sorted-package-names
         package-detail
         refresh-packages!)

(require json)
(require racket/file)
(require web-server/private/gzip)

(define packages #f)

(define (refresh-packages!)
  (set! packages (bytes->jsexpr (gunzip/bytes (file->bytes "../pkgs-all.json.gz")))))

(define (all-package-names)
  (hash-keys packages))

(define (sorted-package-names)
  (sort (all-package-names)
        (lambda (a b)
          (string<? (symbol->string a) (symbol->string b)))))

(define (package-detail package-name)
  (hash-ref packages package-name (lambda () #f)))

(refresh-packages!)

