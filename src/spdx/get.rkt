#lang racket/base

(require racket/runtime-path
         net/url-string)

(provide licenses.json
         exceptions.json
         spdx-url)

(module+ main
  (require net/url
           net/url-connect
           openssl
           racket/path
           racket/file
           racket/port
           racket/cmdline)
  (command-line
   #:usage-help "update “licenses.json” and “exceptions.json” from spdx.org"
   #:args ()
   (parameterize ([current-https-protocol 'secure])
     (for ([pth (list licenses.json exceptions.json)])
       (define in
         (get-pure-port
          (spdx-url (path-element->string (file-name-from-path pth)))))
       (call-with-atomic-output-file pth
         (λ (out _tmp)
           (copy-port in out)))))))

(define-runtime-path licenses.json
  "licenses.json")

(define-runtime-path exceptions.json
  "exceptions.json")

(define (spdx-url file)
  (url "https" #f "spdx.org" #f #t
       (list (path/param "licenses" '())
             (path/param file '()))
       '()
       #f))
