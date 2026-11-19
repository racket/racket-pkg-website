#lang racket/base

(require racket/runtime-path
         net/url-string)

(provide licenses.json
         exceptions.json
         spdx-url)

(module+ main
  (require (prefix-in http: net/http-easy)
           racket/path
           racket/file
           racket/cmdline)
  (command-line
   #:usage-help "update “licenses.json” and “exceptions.json” from spdx.org"
   #:args ()
   (for ([pth (list licenses.json exceptions.json)])
     (define url (spdx-url (path-element->string (file-name-from-path pth))))
     (define response (http:get url #:max-redirects 0))
     (unless (<= 200 (http:response-status-code response) 299)
       (error 'spdx-update "unexpected HTTP status ~a for ~a"
              (http:response-status-code response) (url->string url)))
     (call-with-atomic-output-file pth
       (λ (out _tmp)
         (write-bytes (http:response-body response) out))))))

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
