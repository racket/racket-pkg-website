#lang racket/base

(provide static-generated-directory
         rendering-static-page?
         static-render!
         extra-files-paths)

(require racket/promise)
(require racket/file)
(require web-server/private/servlet)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require net/url)
(require "config.rkt")
(require "hash-utils.rkt")

(define static-generated-directory
  (config-path (or (@ (config) static-generated-directory)
                   (build-path (var-path) "generated-htdocs"))))

(define rendering-static-page? (make-parameter #f))

(define (static-render! #:filename [base-filename #f]
                        named-url handler . named-url-args)
  (define request-url (apply named-url handler named-url-args))
  (log-info "Rendering static version of ~a~a"
            request-url
            (if base-filename
                (format " to ~a" base-filename)
                ""))
  (define response
    (parameterize ((rendering-static-page? #t))
      (call-with-continuation-barrier
       (lambda ()
         (call-with-continuation-prompt
          (lambda ()
            (apply handler
                   (request #"GET"
                            (string->url request-url)
                            '()
                            (delay '())
                            #f
                            "127.0.0.1"
                            0
                            "127.0.0.1")
                   named-url-args))
          servlet-prompt)))))
  (define filename (format "~a~a" static-generated-directory (or base-filename request-url)))
  (cond
   [(<= 200 (response-code response) 299) ;; "OKish" range
    (make-parent-directory* filename)
    (call-with-output-file filename
      (response-output response)
      #:exists 'replace)]
   [(= (response-code response) 404) ;; Not found -> delete the file
    (when (file-exists? filename)
      (delete-file filename))]
   [else
    (log-warning "Unexpected response code ~v when static-rendering ~v"
                 (response-code response)
                 (cons handler named-url-args))]))

(define (extra-files-paths)
  (list (config-path static-generated-directory)
        (config-path "../static")))
