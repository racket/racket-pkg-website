#lang racket/base

(provide rendering-static-page?
         static-render!
         static-put-file!
         static-delete-file!
         static-finish-update!
         extra-files-paths)

(require racket/match)
(require racket/system)
(require racket/path)
(require racket/port)
(require racket/promise)
(require racket/file)
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require web-server/private/servlet)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require file/md5)
(require xml)
(require xml/path)
(require net/url)
(require aws/s3)
(require reloadable)
(require "config.rkt")
(require "daemon.rkt")
(require "rpc.rkt")
(require "hash-utils.rkt")

(define-simple-macro (define/delay id:id expr:expr)
  (begin
    (define delayed (delay expr))
    (define-syntax id
      (make-set!-transformer
       (λ (stx)
         (syntax-parse stx
           #:literals (set! delayed)
           [(set! _ _) (raise-syntax-error 'id "identifier defined with define/delay cannot be mutated")]
           [use:id #'(force delayed)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config

(define/delay static-output-type
  ;; Either 'aws-s3 or 'file
  (or (@ (config) static-output-type)
      'file))

(define/delay aws-s3-bucket+path
  ;; Must end in "/"
  (@ (config) aws-s3-bucket+path))

(define/delay static-generated-directory
  ;; Relevant to static-output-type 'file only
  (config-path (or (@ (config) static-generated-directory)
                   (build-path (var-path) "generated-htdocs"))))

(define/delay static-content-target-directory
  ;; Relevant to static-output-type 'file only
  (let ((p (@ (config) static-content-target-directory)))
    (and p (config-path p))))

(define/delay pkg-index-generated-directory
  (config-path (or (@ (config) pkg-index-generated-directory)
                   (error 'pkg-index-generated-directory "Not specified"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static rendering daemon -- Interface

(define rendering-static-page? (make-parameter #f))

(define (assert-absolute! what absolute-path)
  (when (not (eqv? (string-ref absolute-path 0) #\/))
    (error what "Path must start with /; got ~v" absolute-path)))

(define (static-put-file! absolute-path content-bytes mime-type)
  (assert-absolute! 'static-put-file! absolute-path)
  (renderer-rpc 'put-file! absolute-path content-bytes mime-type))

(define (static-delete-file! absolute-path)
  (assert-absolute! 'static-delete-file! absolute-path)
  (renderer-rpc 'delete-file! absolute-path))

(define (static-render! #:filename [base-filename #f]
                        #:ignore-response-code? [ignore-response-code? #f]
                        #:mime-type mime-type
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
  (define absolute-path (or base-filename request-url))
  (assert-absolute! 'static-render! absolute-path)
  (define content-bytes (call-with-output-bytes (response-output response)))
  (cond
   [(or (<= 200 (response-code response) 299) ;; "OKish" range
        ignore-response-code?)
    (static-put-file! absolute-path content-bytes mime-type)]
   [(= (response-code response) 404) ;; Not found -> delete the file
    (static-delete-file! absolute-path)]
   [else
    (log-warning "Unexpected response code ~v when static-rendering ~v"
                 (response-code response)
                 (cons handler named-url-args))]))

(define (static-finish-update!)
  (renderer-rpc 'finish-update!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static rendering daemon -- Implementation

(define (static-renderer-main)
  (match static-output-type
    ['file (static-renderer-file)]
    ['aws-s3 (static-renderer-aws-s3 #f)])
  (static-renderer-main))

;;---------------------------------------- 'file

(define (static-renderer-file)
  (rpc-handler (sync (rpc-request-evt))
    [('reload!)
     (values (void) (void))]
    [('put-file! absolute-path content-bytes mime-type)
     (define filename (format "~a~a" static-generated-directory absolute-path))
     (make-parent-directory* filename)
     (call-with-output-file filename
       (lambda (p) (write-bytes content-bytes p))
       #:exists 'replace)
     (values (void) (void))]
    [('delete-file! absolute-path)
     (define filename (format "~a~a" static-generated-directory absolute-path))
     (when (file-exists? filename)
       (delete-file filename))
     (values (void) (void))]
    [('finish-update!)
     (when static-content-target-directory
       (make-directory* static-content-target-directory)
       (define command
         (append (list (path->string (find-executable-path "rsync"))
                       "-a"
                       "--delete"
                       (path->string (build-path static-generated-directory "."))
                       (path->string (build-path (config-path "../static") ".")))
                 (list (path->string (build-path pkg-index-generated-directory ".")))
                 (list (path->string (build-path static-content-target-directory ".")))))
       (log-info "Executing rsync to replicate static content; argv: ~v" command)
       (apply system* command))
     (values (void) (void))]))

;;---------------------------------------- 'aws-s3

(define (initial-aws-s3-index)
  (for/hash [(entry (ls/proc aws-s3-bucket+path append '()))]
    (match-define (pregexp "^\"(.*)\"$" (list _ file-md5-str))
      (apply string-append (se-path*/list '(ETag) entry)))
    (values (se-path* '(Key) entry)
            (string->bytes/utf-8 file-md5-str))))

(define (absolute-path->relative-path absolute-path)
  (assert-absolute! 'absolute-path->relative-path absolute-path)
  (substring absolute-path 1))

(define put-bytes-sema (make-semaphore 10))
(define (put/bytes^ p cb mt h)
  (semaphore-wait put-bytes-sema)
  (thread
   (lambda ()
     (with-handlers ((values (lambda (e)
                               (semaphore-post put-bytes-sema)
                               (raise e))))
       (put/bytes p cb mt h)
       (semaphore-post put-bytes-sema)))))

(define (aws-put-file! index absolute-path content-bytes mime-type [headers '()])
  (define relative-path (absolute-path->relative-path absolute-path))
  (define new-md5 (md5 content-bytes))
  (if (equal? new-md5 (hash-ref index relative-path #f))
      (begin
        ;; (log-info "Not uploading ~a to S3, since MD5 has not changed" relative-path)
        (void))
      (begin
        (log-info "Uploading ~a to S3; new MD5 = ~a" relative-path new-md5)
        (put/bytes^ (string-append aws-s3-bucket+path relative-path)
                    content-bytes
                    mime-type
                    (cons (cons 'x-amz-acl "public-read")
                          headers))))
  (hash-set index relative-path new-md5))

(define (aws-delete-file! index absolute-path)
  (define relative-path (absolute-path->relative-path absolute-path))
  (log-info "Deleting ~a from S3" relative-path)
  (delete (string-append aws-s3-bucket+path relative-path))
  (hash-remove index relative-path))

(define (extension-map p)
  (match (filename-extension p)
    [#"html" "text/html"]
    [#"css" "text/css"]
    [#"js" "application/javascript"]
    [#"json" "application/json"]
    [#"png" "image/png"]
    [#"svg" "image/svg"]
    [#f "application/octet-stream"]
    [other ;; (log-info "Unknown extension in extension-map: ~a" other)
           "application/octet-stream"]))

(define (upload-directory! index source-directory0 target-absolute-path-prefix)
  (define source-directory (simple-form-path source-directory0))
  (for/fold [(index index)]
            [(filepath (find-files file-exists? source-directory))]
    (define absolute-path
      (path->string (build-path target-absolute-path-prefix
                                (find-relative-path source-directory filepath))))
    ;; https://github.com/tonyg/racket-pkg-website/issues/28
    ;; TOCTTOU: we checked that `file-exists?` above, but that may have changed since!
    (define contents
      (with-handlers [(exn:fail:filesystem?
                       ;; ^ It would be nice to be able to be more precise here, e.g.
                       ;; file-not-found, but `file->bytes` delegates to `file-size` which
                       ;; only raises `exn:fail:filesystem` when a problem occurs.
                       (lambda (e)
                         (log-warning "Transient (?) problem reading ~v: ~v"
                                      filepath
                                      (exn-message e))
                         #f))]
        (file->bytes filepath)))
    (if contents
        (aws-put-file! index absolute-path contents (extension-map filepath))
        (aws-delete-file! index absolute-path))))

(define (configure-s3-cors!)
  (log-info "Configuring S3 CORS headers:\n~a"
            (put/bytes (string-append aws-s3-bucket+path "?cors")
                       (string->bytes/utf-8 (xexpr->string
                                             `(CORSConfiguration
                                               (CORSRule (AllowedOrigin "*")
                                                         (AllowedMethod "GET")
                                                         (AllowedHeader "*")))))
                       "application/xml"
                       '())))

(define (static-renderer-aws-s3 index)
  (s3-region "us-west-2")
  (when (not index) (configure-s3-cors!))
  (let ((index (or index (initial-aws-s3-index))))
    (match
        (rpc-handler (sync (rpc-request-evt))
          [('reload!)
           (values (void) 'reload!)]
          [('put-file! absolute-path content-bytes mime-type)
           (values (void) (aws-put-file! index absolute-path content-bytes mime-type))]
          [('delete-file! absolute-path)
           (values (void) (aws-delete-file! index absolute-path))]
          [('finish-update!)
           (let* ((index (upload-directory! index (build-path (config-path "../static") ".") "/"))
                  (index (upload-directory! index
                                            (build-path pkg-index-generated-directory "pkg")
                                            "/pkg/")))
             (values (void)
                     (for/fold [(index index)]
                               [(leaf (in-list `(("atom.xml" "application/atom+xml")
                                                 ("pkgs" "application/octet-stream")
                                                 ("pkgs-all" "application/octet-stream")
                                                 ("pkgs-all.json.gz" "application/json"
                                                  (Content-Encoding . "gzip"))
                                                 ("pkgs.json" "application/json"))))]
                       (match-define (list* filename mime-type headers) leaf)
                       (aws-put-file! index
                                      (path->string (build-path "/" filename))
                                      (file->bytes
                                       (build-path pkg-index-generated-directory filename))
                                      mime-type
                                      headers))))])
      ['reload! (void)] ;; effectively restarts daemon
      [next-index (static-renderer-aws-s3 next-index)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static rendering daemon -- Startup

(define static-renderer-thread
  (make-persistent-state 'static-renderer-thread
                         (lambda ()
                           (daemon-thread 'static-renderer
                                          (lambda ()
                                            (static-renderer-main))))))

(define (renderer-rpc . request) (apply rpc-call (static-renderer-thread) request))

(module+ main
  (renderer-rpc 'reload!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to web-server static file serving

(define (extra-files-paths)
  (list static-generated-directory
        (config-path "../static")
        pkg-index-generated-directory))
