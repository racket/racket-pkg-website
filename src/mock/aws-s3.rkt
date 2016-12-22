#lang racket/base

(provide create-bucket
         delete-bucket
         ls/proc
         put/bytes
         get/bytes
         delete)

(require file/glob)
(require file/md5)
(require net/uri-codec)
(require racket/date)
(require racket/dict)
(require racket/file)
(require racket/list)
(require racket/match)
(require racket/string)
(require "../config.rkt")

(module+ test (require rackunit))

(define (create-bucket bucket [location #f])
  (define-values (full-bucket-path path-str) (split-bucket+path/create bucket))
  (when path-str (error 'create-bucket "Do not include a path within the bucket: ~v" bucket)))

(define (delete-bucket bucket)
  (define-values (full-bucket-path path-str) (split-bucket+path bucket))
  (when path-str (error 'delete-bucket "Do not include a path within the bucket: ~v" bucket))
  (delete-directory/files full-bucket-path #:must-exist? #f))

(define (ls/proc bucket+path proc init [max-each 1000] #:delimiter [delimiter #f])
  (when delimiter (error 'ls/proc "mock/aws-s3 lacks support for non-#f delimiter"))
  (define-values (full-bucket-path path-str) (split-bucket+path/create bucket+path))
  (define all-files (for/list [(p (glob (build-path full-bucket-path "*")))]
                      (define-values (_dirp f _must-be-dir?) (split-path p))
                      f))
  (define matching-files
    (if path-str
        (filter (lambda (f) (string-prefix? (unescape-filename f) path-str)) all-files)
        all-files))
  (for/fold [(acc init)] [(group (batch matching-files max-each))]
    (proc init (map (lambda (f) (ListBucketResults-file full-bucket-path f)) group))))

(define (bucket+path->file-path bucket+path)
  (define-values (full-bucket-path path-str) (split-bucket+path/create bucket+path))
  (build-path full-bucket-path (escape-filename (or path-str ""))))

(define (put/bytes bucket+path data mime-type [heads '()])
  (unless (dict-empty? heads)
    (log-warning "mock put/bytes: ignoring non-empty 'heads' dictionary: ~v" heads))
  (display-to-file data (bucket+path->file-path bucket+path) #:exists 'replace))

(define (get/bytes bucket+path [heads '()] [range-begin #f] [range-end #f])
  ;; Signals an error when the file doesn't exist, but not the same
  ;; error the real S3 package signals.
  ;;
  (when (or range-begin range-end)
    (error 'get/bytes "mock/aws-s3 lacks support for get ranges: ~v/~v" range-begin range-end))
  (unless (dict-empty? heads)
    (log-warning "mock get/bytes: ignoring non-empty 'heads' dictionary: ~v" heads))
  (file->bytes (bucket+path->file-path bucket+path)))

(define (delete bucket+path)
  (with-handlers [(exn:fail:filesystem? void)]
    ;; ^ ugh, can't distinguish file-not-found from any other error.
    (delete-file (bucket+path->file-path bucket+path))))

(module+ test
  (define B "testbucket.mock.aws-s3")
  (delete-bucket B)
  (delete-bucket B) ;; it's supposed to be idempotent
  (create-bucket B)
  (create-bucket B) ;; should also be idempotent
  (check-equal? (ls/proc (string-append B "/") append '()) '())
  (put/bytes (string-append B "/foo/bar") #"/foo/bar" "text/plain")
  (put/bytes (string-append B "/bar") #"/bar" "text/plain")
  (check-equal? (get/bytes (string-append B "/foo/bar")) #"/foo/bar")
  (check-equal? (get/bytes (string-append B "/bar")) #"/bar")
  (check-match
   (ls/proc (string-append B "/") append '())
   `((Contents ()
               (Key () "bar")
               (LastModified () ,_)
               (ETag () "\"" "6a764eebfa109a9ef76c113f3f608c6b" "\"")
               (Size () "4")
               (Owner ()
                      (ID () "0000000000000000000000000000000000000000000000000000000000000000")
                      (DisplayName () "mockuser"))
               (StorageClass () "STANDARD"))
     (Contents ()
               (Key () "foo/bar")
               (LastModified () ,_)
               (ETag () "\"" "1df481b1ec67d4d8bec721f521d4937d" "\"")
               (Size () "8")
               (Owner ()
                      (ID () "0000000000000000000000000000000000000000000000000000000000000000")
                      (DisplayName () "mockuser"))
               (StorageClass () "STANDARD"))))
  (delete (string-append B "/zot")) ;; idempotent
  (delete (string-append B "/bar"))
  (check-match
   (ls/proc (string-append B "/") append '())
   `((Contents ()
               (Key () "foo/bar")
               (LastModified () ,_)
               (ETag () "\"" "1df481b1ec67d4d8bec721f521d4937d" "\"")
               (Size () "8")
               (Owner ()
                      (ID () "0000000000000000000000000000000000000000000000000000000000000000")
                      (DisplayName () "mockuser"))
               (StorageClass () "STANDARD"))))
  (delete-bucket B))

(define (batch items batch-size)
  (if (<= (length items) batch-size)
      (if (null? items)
          '()
          (list items))
      (let-values (((h t) (split-at items batch-size)))
        (cons h (batch t batch-size)))))

(module+ test
  (check-equal? (batch '() 3) '())
  (check-equal? (batch '(x) 3) '((x)))
  (check-equal? (batch '(x y z) 3) '((x y z)))
  (check-equal? (batch '(x y z w) 3) '((x y z) (w)))
  (check-equal? (batch '(x y z w a b c d) 3) '((x y z) (w a b) (c d)))
  (check-equal? (batch '(x y z w a b c d e) 3) '((x y z) (w a b) (c d e))))

(define (ListBucketResults-file full-bucket-path f)
  (define path (build-path full-bucket-path f))
  (define checksum (md5 (file->bytes path)))
  (define mtime (file-or-directory-modify-seconds path))
  `(Contents ()
             (Key () ,(unescape-filename f))
             (LastModified () ,(parameterize ((date-display-format 'iso-8601))
                                 (string-append (date->string (seconds->date mtime #f) #t)
                                                ".000Z")))
             (ETag () "\"" ,(bytes->string/utf-8 checksum) "\"")
             (Size () ,(number->string (file-size path)))
             (Owner ()
                    (ID () "0000000000000000000000000000000000000000000000000000000000000000")
                    (DisplayName () "mockuser"))
             (StorageClass () "STANDARD")))

(define (escape-filename f)
  (unless (string? f) (error 'escape-filename "Expects a string: ~v" f))
  (string->path (string-append "f-" (uri-encode f))))

(define (unescape-filename f)
  (unless (path? f) (error 'escape-filename "Expects a path: ~v" f))
  (match (path->string f)
    [(regexp #px"f-(.*)" (list _ s)) (uri-decode s)]
    [_ (error 'unescape-filename "Invalid filename: ~v" f)]))

(module+ test
  (check-equal? (escape-filename "") (string->path "f-"))
  (check-equal? (escape-filename "abc") (string->path "f-abc"))
  (check-equal? (escape-filename "abc/def") (string->path "f-abc%2Fdef"))
  (check-equal? (escape-filename "abc%def") (string->path "f-abc%25def"))
  (check-equal? (unescape-filename (string->path "f-")) "")
  (check-equal? (unescape-filename (string->path "f-abc")) "abc")
  (check-equal? (unescape-filename (string->path "f-abc%2Fdef")) "abc/def")
  (check-equal? (unescape-filename (string->path "f-abc%25def")) "abc%def"))

(define (split-bucket+path bucket+path)
  (define elements0 (explode-path bucket+path))
  (when (null? elements0) (error 'split-bucket+path/create "No bucket supplied"))
  (define elements (if (equal? (string->path "/") (car elements0))
                       (cdr elements0)
                       elements0))
  (match-define (cons bucket-path path-element-paths) elements)
  (define full-bucket-path (build-path (var-path) "mock/aws-s3" bucket-path))
  (values full-bucket-path
          (and (pair? path-element-paths)
               (path->string (apply build-path path-element-paths)))))

(define (split-bucket+path/create bucket+path)
  (define-values (full-bucket-path path-str) (split-bucket+path bucket+path))
  (make-directory* full-bucket-path)
  (values full-bucket-path path-str))
