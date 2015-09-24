#lang racket/base

(provide make-db
         db?
         db-has-key?
         db-ref
         db-set!
         db-remove!
         db-keys)

(require racket/file)
(require file/sha1)

(struct db (name path serializer deserializer) #:transparent)

(define (make-db name path serializer deserializer)
  (make-directory* path)
  (db name path serializer deserializer))

(define (check-key what db key)
  (unless (string? key)
    (error what "Invalid key for db ~a: ~v" (db-name db) key)))

;; We avoid potential filesystem subdirectory escape attacks by
;; encoding key paths into hex. Special characters in keys are thus
;; permitted and rendered harmless.
(define (key->path what db key)
  (check-key what db key)
  (build-path (db-path db) (bytes->hex-string (string->bytes/utf-8 key))))

(define (db-has-key? db key)
  (file-exists? (key->path 'db-has-key? db key)))

(define (db-ref db key default)
  (define p (key->path 'db-ref db key))
  (cond
    [(file-exists? p) ((db-deserializer db) (file->value p))]
    [(procedure? default) (default)]
    [else default]))

(define (db-set! db key value)
  (define p (key->path 'db-set! db key))
  (write-to-file ((db-serializer db) value) p #:exists 'replace))

(define (db-remove! db key)
  (define p (key->path 'db-remove! db key))
  (when (file-exists? p)
    (delete-file p)))

(define (db-keys db)
  (map (lambda (p) (bytes->string/utf-8 (hex-string->bytes (path->string p))))
       (directory-list (db-path db))))
