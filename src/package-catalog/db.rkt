#lang racket/base

(provide make-db
         db?
         db-has-key?
         db-ref
         db-set!
         db-remove!
         db-keys)

(require racket/file)

(struct db (name path serializer deserializer) #:transparent)

(define (make-db name path serializer deserializer)
  (make-directory* path)
  (db name path serializer deserializer))

(define (check-key what db key)
  (unless (string? key)
    (error what "Invalid key for db ~a: ~v" (db-name db) key)))

(define (db-has-key? db key)
  (check-key 'db-has-key? db key)
  (file-exists? (build-path (db-path db) key)))

(define (db-ref db key default-thunk)
  (check-key 'db-ref db key)
  (define p (build-path (db-path db) key))
  (if (file-exists? p)
      ((db-deserializer db) (file->value p))
      (default-thunk)))

(define (db-set! db key value)
  (check-key 'db-set! db key)
  (write-to-file value (build-path (db-path db) key)
                 #:exists 'replace))

(define (db-remove! db key)
  (check-key 'db-remove! db key)
  (define p (build-path (db-path db) key))
  (when (file-exists? p)
    (delete-file p)))

(define (db-keys db)
  (map path->string (directory-list (db-path db))))
