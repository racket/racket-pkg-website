#lang racket/base
;; Inner startup module - required after establishment of server-wide custodian.

(provide main)

(require racket/runtime-path)
(require reloadable)
(require "entrypoint.rkt")
(require "default.rkt")
(require (prefix-in pkg-index: "pkg-index/config.rkt")
         (prefix-in pkg-index: "pkg-index/dynamic.rkt"))

(define-runtime-module-path-index packages.rkt "packages.rkt")
(define-runtime-module-path-index debug.rkt "debug.rkt")
(define-runtime-module-path-index site.rkt "site.rkt")
(define-runtime-module-path-index static.rkt "static.rkt")

(define (as-module-path mpi)
  (resolved-module-path-name (module-path-index-resolve mpi)))

(define default-root "compiled/root")

(define (main [config (hash)])
  ;; start the pkg-index back end
  (when (hash-ref config 'pkg-index (hash))
    (pkg-index:config (extract-pkg-index-config config))
    (thread pkg-index:go))

  ;; start the web front end
  (make-persistent-state '*config* (lambda () config))
  (void (make-reloadable-entry-point 'refresh-packages! (as-module-path packages.rkt)))
  (void (make-reloadable-entry-point 'rerender! (as-module-path site.rkt)))
  (void (make-reloadable-entry-point 'debug-information-dump! (as-module-path debug.rkt)))
  (start-service #:port (hash-ref config 'port (lambda () default-port))
                 #:ssl? (hash-ref config 'ssl? (lambda () #t))
                 #:root (hash-ref config 'root default-root)
                 #:reloadable? (hash-ref config 'reloadable? (lambda () (getenv "SITE_RELOADABLE")))
                 (make-reloadable-entry-point 'request-handler (as-module-path site.rkt))
                 (make-reloadable-entry-point 'on-continuation-expiry (as-module-path site.rkt))
                 (make-reloadable-entry-point 'extra-files-paths (as-module-path static.rkt))))
