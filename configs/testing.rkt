#lang racket/base
;; Configuration for development setup.
(require racket/runtime-path
         racket/format
         racket/path
         "../src/main.rkt")

(define-runtime-path data-file "../pkgs-all.json.gz")

(main (hash 'port 8444
            'reloadable? #t
            'package-index-url (~a "file://" (simple-form-path data-file))
            'static-output-type 'file
            'static-content-target-directory #f
            'dynamic-urlprefix "https://localhost:8444"
            'backend-baseurl "https://localhost:8445"
            'disable-cache? #t
            'pkg-index-generated-directory "/dummy/path/of/pkg-index"
            'user-directory (build-path (find-system-path 'addon-dir) "pkg-website-test-users")))
