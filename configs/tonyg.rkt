#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")
(main (hash 'port 8444
            'reloadable? #t
            'package-index-url "https://localhost:8444/pkgs-all.json.gz"
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Either:
            'static-output-type 'file
            'static-content-target-directory (build-path (find-system-path 'home-dir)
                                                         "public_html/pkg-catalog-static")
            ;; Or:
            ;; 'static-output-type 'aws-s3
            ;; 'aws-s3-bucket+path "pkgs.leastfixedpoint.com/"
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            'static-urlprefix "https://localhost/~tonyg/pkg-catalog-static"
            'dynamic-urlprefix "https://localhost:8444"
            'backend-baseurl "https://localhost:8445"
            'pkg-index-generated-directory (build-path (find-system-path 'home-dir)
                                                       "public_html/pkg-index-static")
            ))
