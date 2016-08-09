#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")
(main (hash 'port 8444
            'reloadable? #t
            'package-index-url "file:///home/tonyg/public_html/pkg-index-static/pkgs-all.json.gz"
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Either:
            ;;
            'static-output-type 'file
            'static-content-target-directory (build-path (find-system-path 'home-dir)
                                                         "public_html/pkg-catalog-static")
            'static-urlprefix "https://localhost/~tonyg/pkg-catalog-static"
            ;;
            ;; Or:
            ;;
            ;; 'static-output-type 'aws-s3
            ;; 'aws-s3-bucket+path "pkgs.leastfixedpoint.com/"
            ;; ;; These two should be set to an HTTPS proxy (e.g. nginx) proxying to S3,
            ;; ;; http://pkgs.leastfixedpoint.com.s3-website-us-east-1.amazonaws.com
            ;; 'static-urlprefix "https://localhost:8446"
            ;; 'dynamic-static-urlprefix "https://localhost:8446"
            ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            'dynamic-urlprefix "https://localhost:8444"
            'backend-baseurl "https://localhost:8445"
            'pkg-index-generated-directory (build-path (find-system-path 'home-dir)
                                                       "public_html/pkg-index-static")
            ))
