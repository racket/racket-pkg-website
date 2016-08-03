#lang racket/base
;; Default configuration; should be suitable for live deployment.
(require "../src/main.rkt")
(define var (getenv "PKGSERVER_DATADIR"))
(main (hash 'port 8444
            'reloadable? #t
            'var-path var
            'package-index-url
              (format "file://~a/public_html/pkg-index-static/pkgs-all.json.gz" var)
            'backend-baseurl "https://localhost:9004"
            'pkg-index-generated-directory (build-path var "public_html/pkg-index-static")
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; To configure a split, S3-based setup, comment out the following lines:
            ;;
            ;; 'static-output-type 'file
            ;; 'static-content-target-directory (build-path var "public_html/pkg-catalog-static")
            ;; 'static-urlprefix ""
            ;; 'dynamic-urlprefix "/catalog"
            ;;
            ;; ... and uncomment and adjust these instead:
            ;;

            'static-output-type 'aws-s3
            'aws-s3-bucket+path "pkgn.racket-lang.org/"
            'static-urlprefix "https://pkgn.racket-lang.org"
            'dynamic-urlprefix "https://pkgd.racket-lang.org/pkgn"
            'dynamic-static-urlprefix "https://pkgn.racket-lang.org"

            ;; 'static-output-type 'aws-s3
            ;; 'aws-s3-bucket+path "pkgs.leastfixedpoint.com/"
            ;; 'static-urlprefix "http://pkgs.leastfixedpoint.com.s3-website-us-east-1.amazonaws.com"
            ;; 'dynamic-urlprefix "https://localhost:8444"

            ;;
            ;; Make sure to *include* a slash at the end of
            ;; aws-s3-bucket+path, and to *exclude* a slash from the
            ;; end of both static-urlprefix and dynamic-urlprefix.
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ))
