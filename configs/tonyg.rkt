#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")

(define pkg-index-generated-directory
  (build-path (find-system-path 'home-dir) "src/pkg-index/official/static-gen"))

(main (hash 'port 8444
            'ssl? #f
            'reloadable? #t
            'package-index-url (format "file://~a/pkgs-all.json.gz" pkg-index-generated-directory)
            'user-directory (build-path (find-system-path 'home-dir)
                                        "src/pkg-index/official/root/users.new")
            'email-sender-address "tonyg@racket-lang.org"
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
            'dynamic-urlprefix "http://localhost:8444"
            'backend-baseurl "http://localhost:8445"
            'pkg-index-generated-directory pkg-index-generated-directory

            'pkg-index (hash
                        's3-bucket #f
                        'redirect-to-static-proc
                        (lambda (req)
                          (log-info "Would redirect to static version of ~a"
                                    (url->string (request-uri req)))
                          (redirect-to "http://localhost/~tonyg/pkg-catalog-static/FIXME"))
                        'port 8445
                        'ssl? #f
                        'atom-package-url-format-string
                        "https://localhost/~~tonyg/pkg-catalog-static/package/~a")
            ))
