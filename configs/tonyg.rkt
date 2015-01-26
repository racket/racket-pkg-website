#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")
(main (hash 'port 8444
            'reloadable? #t
            'package-index-url "http://localhost/~tonyg/pkg-catalog-static/pkgs-all.json.gz"
            'static-generated-directory (build-path (find-system-path 'home-dir)
                                                    "public_html/pkg-catalog-static")
            'static-urlprefix "http://localhost/~tonyg/pkg-catalog-static"
            'dynamic-urlprefix "https://localhost:8444"
            'backend-baseurl "https://localhost:8445"
            ))
