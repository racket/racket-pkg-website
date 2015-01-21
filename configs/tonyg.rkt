#lang racket/base
;; Configuration for tonyg's development setup.
(require "../src/main.rkt")
(main (hash 'port 8444
            'reloadable? #t
            'package-index-url "http://localhost/~tonyg/pkg-catalog-static/pkgs-all.json.gz"
            'static-cached-directory (build-path (find-system-path 'home-dir)
                                                 "public_html/pkg-catalog-static")
            'static-cached-urlprefix "http://localhost/~tonyg/pkg-catalog-static/"
            'backend-baseurl "https://localhost:8445"
            ))
