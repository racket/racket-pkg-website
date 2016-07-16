#lang racket/base
;; Configuration for pkgd
(require "../src/main.rkt")
(main (hash 'port 8444
            'backend-baseurl "https://localhost:9004"
            'package-index-url "file:///home/ubuntu/local/new-plt/pkgs/plt-services/meta/pkg-index/official/static-gen/pkgs-all.json.gz"
            'static-output-type 'aws-s3
            'aws-s3-bucket+path "pkgn.racket-lang.org/"
            'dynamic-urlprefix "https://pkgd.racket-lang.org/pkgn"
            'static-urlprefix "https://pkgn.racket-lang.org"
            'dynamic-static-urlprefix "https://pkgn.racket-lang.org"
            'pkg-index-generated-directory "/home/ubuntu/local/new-plt/pkgs/plt-services/meta/pkg-index/official/static-gen/"
            ))
