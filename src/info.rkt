#lang info
(define deps
  ;; 6.7 was released with an incorrect version number
  '("web-server-lib"
    ("base" #:version "6.6.0.900")
    "reloadable"
    "aws"))
(define collection "pkg-website")
(define build-deps '("rackunit-lib"))
