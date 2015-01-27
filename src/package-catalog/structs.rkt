#lang racket/base

(provide (struct-out package)
         package-author
         serialize-package
         deserialize-package

         (struct-out computed-info)
         serialize-computed-info
         deserialize-computed-info

         (struct-out github-info)
         serialize-github-info
         deserialize-github-info)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-split string-join))
(require "source.rkt")

;; A Time here is milliseconds-since-epoch - e.g. a result from
;; (current-inexact-milliseconds).

(define package-format-version 0) ;; IMPORTANT - UPDATE THIS AND CHANGE SERIALIZATION
                                  ;; WHENEVER THE STRUCT DEFINITION FOR package CHANGES
(struct package (name ;; String
                 source ;; PackageSource
                 description ;; String
                 tags ;; (Listof String)
                 authors ;; (Listof String)
                 versions ;; (HashTable String PackageSource)
                 ring ;; Nat
                 last-edit ;; Time
                 )
        #:prefab)

(define (package-author p)
  (string-join (package-authors p) " "))

(define computed-info-format-version 0) ;; IMPORTANT - UPDATE THIS AND CHANGE SERIALIZATION
                                        ;; WHENEVER THE STRUCT DEFINITION FOR computed-info CHANGES
(struct computed-info (package-name ;; String
                       last-updated ;; Time, most recent change to package source
                       last-checked ;; Time, when package source was most recently checked
                       checksums ;; (HashTable String String), including "default" key
                       checksum-errors ;; (HashTable String String), including "default" key
                       github-info ;; GithubInfo or #f
                       declared-conflicts ;; (Setof String), package names
                       modules ;; (Listof ModulePath)
                       dependencies ;; (Listof String), package names
                       )
        #:prefab)

(define github-info-format-version 0) ;; IMPORTANT - UPDATE THIS AND CHANGE SERIALIZATION
                                      ;; WHENEVER THE STRUCT DEFINITION FOR github-info CHANGES
(struct github-info (readme-exists? ;; Boolean
                     )
        #:prefab)

;;---------------------------------------------------------------------------
;; This is the kind of stupid repetitive code our struct system should
;; allow us to automate.

(define (serialize-package p)
  (match-define (package name source description tags authors versions ring last-edit) p)
  (list 'package package-format-version
        (hash 'name name
              'source (package-source->string source)
              'description description
              'tags tags
              'authors authors
              'versions (for/hash [((version source) (in-hash versions))]
                          (values version (package-source->string source)))
              'ring ring
              'last-edit last-edit)))

(define (deserialize-package p)
  (match p
    [(? hash?)
     (package (hash-ref p 'name)
              (string->package-source (hash-ref p 'source))
              (hash-ref p 'description "")
              (hash-ref p 'tags '())
              (string-split (hash-ref p 'author ""))
              (for/hash [((version fields) (in-hash (hash-ref p 'versions (hash))))]
                (values version (string->package-source (hash-ref fields 'source))))
              (hash-ref p 'ring 2)
              (hash-ref p 'last-edit 0))]
    [(list 'package 0
           (hash-table ['name (? string? name)]
                       ['source (? string? source0)]
                       ['description (? string? description)]
                       ['tags (and (list (? string?) ...) tags)]
                       ['authors (and (list (? string?) ...) authors)]
                       ['versions versions0]
                       ['ring (? number? ring)]
                       ['last-edit (? number? last-edit)]))
     (define source (string->package-source source0))
     (define versions (for/hash [((version source) (in-hash versions0))]
                        (values version (string->package-source source))))
     (package name source description tags authors versions ring last-edit)]
    [_
     (error 'deserialize-package "Unrecognized serialized package: ~v" p)]))

(define (serialize-computed-info ci)
  (match-define (computed-info package-name
                               last-updated
                               last-checked
                               checksums
                               checksum-errors
                               github-info
                               declared-conflicts
                               modules
                               dependencies)
                ci)
  (list 'computed-info computed-info-format-version
        (hash 'package-name package-name
              'last-updated last-updated
              'last-checked last-checked
              'checksums checksums
              'checksum-errors checksum-errors
              'github-info (and github-info (serialize-github-info github-info))
              'declared-conflicts declared-conflicts
              'modules modules
              'dependencies dependencies)))

(define (deserialize-computed-info ci)
  (match ci
    [(? hash?)
     (computed-info (hash-ref ci 'name)
                    (hash-ref ci 'last-updated 0)
                    (hash-ref ci 'last-checked 0)
                    (let ((cs (for/hash [((v fs) (in-hash (hash-ref ci 'versions (hash))))
                                         #:when (hash-has-key? fs 'checksum)]
                                (values v (hash-ref fs 'checksum)))))
                      (if (hash-has-key? ci 'checksum)
                          (hash-set cs "default" (hash-ref ci 'checksum))
                          cs))
                    (let ((err (hash-ref ci 'checksum-error #f)))
                      (if err
                          (hash "default" err)
                          (hash)))
                    #f
                    (list->set (hash-ref ci 'conflicts '()))
                    (hash-ref ci 'modules '())
                    (hash-ref ci 'dependencies '()))]
    [(list 'computed-info 0
           (hash-table ['package-name (? string? package-name)]
                       ['last-updated (? number? last-updated)]
                       ['last-checked (? number? last-checked)]
                       ['checksums checksums]
                       ['checksum-errors checksum-errors]
                       ['github-info github-info0]
                       ['declared-conflicts declared-conflicts]
                       ['modules (and (list (? module-path?) ...) modules)]
                       ['dependencies (and (list (? string?) ...) dependencies)]))
     (define github-info (and github-info0 (deserialize-github-info github-info0)))
     (computed-info package-name
                    last-updated
                    last-checked
                    checksums
                    checksum-errors
                    github-info
                    declared-conflicts
                    modules
                    dependencies)]
    [_
     (error 'deserialize-computed-info "Unrecognized serialized computed-info: ~v" ci)]))

(define (serialize-github-info gi)
  (match-define (github-info readme-exists?) gi)
  (list 'github-info github-info-format-version
        (hash 'readme-exists? readme-exists?)))

(define (deserialize-github-info gi)
  (match gi
    [(list 'github-info 0
           (hash-table ['readme-exists? readme-exists?]))
     (github-info readme-exists?)]
    [_
     (error 'deserialize-github-info "Unrecognized serialized github-info: ~v" gi)]))

;;---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define empty-zip "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
  (define empty-zip-checksum "9f098dddde7f217879070816090c1e8e74d49432")

  (define xrepl-lib-hash
    #hash((name . "xrepl-lib")
          (source . "git://github.com/racket/xrepl/?path=xrepl-lib")
          (author . "eli@racket-lang.org")
          (last-updated . 1417912075)
          (last-edit . 1417659583)
          (last-checked . 1421095102)
          (versions
           . #hash(("5.3.5"
                    . #hash((source
                             . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
                            (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))
                   ("5.3.4"
                    . #hash((source
                             . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
                            (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))
                   ("5.3.6"
                    . #hash((source
                             . "http://racket-packages.s3-us-west-2.amazonaws.com/pkgs/empty.zip")
                            (checksum . "9f098dddde7f217879070816090c1e8e74d49432")))))
          (tags . ("main-distribution"))
          (checksum-error . #f)
          (ring . 0)
          (checksum . "c88f8430b054d8a207a95acb0d1de0efece33510")
          (description . "implementation (no documentation) part of \"xrepl\"")
          (modules . ((lib "xrepl/saved-values.rkt")
                      (lib "xrepl/xrepl.rkt")
                      (lib "xrepl/main.rkt")))
          (dependencies . ("base" "readline-lib" "scribble-text-lib"))
          (conflicts . ())))

  (define xrepl-lib (package "xrepl-lib"
                             (git-source "github.com"
                                         #f
                                         "racket/xrepl"
                                         "master"
                                         "xrepl-lib")
                             "implementation (no documentation) part of \"xrepl\""
                             '("main-distribution")
                             '("eli@racket-lang.org")
                             (hash "5.3.4" (url-source empty-zip)
                                   "5.3.5" (url-source empty-zip)
                                   "5.3.6" (url-source empty-zip))
                             0
                             1417659583))

  (define xrepl-lib-info (computed-info "xrepl-lib"
                                        1417912075
                                        1421095102
                                        (hash "5.3.4" empty-zip-checksum
                                              "5.3.5" empty-zip-checksum
                                              "5.3.6" empty-zip-checksum
                                              "default" "c88f8430b054d8a207a95acb0d1de0efece33510")
                                        (hash)
                                        #f
                                        (set)
                                        (list '(lib "xrepl/saved-values.rkt")
                                              '(lib "xrepl/xrepl.rkt")
                                              '(lib "xrepl/main.rkt"))
                                        (list "base" "readline-lib" "scribble-text-lib")))

  (check-equal? (deserialize-package xrepl-lib-hash) xrepl-lib)
  (check-equal? (serialize-package xrepl-lib)
                (list 'package package-format-version
                      (hash 'name "xrepl-lib"
                            'source "git://github.com/racket/xrepl?path=xrepl-lib#master"
                            'tags '("main-distribution")
                            'description "implementation (no documentation) part of \"xrepl\""
                            'last-edit 1417659583
                            'versions (hash "5.3.4" empty-zip
                                            "5.3.5" empty-zip
                                            "5.3.6" empty-zip)
                            'ring 0
                            'authors '("eli@racket-lang.org"))))
  (check-equal? (deserialize-package (serialize-package xrepl-lib)) xrepl-lib)

  (check-equal? (deserialize-computed-info xrepl-lib-hash) xrepl-lib-info)
  (check-equal? (deserialize-computed-info (serialize-computed-info xrepl-lib-info)) xrepl-lib-info)
  )
