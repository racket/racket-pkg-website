#lang racket/base

(provide (struct-out url-source)
         (struct-out git-source)
         package-source?
         string->package-source
         package-source->string
         github-source?
         github-user+repo)

(require racket/match)
(require net/url)
(require pkg/name)
(require pkg/private/repo-path)
(require (only-in racket/string string-join))

(struct url-source (url ;; String
                    )
        #:prefab)
(struct git-source (host ;; String
                    port ;; Nat or #f
                    repo ;; String (e.g. for github.com, "/user/repo")
                    branch ;; String
                    path ;; Relative URL String
                    )
        #:prefab)

(define (package-source? x)
  (or (url-source? x)
      (git-source? x)))

(define (string->package-source str)
  (define u (string->url str))
  (define-values (_name type) (package-source->name+type str #f))
  (cond
   [(memq type '(git github))
    (define-values (_type host port repo branch path)
      (if (equal? "github" (url-scheme u))
          (match (split-github-url u)
            [(list* user repo branch path)
             (values 'github "github.com" #f (string-append user "/" repo) branch path)]
            [(list user repo)
             (values 'github "github.com" #f (string-append user "/" repo) "master" '())]
            [_ (error 'string->package-source "Invalid github url: ~v" str)])
          (split-git-url u)))
    ;; TODO: clean this up in repo-path.rkt
    (git-source host
                port
                repo
                branch
                (string-join path "/"))]
   ;; [(and (member (url-scheme u) '("http" "https"))
   ;;       (equal? (url-host u) "github.com"))
   ;;  ;; ... parse the path, etc., and turn it into a git-source ...
   ;;  ]
   [else
    (url-source (url->string u))]))

(define (package-source->string s)
  (match s
    [(url-source u) u]
    [(git-source host port repo branch path)
     (url->string (url "git"
                       #f
                       host
                       port
                       #t
                       (url-path (path->url repo))
                       (if (string=? path "")
                           '()
                           (list (cons 'path path)))
                       branch))]))

(define (github-source? s)
  (unless (package-source? s) (error 'github-source? "Expected package-source: ~v" s))
  (match s
    [(git-source "github.com" #f (regexp "^([^/]+)/([^/]+)/*$") _ _) #t]
    [_ #f]))

(define (github-user+repo s)
  (unless (github-source? s) (error 'github-user+repo "Expected github package-source: ~v" s))
  (match (regexp-match "^([^/]+)/([^/]+)/*$" (git-source-repo s))
    [(list _ user repo) (values user repo)]
    [#f (error 'github-user+repo "Invalid github repo path: ~v" (git-source-repo s))]))

(module+ test
  (require rackunit)

  (check-equal? (string->package-source "https://github.com/user/repo")
                (url-source "https://github.com/user/repo"))
  (check-equal? (string->package-source "http://example.com/some/path/to/package.zip")
                (url-source "http://example.com/some/path/to/package.zip"))

  (check-equal? (string->package-source "git://github.com/user/repo")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "git://github.com/user/repo#master")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "git://github.com/user/repo?path=/subdir1/subdir2#master")
                (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=subdir1/subdir2#master")
                (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=%2fsubdir1%2fsubdir2#master")
                (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=subdir1%2fsubdir2#master")
                (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo#otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" ""))
  (check-equal? (string->package-source "git://github.com/user/repo?path=/subdir1/subdir2#otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=subdir1/subdir2#otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=%2fsubdir1%2fsubdir2#otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))
  (check-equal? (string->package-source "git://github.com/user/repo?path=subdir1%2fsubdir2#otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))

  (check-exn #px"Invalid github url"
             (lambda () (string->package-source "github://github.com/user/")))

  (check-equal? (string->package-source "github://github.com/user/repo")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/master")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/master/")
                (git-source "github.com" #f "user/repo" "master" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/master/subdir1/subdir2")
                (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
  (check-equal? (string->package-source "github://github.com/user/repo/otherbranch")
                (git-source "github.com" #f "user/repo" "otherbranch" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/otherbranch/")
                (git-source "github.com" #f "user/repo" "otherbranch" ""))
  (check-equal? (string->package-source "github://github.com/user/repo/otherbranch/subdir1/subdir2")
                (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))

  (check-equal? (package-source->string
                 (git-source "github.com" #f "user/repo" "master" "subdir1/subdir2"))
                "git://github.com/user/repo?path=subdir1%2Fsubdir2#master")
  (check-equal? (package-source->string
                 (git-source "github.com" #f "user/repo" "otherbranch" "subdir1/subdir2"))
                "git://github.com/user/repo?path=subdir1%2Fsubdir2#otherbranch")

  (define (roundtrip str)
    (check-equal? (package-source->string (string->package-source str)) str))

  (roundtrip "https://github.com/user/repo")
  (roundtrip "http://example.com/some/path/to/package.zip")
  (roundtrip "git://github.com/user/repo#master")
  (roundtrip "git://github.com/user/repo#otherbranch")

  (check-equal? (github-source? (string->package-source "github://github.com/user/repo")) #t)
  (check-equal? (github-source? (string->package-source "github://github.com/user/repo/master")) #t)
  (check-equal? (github-source? (string->package-source "github://github.com/user/repo/master/subdir")) #t)

  (check-equal? (github-source? (string->package-source "git://github.com/user/repo")) #t)
  (check-equal? (github-source? (string->package-source "git://github.com/user/repo#master")) #t)
  (check-equal? (github-source? (string->package-source "git://github.com/user/repo?path=subdir#master")) #t)

  (check-equal? (github-source? (string->package-source "git://github.com/user/repo/more")) #f)
  (check-equal? (github-source? (string->package-source "git://github.com/user/repo/more#master")) #f)
  (check-equal? (github-source? (string->package-source "git://github.com/user/repo/more?path=subdir#master")) #f)

  (check-equal? (github-source? (string->package-source "git://github.com/user")) #f)
  (check-equal? (github-source? (string->package-source "git://github.com/user#master")) #f)
  (check-equal? (github-source? (string->package-source "git://github.com/user?path=subdir#master")) #f)

  (check-equal? (github-source? (string->package-source "git://example.com/user/repo")) #f)
  (check-equal? (github-source? (string->package-source "git://example.com/user/repo#master")) #f)
  (check-equal? (github-source? (string->package-source "git://example.com/user/repo?path=subdir#master")) #f)

  (define (extract-user+repo str)
    (define-values (user repo) (github-user+repo (string->package-source str)))
    (list user repo))

  (check-equal? (extract-user+repo "github://github.com/user/repo") (list "user" "repo"))
  (check-equal? (extract-user+repo "github://github.com/user/repo/master") (list "user" "repo"))
  (check-equal? (extract-user+repo "github://github.com/user/repo/master/subdir") (list "user" "repo"))
  (check-equal? (extract-user+repo "git://github.com/user/repo") (list "user" "repo"))
  (check-equal? (extract-user+repo "git://github.com/user/repo#master") (list "user" "repo"))
  (check-equal? (extract-user+repo "git://github.com/user/repo?path=subdir#master") (list "user" "repo"))
  )
