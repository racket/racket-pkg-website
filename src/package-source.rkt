#lang racket/base
;; Package Source URLs: their various kinds

;; Here we're only interested in remote URLs -- http, https, git and
;; github. Local file and directory package sources are not to be
;; accepted.

(provide parse-package-source
         parsed-package-source-human-url
         parsed-package-source-human-tree-url
         unparse-package-source
         package-source->human-tree-url
         (struct-out parsed-package-source)
         (struct-out simple-url-source)
         (struct-out git-source))

(require racket/match)
(require (only-in racket/string string-join string-split))
(require net/url)
(require pkg/private/repo-path)
(require pkg/name)

;; A ParsedPackageSource is one of
;; -- (simple-url-source    String (Option String) (Option Symbol))
;; -- (git-source           String (Option String) Symbol Symbol String (Option Number) String String String)
(struct parsed-package-source (url-string inferred-name type) #:prefab)
(struct simple-url-source parsed-package-source () #:prefab)
(struct git-source parsed-package-source (transport host port repo commit path) #:prefab)

;; String -> (Values (Option ParsedPackageSource) (Listof String))
;; The second result is a list of complaints about the passed-in package source URL string.
(define (parse-package-source p)
  (define complaints '())
  (define (complain message) (set! complaints (append complaints (list message))))

  (define-values (name type)
    (with-handlers ([void (lambda (e) (values #f #f))])
      (package-source->name+type p #f
                                 #:complain (lambda (_p message) (complain message))
                                 #:must-infer-name? #t)))

  (define parsed-source
    (match type
      [#f
       (complain "couldn't guess package source type")
       (simple-url-source p name type)]

      ;; ['name] -- only ever returned if it was passed in as second arg to package-source->name+type
      ;; ['clone] -- only returned if passed in, like 'name
      ;; ['link] -- only returned if #:link-dirs? given, except if it's a file:// url with a type query parameter of link
      ;; ['static-link] -- only returned if it's a file:// url with a type query parameter of static-link

      [(or 'file 'dir)
       (complain "local file or directory package source types are not permitted")
       #f]

      [(or 'git 'github)
       (with-handlers ([void (lambda (e) (simple-url-source p name type))])
         (define u (string->url p))
         (define-values (transport host port repo commit path) (split-git-or-hub-url u #:type type))
         (git-source p name type
                     (if (eq? type 'github) 'git transport)
                     host
                     port
                     repo
                     commit
                     (string-join path "/")))]

      [(or 'file-url 'dir-url)
       (with-handlers ([void (lambda (e) (simple-url-source p name type))])
         (define u (string->url p)) ;; just to check it *can* be parsed as a URL
         (simple-url-source p name type))]))

  (values parsed-source complaints))

(define (parsed-package-source-human-url s)
  (match s
    [(git-source u _ type _ host port repo _ _)
     (real-git-url (string->url u) host port repo #:type type)]
    [(simple-url-source u _ _)
     u]))

(define (parsed-package-source-human-tree-url s)
  (match s
    [(git-source _ _ _ _ "github.com" _ repo commit path)
     (url->string
      (url "https"
           #f
           "github.com"
           #f
           #t
           (append (->url-path (regexp-replace #rx"[.]git$" repo ""))
                   (list (path/param "tree" '())
                         (path/param commit '()))
                   (->url-path path))
           '()
           #f))]
    [_ (parsed-package-source-human-url s)]))

(define (unparse-package-source s)
  (match s
    [(git-source _ _ _ transport host port repo commit path)
     (url->string
      (url (symbol->string transport)
           #f
           host
           port
           #t
           (->url-path repo)
           (match path ["" '()] [_ (list (cons 'path path))])
           (match commit [#f #f] ["master" #f] [_ commit])))]
    [(simple-url-source u _ _)
     u]))

(define (->url-path str)
  (map (lambda (s) (path/param s '())) (string-split str "/")))

(define (package-source->human-tree-url source)
  (define-values (parsed complaints) (parse-package-source source))
  (if parsed (parsed-package-source-human-tree-url parsed) source))

(module+ test
  (define test-data
    (list
     "http://github.com/test/repo.git"
     "https://github.com/test/repo.git"
     "http://leastfixedpoint.com:55555/foo/bar.git?path=zot/quux/baz#release"
     "git://leastfixedpoint.com:55555/foo/bar.git?path=zot/quux/baz#release"
     "github://github.com/foo/bar/master"
     "github://github.com/foo/bar.git/master"
     "github://github.com/foo/bar.git/release/zot/quux/baz"
     "github://github.com/foo/bar/release/zot/quux/baz"
     "github://github.com/tonyg/racket-ansi.git/master"
     "github://github.com/tonyg/racket-ansi/master"
     ))

  (require rackunit)
  (require racket/set)

  (define seen-types
    (for/set ((p test-data))
      (define-values (name type) (package-source->name+type p #f))
      type))

  (define expected-types
    (set 'git 'github 'file-url 'dir-url))

  (check-equal? (set) (set-subtract seen-types expected-types))
  (check-equal? (set) (set-subtract expected-types seen-types))

  (for ((p test-data))
    (define-values (parsed-source complaints) (parse-package-source p))
    (printf "~v:\n - ~v\n - ~v\n - ~v\n"
            p
            parsed-source
            complaints
            (unparse-package-source parsed-source))
    (void)
    )
  )
