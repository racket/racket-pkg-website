#lang racket/base

(require racket/set)
(require racket/match)
(require racket/format)
(require racket/date)
(require racket/port)
(require racket/string)
(require net/uri-codec)
(require web-server/servlet)
(require "bootstrap.rkt")
(require "html-utils.rkt")
(require "packages.rkt")

(define nav-index "Package Index")
(define nav-docs "Documentation")

(bootstrap-project-name
 `(a ((class "four columns logo")
      (href "http://www.racket-lang.org/"))
   (img ((src "http://pkgs.racket-lang.org/logo-and-text.png")
         (height "50")
         (alt "Racket Package Index")))))

(bootstrap-active-navigation nav-index)
(bootstrap-navigation `((,nav-index "/")
                        ("Documentation" "http://docs.racket-lang.org/")
                        ("Blog" "http://blog.racket-lang.org/")
                        ((div (span ((class "glyphicon glyphicon-download-alt")))
                              " Download")
                         "http://download.racket-lang.org/")))

(bootstrap-navbar-extension-fn
 (lambda ()
   `(
     (form ((class "navbar-form navbar-right")
            (role "form"))
           (div ((class "form-group"))
                (input ((type "text")
                        (placeholder "Email")
                        (class "form-control"))))
           (div ((class "form-group"))
                (input ((type "password")
                        (placeholder "Password")
                        (class "form-control"))))
           (button ((type "submit")
                    (class "btn btn-success"))
                   "Sign in"))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (request-handler named-url)
  (dispatch-rules
   [("") main-page]
   [("search") search-page]
   [("package" (string-arg)) package-page]
   ))

(module+ main
  (require "entrypoint.rkt")
  (start-service request-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (package-link package-name)
  (define package-name-str (~a package-name))
  `(a ((href ,(named-url package-page package-name-str))) ,package-name-str))

(define (doc-destruct doc)
  (match doc
    [(list _ n u) (values n u)]
    [(list _ n) (values n #f)]))

(define (doc-link doc)
  (define-values (docset-name docset-url) (doc-destruct doc))
  (if docset-url
      (buildhost-link docset-url docset-name)
      `(del ,docset-name)))

(define (tags-page-url tags)
  (format "~a?~a"
          (named-url search-page)
          (alist->form-urlencoded (list (cons 'tags (string-join tags))))))

(define (author-link author-name)
  `(a ((href ,(tags-page-url (list (format "author:~a" author-name))))) ,author-name))

(define (tag-link tag-name)
  `(a ((href ,(tags-page-url (list tag-name)))) ,tag-name))

(define (buildhost-link #:attributes [attributes '()] url-suffix label)
  `(a (,@attributes
       (href ,(format "http://pkg-build.racket-lang.org/~a" url-suffix))) ,label))

(define (authors-list authors)
  `(ul ((class "authors")) ,@(for/list ((author authors)) `(li ,(author-link author)))))

(define (package-links #:pretty? [pretty? #t] package-names)
  (if (and pretty? (null? (or package-names '())))
      `(span ((class "packages none")) "None")
      `(ul ((class "list-inline packages"))
        ,@(for/list ((p package-names)) `(li ,(package-link p))))))

(define (doc-links docs)
  `(ul ((class "list-inline doclinks"))
    ,@(for/list ((doc (or docs '()))) `(li ,(doc-link doc)))))

(define (tag-links tags)
  `(ul ((class "list-inline taglinks")) ,@(for/list ((tag (or tags '()))) `(li ,(tag-link tag)))))

(define (utc->string utc)
  (string-append (date->string (seconds->date utc #f) #t) " (UTC)"))

(define (package-summary-table package-names)
  `(table
    ((class "packages sortable"))
    (tr
     (th "Package")
     (th "Description")
     (th "Build"))
    ,@(maybe-splice (null? package-names)
                    `(tr (td ((colspan "3"))
                             (div ((class "alert alert-info"))
                                  "No packages found."))))
    ,@(for/list ((package-name package-names))
        (define pkg (package-detail package-name))
        `(tr
          (td (h2 ,(package-link package-name))
              ,(authors-list (@ pkg authors))
              ;; recently-updated?
              )
          (td (p ,(@ pkg description))
              ,@(maybe-splice
                 (pair? (@ pkg build docs))
                 `(div
                   (span ((class "doctags-label")) "Docs: ")
                   ,(doc-links (@ pkg build docs))))
              ,@(maybe-splice
                 (pair? (@ pkg tags))
                 `(div
                   (span ((class "doctags-label")) "Tags: ")
                   ,(tag-links (@ pkg tags)))))
          ,(cond
            [(@ pkg build failure-log)
             `(td ((class "build_red"))
               ,(buildhost-link (@ pkg build failure-log) "fails"))]
            [(and (@ pkg build success-log)
                  (@ pkg build dep-failure-log))
             `(td ((class "build_yellow"))
               ,(buildhost-link (@ pkg build success-log)
                                "succeeds")
               " with "
               ,(buildhost-link (@ pkg build dep-failure-log)
                                "dependency problems"))]
            [(@ pkg build success-log)
             `(td ((class "build_green"))
               ,(buildhost-link (@ pkg build success-log) "succeeds"))]
            [else
             `(td)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-page request)
  (bootstrap-response "Racket Package Index"
                      #:title-element ""
                      `(div ((class "jumbotron"))
                        (h1 "Racket Package Index")
                        (p "These are the packages available via the "
                           (a ((href "docs.racket-lang.org/pkg/getting-started.html"))
                              "Racket package system") ".")
                        (p "Simply run " (kbd "raco pkg install " (var "package-name"))
                           " to install a package.")
                        (form ((role "form")
                               (action ,(named-url search-page)))
                              (div ((class "form-group"))
                                   (input ((class "form-control")
                                           (type "text")
                                           (placeholder "Search packages")
                                           (name "q")
                                           (value "")
                                           (id "q"))))
                              ))
                      (package-summary-table (package-search "" '((main-distribution #f))))))

(define (package-page request package-name-str)
  (define package-name (string->symbol package-name-str))
  (define pkg (package-detail package-name))
  (define default-version (hash-ref (or (@ pkg versions) (hash)) 'default (lambda () #f)))
  (if (not pkg)
      (bootstrap-response #:code 404
                          #:message #"No such package"
                          "Package not found"
                          `(div "The package " (code ,package-name-str) " does not exist."))
      (bootstrap-response (~a package-name)
                          #:title-element ""
                          `(div ((class "jumbotron"))
                            (h1 ,(~a package-name))
                            (p ,(@ pkg description))
                            (div ,@(let ((docs (or (@ pkg build docs) '())))
                                     (match docs
                                       [(list)
                                        `()]
                                       [(list doc)
                                        (define-values (n u) (doc-destruct doc))
                                        (list (buildhost-link
                                               #:attributes `((class "btn btn-success btn-lg"))
                                               u
                                               "Documentation"))]
                                       [_
                                        `((button ((class "btn btn-success btn-lg dropdown-toggle")
                                                   (data-toggle "dropdown"))
                                                  "Documentation "
                                                  (span ((class "caret"))))
                                          (ul ((class "dropdown-menu")
                                               (role "menu"))
                                              ,@(for/list ((doc docs)) `(li ,(doc-link doc)))))]))

                                 ;; Heuristic guess as to whether we should present a "browse"
                                 ;; link or a "download" link.
                                 " "
                                 ,(if (equal? (@ default-version source)
                                             (@ default-version source_url))
                                      `(a ((class "btn btn-default btn-lg")
                                           (href ,(@ default-version source_url)))
                                        (span ((class "glyphicon glyphicon-download")))
                                        " Snapshot")
                                      `(a ((class "btn btn-default btn-lg")
                                           (href ,(@ default-version source_url)))
                                        (span ((class "glyphicon glyphicon-link")))
                                        " Code"))
                                 ))

                          (if (@ pkg checksum-error)
                              `(div ((class "alert alert-danger")
                                     (role "alert"))
                                (span ((class "label label-danger"))
                                      "Checksum error")
                                " The package checksum does not match"
                                " the package source code.")
                              "")

                          `(table ((class "package-details"))
                            (tr (th "Authors")
                                (td ,(authors-list (@ pkg authors))))
                            (tr (th "Documentation")
                                (td ,(doc-links (@ pkg build docs))))
                            (tr (th "Tags")
                                (td ,(tag-links (@ pkg tags))))
                            (tr (th "Last updated")
                                (td ,(utc->string (@ pkg last-updated))))
                            (tr (th "Ring")
                                (td ,(~a (@ pkg ring))))
                            (tr (th "Conflicts")
                                (td ,(package-links (@ pkg conflicts))))
                            (tr (th "Dependencies")
                                (td ,(package-links (@ pkg dependencies))))
                            (tr (th "Most recent build results")
                                (td (ul ((class "build-results"))
                                     ,@(maybe-splice
                                        (@ pkg build success-log)
                                        `(li "Compiled successfully: "
                                          ,(buildhost-link (@ pkg build success-log) "transcript")))
                                     ,@(maybe-splice
                                        (@ pkg build failure-log)
                                        `(li "Compiled unsuccessfully: "
                                          ,(buildhost-link (@ pkg build failure-log) "transcript")))
                                     ,@(maybe-splice
                                        (@ pkg build conflicts-log)
                                        `(li "Conflicts: "
                                          ,(buildhost-link (@ pkg build conflicts-log) "details")))
                                     ,@(maybe-splice
                                        (@ pkg build dep-failure-log)
                                        `(li "Dependency problems: "
                                          ,(buildhost-link (@ pkg build dep-failure-log) "details")))
                                     )))
                            (tr (th "Modules")
                                (td (ul ((class "module-list"))
                                        ,@(for/list ((mod (@ pkg modules)))
                                            (match-define (list kind path) mod)
                                            `(li ((class ,kind)) ,path)))))
                            ,@(let* ((vs (or (@ pkg versions) (hash)))
                                     (empty-checksum "9f098dddde7f217879070816090c1e8e74d49432")
                                     (vs (for/hash (((k v) (in-hash vs))
                                                    #:when (not (equal? (@ v checksum)
                                                                        empty-checksum)))
                                           (values k v))))
                                (maybe-splice
                                 (not (hash-empty? vs))
                                 `(tr (th "Versions")
                                   (td (table ((class "package-versions"))
                                              (tr (th "Version")
                                                  (th "Source")
                                                  (th "Checksum"))
                                              ,@(for/list
                                                    (((version-sym v) (in-hash vs)))
                                                  `(tr
                                                    (td ,(~a version-sym))
                                                    (td (a ((href ,(@ v source_url)))
                                                           ,(@ v source)))
                                                    (td ,(@ v checksum)))))))))
                            (tr (th "Last checked")
                                (td ,(utc->string (@ pkg last-checked))))
                            (tr (th "Last edited")
                                (td ,(utc->string (@ pkg last-edit))))
                            ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-page request)
  (define-form-bindings request ([search-text q ""]
                                 [tags-input tags ""]))
  (define tags (for/list ((t (string-split tags-input)))
                 (match t
                   [(pregexp #px"!(.*)" (list _ tag)) (list (string->symbol tag) #f)]
                   [tag (list (string->symbol tag) #t)])))
  (bootstrap-response "Search Racket Package Index"
                      `(form ((class "form-horizontal")
                              (role "form"))
                        (div ((class "form-group"))
                             (label ((class "col-sm-2 control-label")
                                     (for "q")) "Search terms")
                             (div ((class "col-sm-10"))
                                  (input ((class "form-control")
                                          (type "text")
                                          (placeholder "Enter free-form text to match here")
                                          (name "q")
                                          (value ,search-text)
                                          (id "q")))))
                        (div ((class "form-group"))
                             (label ((class "col-sm-2 control-label")
                                     (for "tags")) "Tags")
                             (div ((class "col-sm-10"))
                                  (input ((class "form-control")
                                          (type "text")
                                          (placeholder "tag1 tag2 tag3 ...")
                                          (name "tags")
                                          (value ,tags-input)
                                          (id "tags")))))
                        (div ((class "form-group"))
                             (div ((class "col-sm-offset-2 col-sm-10"))
                                  (button ((type "submit")
                                           (class "btn btn-primary"))
                                          (span ((class "glyphicon glyphicon-search")))
                                          " Search")))
                        (div ((class "search-results"))
                             ,@(maybe-splice
                                (or (pair? tags) (not (equal? search-text "")))
                                (package-summary-table (package-search search-text tags)))))))
