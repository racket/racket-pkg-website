#lang racket/base

(require racket/set)
(require racket/match)
(require racket/format)
(require racket/date)
(require racket/port)
(require racket/string)
(require net/uri-codec)
(require web-server/servlet)
(require web-server/http/id-cookie)
(require web-server/http/cookie-parse)
(require "bootstrap.rkt")
(require "html-utils.rkt")
(require "packages.rkt")
(require "sessions.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (request-handler named-url)
  (dispatch-rules
   [("") main-page]
   [("search") search-page]
   [("package" (string-arg)) package-page]
   [("package" (string-arg) "edit") edit-package-page]
   ))

(module+ main
  (require "entrypoint.rkt")
  (start-service request-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (authentication-wrap #:request request body ...)
  (authentication-wrap* request (lambda () body ...)))

(define COOKIE "pltsession")

(define current-session (make-parameter #f))
(define (current-email)
  (define s (current-session))
  (and s (session-email s)))

(define (authentication-wrap* request body)
  (define original-session-cookie
    (findf (lambda (c) (equal? (client-cookie-name c) COOKIE))
           (request-cookies request)))
  (define original-session-key
    (and original-session-cookie (client-cookie-value original-session-cookie)))
  (log-info "Session key from cookie: ~a" original-session-key)
  (let redo ((session-key original-session-key
              ))
    (define session (lookup-session/touch! session-key))
    (log-info "session: ~a" session)
    (send/suspend/dispatch
     (lambda (embed-url)
       (parameterize ((bootstrap-navbar-extension
                       (cond
                        [(not session)
                         `((a ((class "btn btn-default navbar-btn navbar-right")
                               (href ,(embed-url (lambda (req) (redo (register-page))))))
                              "Register")
                           (a ((class "btn btn-success navbar-btn navbar-right")
                               (href ,(embed-url (lambda (req) (redo (login-page))))))
                              "Sign in"))]
                        [else
                         `((ul ((class "nav navbar-nav navbar-right"))
                               (li ((class "dropdown"))
                                   (a ((class "dropdown-toggle")
                                       (data-toggle "dropdown"))
                                      ,(session-email session)
                                      " "
                                      (span ((class "caret"))))
                                   (ul ((class "dropdown-menu") (role "menu"))
                                       (li "foo")
                                       (li "bar")))))]))
                      (current-session session)
                      (bootstrap-cookies
                       (if session
                           (list (make-cookie COOKIE session-key
                                              ;; TODO #:secure? #t
                                              ))
                           (list (make-cookie COOKIE ""
                                              #:expires "Thu, 01 Jan 1970 00:00:00 GMT")))))
         (body))))))

(define (authenticate-with-server! email password code)
  (define auth-url
    (string->url
     (format "https://pkgd.racket-lang.org/jsonp/authenticate?~a"
             (alist->form-urlencoded (list (cons 'callback "x")
                                           (cons 'email email)
                                           (cons 'passwd password)
                                           (cons 'code code))))))
  (define-values (body-port response-headers) (get-pure-port/headers auth-url))
  (match-define (pregexp #px"^x\\((.*)\\);$" (list _ json)) (port->string body-port))
  (log-info "JSON: ~a" json)
  json)

(define (login-page [error-message #f])
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response "Login"
                         `(form ((class "form-horizontal")
                                 (method "post")
                                 (action ,(embed-url process-login-credentials))
                                 (role "form"))
                           (div ((class "form-group"))
                                (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                        (for "email")) "Email address:")
                                (div ((class "col-sm-5"))
                                     (input ((class "form-control")
                                             (type "email")
                                             (name "email")
                                             (value "")
                                             (id "email")))))
                           (div ((class "form-group"))
                                (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                        (for "password")) "Password:")
                                (div ((class "col-sm-5"))
                                     (input ((class "form-control")
                                             (type "password")
                                             (name "password")
                                             (value "")
                                             (id "password")))))
                           (div ((class "form-group"))
                                (div ((class "col-sm-offset-4 col-sm-5"))
                                     (a ((href ,(embed-url (lambda (req) (register-page)))))
                                        "Need to reset your password?")
                             (div ((class "form-group"))
                                  (div ((class "col-sm-offset-4 col-sm-5"))
                                       (button ((type "submit")
                                                (class "btn btn-primary"))
                                               "Log in"))))
                           ))))))

(define (process-login-credentials request)
  (define-form-bindings request (email password))
  (match (authenticate-with-server! email password "")
    ["wrong-code"
     (login-page "Something went awry; please try again.")]
    [(or "emailed" #f)
     (summarise-code-emailing "Incorrect password, or nonexistent user." email)]
    [else
     (create-session! email password)]))

(define (register-page #:email [email ""]
                       #:code [code ""]
                       #:error-message [error-message #f])
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response "Register/Reset Account"
                         #:title-element ""
                         `(div
                           (h1 "Got a registration or reset code?")
                           (p "Great! Enter it below, with your chosen password, to log in.")
                           (form ((class "form-horizontal")
                                  (method "post")
                                  (action ,(embed-url apply-account-code))
                                  (role "form"))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "email")) "Email address:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "email")
                                                   (name "email")
                                                   (value ,email)
                                                   (id "email")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "code")) "Code:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "text")
                                                   (name "code")
                                                   (value ,code)
                                                   (id "code")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "password")) "Password:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "password")
                                                   (name "password")
                                                   (value "")
                                                   (id "password")))))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "password")) "Confirm password:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "password")
                                                   (name "confirm_password")
                                                   (value "")
                                                   (id "confirm_password")))))
                                 ,@(maybe-splice
                                    error-message
                                    `(div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (div ((class "alert alert-danger"))
                                                (p ,error-message)))))
                                 (div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (button ((type "submit")
                                                    (class "btn btn-primary"))
                                                   "Continue")))))
                         `(div
                           (h1 "Need a code?")
                           (p "Enter your email address below, and we'll send you one.")
                           (form ((class "form-horizontal")
                                  (method "post")
                                  (action ,(embed-url notify-of-emailing))
                                  (role "form"))
                                 (div ((class "form-group"))
                                      (label ((class "col-sm-offset-2 col-sm-2 control-label")
                                              (for "email")) "Email address:")
                                      (div ((class "col-sm-5"))
                                           (input ((class "form-control")
                                                   (type "email")
                                                   (name "email_for_code")
                                                   (value "")
                                                   (id "email_for_code")))))
                                 (div ((class "form-group"))
                                      (div ((class "col-sm-offset-4 col-sm-5"))
                                           (button ((type "submit")
                                                    (class "btn btn-primary"))
                                                   "Email me a code")))))))))

(define (apply-account-code request)
  (define-form-bindings request (email code password confirm_password))
  (define (retry msg)
    (register-page #:email email
                   #:code code
                   #:error-message msg))
  (cond
   [(equal? (string-trim email) "")
    (retry "Please enter your email address.")]
   [(equal? (string-trim code) "")
    (retry "Please enter the code you received in your email.")]
   [(not (equal? password confirm_password))
    (retry "Please make sure the two password fields match.")]
   [(equal? (string-trim password) "")
    (retry "Please enter a password.")]
   [else
    (match (authenticate-with-server! email password code)
      ["wrong-code"
       (retry "The code you entered was incorrect. Please try again.")]
      [(or "emailed" #f)
       (retry "Something went awry; you have been emailed another code. Please check your email.")]
      [else
       ;; The email and password combo we have been given is good to go.
       ;; Set a cookie and consider ourselves logged in.
       (create-session! email password)])]))

(define (notify-of-emailing request)
  (define-form-bindings request (email_for_code))
  (authenticate-with-server! email_for_code "" "") ;; TODO check result?
  (summarise-code-emailing "Account registration/reset code emailed" email_for_code))

(define (summarise-code-emailing reason email)
  (send/suspend/dispatch
   (lambda (embed-url)
     (bootstrap-response reason
                         `(p
                           "We've emailed an account registration/reset code to "
                           (code ,email) ". Please check your email and then click "
                           "the button to continue:")
                         `(a ((class "btn btn-primary")
                              (href ,(embed-url (lambda (req) (register-page)))))
                           "Enter your code")))))

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
  (authentication-wrap
   #:request request
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
                       (package-summary-table (package-search "" '((main-distribution #f)))))))

(define (package-page request package-name-str)
  (authentication-wrap
   #:request request
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

                                  ,@(maybe-splice
                                     (member (current-email) (or (@ pkg authors) '()))
                                     " "
                                     `(a ((class "btn btn-info btn-lg")
                                          (href ,(named-url edit-package-page package-name-str)))
                                       (span ((class "glyphicon glyphicon-edit")))
                                       " Edit this package"))
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
                             )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct draft-package (old-name name description authors tags versions) #:transparent)

(define (edit-package-page request package-name-str)
  (authentication-wrap
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (cond
    [(and pkg (not (member (current-email) (or (@ pkg authors) '()))))
     ;; Not ours. Show it instead.
     (package-page request package-name-str)]
    [(not pkg)
     ;; Doesn't exist.
     (package-form (draft-package ""
                                  package-name
                                  ""
                                  '()
                                  '()
                                  '(("default" ""))))]
    [else
     (package-form (draft-package package-name
                                  package-name
                                  (@ pkg description)
                                  (@ pkg authors)
                                  (@ pkg tags)
                                  (for/list (((ver info) (in-hash (@ pkg versions))))
                                    (list (symbol->string ver) (@ info source)))))])))

(define (package-source-option source-type value label)
  `(option ((value ,value)
            ,@(maybe-splice (equal? source-type value) '(selected "selected")))
    ,label))

(define (package-form draft)
  (send/suspend/dispatch
   (lambda (embed-url)

     (define (build-versions-table)
       `(table
         (tr (th "Version")
             (th "Source"))
         ,@(for/list ((v (draft-package-versions draft)))
             (match-define (list version source) v)
             (define (control-name c) (format "version__~a__~a" version c))
             (define (textfield name label value [placeholder ""])
               `(div ((class "form-group"))
                 (label ((for ,(control-name name))) ,label)
                 (input ((class "form-control")
                         (type "text")
                         (name ,(control-name name))
                         (id ,(control-name name))
                         (placeholder ,placeholder)
                         (value ,value)))))
             (define-values (source-type simple-url g-host g-user g-project g-branch)
               (match source
                 [(pregexp #px"github://github\\.com/([^/]+)/([^/]+)(/([^/]+)/?)?"
                           (list _ u p _ b))
                  (values "github" "" "github.com" u p (if (equal? b "master") "" (or b #f)))]
                 [(pregexp #px"git://([^/]+)/([^/]+)/([^/]+)(/([^/]+)/?)?"
                           (list _ h u p _ b))
                  (values "git" "" h u p (if (equal? b "master") "" (or b "")))]
                 [_
                  (values "simple" source "" "" "" "")]))
             `(tr
               (td ,version)
               (td (div ((class "form-group"))
                        (label ((for ,(control-name "type"))) "Source type")
                        " "
                        (select ((class "package-version-source-type")
                                 (data-packageversion ,version)
                                 (name ,(control-name "type")))
                                ,(package-source-option source-type
                                                        "github"
                                                        "Github Repository")
                                ,(package-source-option source-type
                                                        "git"
                                                        "Git Repository")
                                ,(package-source-option source-type
                                                        "simple"
                                                        "Simple URL")))
                   ,(textfield "simple_url" "Source URL" simple-url)
                   ,(textfield "g_host" "Git Repository Host" g-host)
                   ,(textfield "g_user" "Git Repository User" g-user)
                   ,(textfield "g_project" "Git Repository Project" g-project)
                   ,(textfield "g_branch" "Git Repository Branch" g-branch "master")
                   ,@(maybe-splice
                      (not (equal? version "default"))
                      `(button ((type "submit")
                                (name ,(control-name "delete")))
                        (span ((class "glyphicon glyphicon-delete")))
                        " Delete version")))))))

     (parameterize ((bootstrap-page-scripts '("/editpackage.js")))
       (bootstrap-response (format "Editing package ~a" (draft-package-old-name draft))
                           `(form ((method "post")
                                   (action "TODO")
                                   (role "form"))
                             (div ((class "container"))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "name")) "Package Name")
                                            (input ((class "form-control")
                                                    (type "text")
                                                    (name "name")
                                                    (id "name")
                                                    (value ,(~a (draft-package-name draft))))))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "tags")) "Package Tags (space-separated)")
                                            (input ((class "form-control")
                                                    (type "text")
                                                    (tags "tags")
                                                    (id "tags")
                                                    (value ,(string-join
                                                             (draft-package-tags draft)))))))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "description")) "Package Description")
                                            (textarea ((class "form-control")
                                                       (name "description")
                                                       (id "description"))
                                                      ,(draft-package-description draft)))
                                       (div ((class "form-group col-sm-6"))
                                            (label ((for "authors"))
                                                   "Author email addresses (one per line)")
                                            (textarea ((class "form-control")
                                                       (name "authors")
                                                       (id "authors"))
                                                      ,(string-join (draft-package-authors draft)
                                                                    "\n"))))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-12"))
                                            (label "Package Versions & Sources")
                                            ,(build-versions-table)))
                                  (div ((class "row"))
                                       (div ((class "form-group col-sm-12"))
                                            (button ((type "submit")
                                                     (class "btn btn-primary")
                                                     (name "save_changes"))
                                                    (span ((class "glyphicon glyphicon-save")))
                                                    " Save changes")))))
                           )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-page request)
  (authentication-wrap
   #:request request
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
                                 (package-summary-table (package-search search-text tags))))))))
