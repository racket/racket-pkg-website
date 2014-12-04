#lang racket/base

(provide request-handler
         on-continuation-expiry
         rerender-all!)

(require racket/set)
(require racket/match)
(require racket/format)
(require racket/date)
(require racket/string)
(require racket/port)
(require net/url)
(require net/uri-codec)
(require web-server/servlet)
(require json)
(require "gravatar.rkt")
(require "bootstrap.rkt")
(require "html-utils.rkt")
(require "packages.rkt")
(require "sessions.rkt")
(require "jsonp-client.rkt")
(require reloadable)
(require "daemon.rkt")

(define static-cached-directory "../static/cached")
(define static-cached-urlprefix "/cached")

(define disable-cache? #f)

(define nav-index "Package Index")
(define nav-search "Search")

(define navbar-header
  `(a ((href "http://www.racket-lang.org/"))
    (img ((src "/logo-and-text.png")
          (height "60")
          (alt "Racket Package Index")))))

(define navigation `((,nav-index "/")
                     (,nav-search "/search")
                     ;; ((div ,(glyphicon 'download-alt)
                     ;;       " Download")
                     ;;  "http://download.racket-lang.org/")
                     ))

(define backend-baseurl "https://pkgd.racket-lang.org")

(define default-empty-source-url "git://github.com//")
(define COOKIE "pltsession")
(define recent-seconds (* 2 24 60 60)) ;; two days

(struct draft-package (old-name name description authors tags versions) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (request-handler named-url)
  (dispatch-rules
   [("index") main-page]
   [("") main-page]
   [("search") search-page]
   [("package" (string-arg)) package-page]
   [("package" (string-arg) "edit") edit-package-page]
   [("create") edit-package-page]
   [("login") login-page]
   [("register-or-reset") register-or-reset-page]
   [("logout") logout-page]
   [("json" "search-completions") json-search-completions]
   [("json" "tag-search-completions") json-tag-search-completions]
   [("json" "formal-tags") json-formal-tags]
   [("pkgs-all.json") pkgs-all-json]
   ))

(define (on-continuation-expiry request)
  (bootstrap-continuation-expiry-handler request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define static-render (make-parameter #f))

(define-syntax-rule (authentication-wrap #:request request body ...)
  (authentication-wrap* #f request (lambda () body ...)))

(define-syntax-rule (authentication-wrap/require-login #:request request body ...)
  (authentication-wrap* #t request (lambda () body ...)))

(define-syntax-rule (with-site-config body ...)
  (parameterize ((bootstrap-navbar-header navbar-header)
                 (bootstrap-navigation navigation)
                 (jsonp-baseurl backend-baseurl))
    body ...))

(define clear-session-cookie (make-cookie COOKIE
                                          ""
                                          #:path "/"
                                          #:expires "Thu, 01 Jan 1970 00:00:00 GMT"))

(define-syntax-rule (with-session-cookie cookie-value body ...)
  (let ((v cookie-value))
    (parameterize ((bootstrap-cookies
                    (if v
                        (list (make-cookie COOKIE v #:path "/" #:secure? #t))
                        (list clear-session-cookie))))
      body ...)))

(define (request->session request)
  (define session-cookies
    (filter (lambda (c) (equal? (client-cookie-name c) COOKIE))
            (request-cookies request)))
  (define session-keys (map client-cookie-value session-cookies))
  ;; (log-info "Session keys from cookie: ~a" session-keys)
  (for/or ((k session-keys)) (lookup-session/touch! k)))

(define (authentication-wrap* require-login? request body)
  (define session (request->session request))
  ;; (log-info "session: ~a" session)
  (define requested-url (url->string (request-uri request)))

  (if (and require-login? (not session))
      (login-or-register-flow* requested-url login-form)
      (parameterize ((bootstrap-navbar-extension
                      (cond
                       [(not session)
                        `((a ((id "register-button")
                              (class "btn btn-default navbar-btn navbar-right")
                              (href ,(login-or-register-url requested-url
                                                            (named-url register-or-reset-page))))
                             "Register")
                          (a ((id "sign-in-button")
                              (class "btn btn-success navbar-btn navbar-right")
                              (href ,(login-or-register-url requested-url
                                                            (named-url login-page))))
                             "Sign in"))]
                       [else
                        `((ul ((class "nav navbar-nav navbar-right"))
                              (li ((class "dropdown"))
                                  (a ((class "dropdown-toggle")
                                      (data-toggle "dropdown"))
                                     (img ((src ,(gravatar-image-url (session-email session)
                                                                     48))))
                                     " "
                                     ,(session-email session)
                                     " "
                                     (span ((class "caret"))))
                                  (ul ((class "dropdown-menu") (role "menu"))
                                      (li (a ((href ,(named-url edit-package-page)))
                                             ,(glyphicon 'plus-sign) " New package"))
                                      (li (a ((href ,(tags-page-url
                                                      (list
                                                       (format "author:~a"
                                                               (session-email session))))))
                                             ,(glyphicon 'user) " My packages"))
                                      (li ((class "divider"))
                                          (li (a ((href
                                                   ,(login-or-register-url
                                                     requested-url
                                                     (named-url logout-page))))
                                                 ,(glyphicon 'log-out) " Log out")))))))]))
                     (current-session session)
                     (bootstrap-cookies
                      (if session
                          (list (make-cookie COOKIE
                                             (session-key session)
                                             #:path "/"
                                             #:secure? #t))
                          (list))))
        (with-site-config (body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((generic-input type) name [initial-value ""] #:placeholder [placeholder #f])
  `(input ((class "form-control")
           (type ,type)
           (name ,name)
           (id ,name)
           ,@(maybe-splice placeholder `(placeholder ,placeholder))
           (value ,initial-value))))

(define email-input (generic-input "email"))
(define password-input (generic-input "password"))
(define text-input (generic-input "text"))

(define (label for . content)
  `(label ((class "control-label") ,@(maybe-splice for `(for ,for)))
    ,@content))

(define (primary-button . content)
  `(button ((type "submit")
            (class "btn btn-primary"))
    ,@content))

(define (generic-row class)
  (define (wrap cell)
    (match cell
      [(cons 'label _) cell]
      [_ `(div ,cell)]))
  (lambda (#:id [id #f] . args)
    `(div (,@(maybe-splice id `(id ,id))
           (class ,class))
      ,@(let loop ((args args))
          (match args
            [(list* _ _   #f rest)
             (loop rest)]
            [(list* 0 0 cell rest)
             (cons cell (loop rest))]
            [(list* 0 w cell rest)
             (cons (add-classes (list (format "col-sm-~a" w)) (wrap cell))
                   (loop rest))]
            [(list* o w cell rest)
             (cons (add-classes (list (format "col-sm-offset-~a col-sm-~a" o w)) (wrap cell))
                   (loop rest))]
            ['()
             '()])))))

(define form-group (generic-row "form-group"))
(define row (generic-row "row"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (login-or-register-url k baseurl)
  (format "~a?~a"
          baseurl
          (alist->form-urlencoded (list (cons 'k k)))))

(define (login-or-register-flow request thunk)
  (define-form-bindings request ([k (named-url main-page)]))
  (login-or-register-flow* k thunk))

(define (login-or-register-flow* k thunk)
  (with-session-cookie (thunk)
    (with-site-config
      (bootstrap-redirect k))))

(define (login-page request)
  (login-or-register-flow request login-form))

(define (register-or-reset-page request)
  (login-or-register-flow request register-form))

(define (logout-page request)
  (define session (request->session request))
  (when session (destroy-session! (session-key session)))
  (login-or-register-flow request (lambda () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (login-form [error-message #f])
  (with-site-config
   (send/suspend/dispatch
    (lambda (embed-url)
      (bootstrap-response "Login"
                          `(form ((class "form-horizontal")
                                  (method "post")
                                  (action ,(embed-url process-login-credentials))
                                  (role "form"))
                            ,(form-group 2 2 (label "email" "Email address")
                                         0 5 (email-input "email"))
                            ,(form-group 2 2 (label "password" "Password:")
                                         0 5 (password-input "password"))
                            ,(form-group 4 5
                                         `(a ((href ,(embed-url (lambda (req) (register-form)))))
                                           "Need to reset your password?"))
                            ,(form-group 4 5
                                         `(a ((href ,(embed-url (lambda (req) (register-form)))))
                                           "Register an account"))
                            ,@(maybe-splice
                               error-message
                               (form-group 4 5
                                           `(div ((class "alert alert-danger"))
                                             (p ,error-message))))
                            ,(form-group 4 5 (primary-button "Log in"))))))))

(define (authenticate-with-server! email password code)
  (jsonp-rpc! #:sensitive? #t
              #:include-credentials? #f
              "/jsonp/authenticate"
              (list (cons 'email email)
                    (cons 'passwd password)
                    (cons 'code code))))

(define (process-login-credentials request)
  (define-form-bindings request (email password))
  (if (or (equal? (string-trim email) "")
          (equal? (string-trim password) ""))
      (login-form "Please enter your email address and password.")
      (match (authenticate-with-server! email password "")
        ["wrong-code"
         (login-form "Something went awry; please try again.")]
        [(or "emailed" #f)
         (summarise-code-emailing "Incorrect password, or nonexistent user." email)]
        [else
         (create-session! email password)])))

(define (register-form #:email [email ""]
                       #:code [code ""]
                       #:error-message [error-message #f])
  (with-site-config
   (send/suspend/dispatch
    (lambda (embed-url)
      (bootstrap-response "Register/Reset Account"
                          #:title-element ""
                          `(div ((class "registration-step-container"))
                            (div ((class "registration-step"))
                                 (div (h1 "Step 1")
                                      (p "Get a code")))
                            (span ((class "registration-step-arrow")) "→")
                            (div ((class "registration-step"))
                                 (div (h1 "Step 2")
                                      (p "Use the code"))))

                          `(div
                            (h1 "Need a code?")
                            (p "Enter your email address below, and we'll send you one.")
                            (form ((class "form-horizontal")
                                   (method "post")
                                   (action ,(embed-url notify-of-emailing))
                                   (role "form"))
                                  ,(form-group 1 3 (label "email" "Email address")
                                               0 5 (email-input "email_for_code"))
                                  ,(form-group 4 5 (primary-button "Email me a code"))))

                          `(div
                            (h1 "Got a registration or reset code?")
                            (p "Great! Enter it below, with your chosen password, to log in.")
                            (form ((class "form-horizontal")
                                   (method "post")
                                   (action ,(embed-url apply-account-code))
                                   (role "form"))
                                  ,(form-group 1 3 (label "email" "Email address")
                                               0 5 (email-input "email" email))
                                  ,(form-group 1 3 (label "code" "Code")
                                               0 5 (text-input "code" code))
                                  ,(form-group 1 3 (label "password" "Password")
                                               0 5 (password-input "password"))
                                  ,(form-group 1 3 (label "password" "Confirm password")
                                               0 5 (password-input "confirm_password"))
                                  ,@(maybe-splice
                                     error-message
                                     (form-group 4 5
                                                 `(div ((class "alert alert-danger"))
                                                   (p ,error-message))))
                                  ,(form-group 4 5 (primary-button "Continue")))))))))

(define (apply-account-code request)
  (define-form-bindings request (email code password confirm_password))
  (define (retry msg)
    (register-form #:email email
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
  (with-site-config
   (send/suspend/dispatch
    (lambda (embed-url)
      (bootstrap-response reason
                          `(p
                            "We've emailed an account registration/reset code to "
                            (code ,email) ". Please check your email and then click "
                            "the button to continue:")
                          `(a ((class "btn btn-primary")
                               (href ,(embed-url (lambda (req) (register-form)))))
                            "Enter your code"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (use-cache?)
  ;; We use the cache if it isn't disabled, but ONLY when the user is
  ;; not logged in to an account. When they are logged in, they see
  ;; user-specific options which don't cache well.
  (not (or (current-session) disable-cache?)))

(define (main-page-url)
  (if (use-cache?)
      (format "~a~a" static-cached-urlprefix (named-url main-page))
      (named-url main-page)))

(define (view-package-url package-name)
  (define package-name-str (~a package-name))
  (if (use-cache?)
      (format "~a~a" static-cached-urlprefix (named-url package-page package-name-str))
      (named-url package-page package-name-str)))

(define (package-link package-name)
  `(a ((href ,(view-package-url package-name))) ,(~a package-name)))

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

(define (author-link author-name #:gravatar? [gravatar? #f])
  `(a ((href ,(tags-page-url (list (format "author:~a" author-name)))))
    ,@(maybe-splice gravatar?
                    `(img ((src ,(gravatar-image-url author-name 48))))
                    " ")
    ,author-name))

(define (tag-link tag-name)
  `(a ((href ,(tags-page-url (list tag-name)))) ,tag-name))

(define (buildhost-link #:attributes [attributes '()] url-suffix label)
  `(a (,@attributes
       (href ,(format "http://pkg-build.racket-lang.org/~a" url-suffix))) ,label))

(define (authors-list authors #:gravatars? [gravatars? #f])
  `(ul ((class "authors")) ,@(for/list ((author authors))
                               `(li ,(author-link author #:gravatar? gravatars?)))))

(define (package-links #:pretty? [pretty? #t] package-names)
  (if (and pretty? (null? package-names))
      `(span ((class "packages none")) "None")
      `(ul ((class "list-inline packages"))
        ,@(for/list ((p package-names)) `(li ,(package-link p))))))

(define (doc-links docs)
  `(ul ((class "list-inline doclinks"))
    ,@(for/list ((doc (or docs '()))) `(li ,(doc-link doc)))))

(define (tag-links tags)
  `(ul ((class "list-inline taglinks")) ,@(for/list ((tag (or tags '()))) `(li ,(tag-link tag)))))

(define (utc->string utc)
  (if (and utc (not (zero? utc)))
      (string-append (date->string (seconds->date utc #f) #t) " (UTC)")
      "N/A"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package hashtable getters.
;; TODO factor this stuff out into a proper data structure

;; Mandatory -- never #f
(define (package-name pkg)                  (@ pkg name))

;; Optional -- sometimes #f
(define (package-build-failure-log pkg)     (@ pkg build failure-log))
(define (package-build-success-log pkg)     (@ pkg build success-log))
(define (package-build-dep-failure-log pkg) (@ pkg build dep-failure-log))
(define (package-build-conflicts-log pkg)   (@ pkg build conflicts-log))
(define (package-ring pkg)                  (@ pkg ring))
(define (package-checksum-error pkg)        (@ pkg checksum-error))

(define (package-readme-url pkg)
  (@ (package-external-information (string->symbol (@ pkg name))) readme-url))

(define (package-default-version pkg)
  (@ (package-versions pkg) default))

(define (package-locally-modified? pkg)
  (@ pkg _LOCALLY_MODIFIED_))

;; If absent, default values substituted
(define (package-last-updated pkg)      (or (@ pkg last-updated) 0))
(define (package-last-checked pkg)      (or (@ pkg last-checked) 0))
(define (package-last-edit pkg)         (or (@ pkg last-edit) 0))
(define (package-authors pkg)           (or (@ pkg authors) '()))
(define (package-description pkg)       (or (@ pkg description) ""))
(define (package-tags pkg)              (or (@ pkg tags) '()))
(define (package-versions pkg)          (or (@ pkg versions) (hash)))
(define (package-docs pkg)              (or (@ pkg build docs) '()))
(define (package-conflicts pkg)         (or (@ pkg conflicts) '()))
(define (package-dependencies pkg)      (or (@ pkg dependencies) '()))
(define (package-modules pkg)           (or (@ pkg modules) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (package-summary-table package-names)
  (define now (/ (current-inexact-milliseconds) 1000))
  `(table
    ((class "packages sortable"))
    (thead
     (tr
      (th 'nbsp)
      (th "Package")
      (th "Description")
      (th "Build")))
    (tbody
     ,@(maybe-splice (null? package-names)
                     `(tr (td ((colspan "4"))
                              (div ((class "alert alert-info"))
                                   "No packages found."))))
     ,@(for/list ((pkg (package-batch-detail package-names)))
         `(tr
           (td (span ((class "last-updated-negated") (style "display: none"))
                     ,(~a (- (package-last-updated pkg))))
               ,@(maybe-splice
                  (< (- now (package-last-updated pkg)) recent-seconds)
                  `(span ((class "label label-info")) "New")))
           (td (h2 ,(package-link (package-name pkg)))
               ,(authors-list (package-authors pkg)))
           (td (p ,(package-description pkg))
               ,@(maybe-splice
                  (or (pair? (package-docs pkg)) (package-readme-url pkg))
                  `(div
                    (span ((class "doctags-label")) "Docs: ")
                    ,(doc-links (package-docs pkg))
                    ,@(maybe-splice (package-readme-url pkg)
                                    " "
                                    `(a ((href ,(package-readme-url pkg)))
                                      "README"))
                    ))
               ,@(maybe-splice
                  (pair? (package-tags pkg))
                  `(div
                    (span ((class "doctags-label")) "Tags: ")
                    ,(tag-links (package-tags pkg)))))
           ,(cond
             [(package-build-failure-log pkg)
              `(td ((class "build_red"))
                ,(buildhost-link (package-build-failure-log pkg) "fails"))]
             [(and (package-build-success-log pkg)
                   (package-build-dep-failure-log pkg))
              `(td ((class "build_yellow"))
                ,(buildhost-link (package-build-success-log pkg)
                                 "succeeds")
                " with "
                ,(buildhost-link (package-build-dep-failure-log pkg)
                                 "dependency problems"))]
             [(package-build-success-log pkg)
              `(td ((class "build_green"))
                ,(buildhost-link (package-build-success-log pkg) "succeeds"))]
             [else
              `(td)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-page request)
  (parameterize ((bootstrap-active-navigation nav-index)
                 (bootstrap-page-scripts '("/searchbox.js")))
    (define package-name-list (package-search "" '((main-distribution #f))))
    (authentication-wrap
     #:request request
     (cond
      [(and (use-cache?) (not (static-render)))
       ;; Redirect to static version
       (bootstrap-redirect (main-page-url))]
      [else
       (bootstrap-response "Racket Package Index"
                           #:title-element ""
                           #:body-class "main-page"
                           `(div ((class "jumbotron"))
                             (h1 "Racket Packages")
                             (p "These are the packages in the official "
                                (a ((href "http://docs.racket-lang.org/pkg/getting-started.html"))
                                   "package catalog") ".")
                             (p (a ((href "http://docs.racket-lang.org/pkg/cmdline.html"))
                                   (kbd "raco pkg install " (var "package-name")))
                                " installs a package.")
                             (p "You can "
                                (a ((id "create-package-link")
                                    (href ,(named-url edit-package-page)))
                                   (span ((class "label label-success"))
                                         ,(glyphicon 'plus-sign)
                                         " add your own"))
                                " packages to the index."))
                           `(div ((id "search-box"))
                             (form ((role "form")
                                    (action ,(named-url search-page)))
                                   ,(text-input "q" #:placeholder "Search packages")))
                           `(div
                             (p ((class "package-count"))
                                ,(format "~a packages" (length package-name-list)))
                             ,(package-summary-table package-name-list)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-status buildhost-url str label-type glyphicon-type)
  `(p ((class "build-status"))
    "Build status: "
    ,(buildhost-link buildhost-url
                     `(span ((class ,(format "label label-~a" label-type)))
                       ,(glyphicon glyphicon-type) " " ,str))))

(define (package-page request package-name-str)
  (authentication-wrap
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (define default-version (package-default-version pkg))
   (cond
    [(and (use-cache?) (not (static-render)))
     ;; Redirect to static version
     (bootstrap-redirect (view-package-url package-name))]
    [(not pkg)
     (bootstrap-response #:code 404
                         #:message #"No such package"
                         "Package not found"
                         `(div "The package " (code ,package-name-str) " does not exist."))]
    [else
     (bootstrap-response (~a package-name)
                         #:title-element ""
                         `(div ((class "jumbotron"))
                           (h1 ,(~a package-name))
                           (p ,(package-description pkg))
                           ,(cond
                             [(package-build-failure-log pkg)
                              (build-status (package-build-failure-log pkg)
                                            "failed" "danger" "fire")]
                             [(and (package-build-success-log pkg)
                                   (package-build-dep-failure-log pkg))
                              (build-status (package-build-dep-failure-log pkg)
                                            "problems" "warning" "question-sign")]
                             [(package-build-success-log pkg)
                              (build-status (package-build-success-log pkg)
                                            "ok" "success" "ok")]
                             [else
                              ""])
                           (div ((class "dropdown"))
                                ,@(let ((docs (package-docs pkg)))
                                    (match docs
                                      [(list)
                                       `()]
                                      [(list doc)
                                       (define-values (n u) (doc-destruct doc))
                                       (list (buildhost-link
                                              #:attributes `((class "btn btn-success btn-lg"))
                                              u
                                              `(span ,(glyphicon 'file) " Documentation")))]
                                      [_
                                       `((button ((class "btn btn-success btn-lg dropdown-toggle")
                                                  (data-toggle "dropdown"))
                                                 ,(glyphicon 'file)
                                                 " Documentation "
                                                 (span ((class "caret"))))
                                         (ul ((class "dropdown-menu")
                                              (role "menu"))
                                             ,@(for/list ((doc docs)) `(li ,(doc-link doc)))))]))

                                " "
                                ,@(maybe-splice
                                   (package-readme-url pkg)
                                   `(a ((class "btn btn-info btn-lg")
                                        (href ,(package-readme-url pkg)))
                                     ,(glyphicon 'eye-open)
                                     " README"))

                                ;; Heuristic guess as to whether we should present a "browse"
                                ;; link or a "download" link.
                                " "
                                ,(if (equal? (@ default-version source)
                                             (@ default-version source_url))
                                     `(a ((class "btn btn-default btn-lg")
                                          (href ,(@ default-version source_url)))
                                       ,(glyphicon 'download) " Download"
                                       ;; ,(if (regexp-match? "(?i:\\.zip$)" (or (@ default-version source_url) ""))
                                       ;;      " Zip file"
                                       ;;      " Download")
                                       )
                                     `(a ((class "btn btn-default btn-lg")
                                          (href ,(@ default-version source_url)))
                                       ,(glyphicon 'link) " Code"))

                                ,@(maybe-splice
                                   (member (current-email) (package-authors pkg))
                                   " "
                                   `(a ((class "btn btn-info btn-lg")
                                        (href ,(named-url edit-package-page package-name-str)))
                                     ,(glyphicon 'edit) " Edit this package"))
                                ))

                         (if (package-locally-modified? pkg)
                             `(div ((class "alert alert-warning")
                                    (role "alert"))
                               ,(glyphicon 'exclamation-sign)
                               " This package has been modified since the package index was last rebuilt."
                               " The next index refresh is scheduled for "
                               ,(utc->string (/ (next-fetch-deadline) 1000)) ".")
                             "")

                         (if (package-checksum-error pkg)
                             `(div ((class "alert alert-danger")
                                    (role "alert"))
                               (span ((class "label label-danger"))
                                     "Checksum error")
                               " The package checksum does not match"
                               " the package source code.")
                             "")

                         `(table ((class "package-details"))
                           (tr (th "Authors")
                               (td (div ((class "authors-detail"))
                                        ,(authors-list #:gravatars? #t (package-authors pkg)))))
                           (tr (th "Documentation")
                               (td ,(doc-links (package-docs pkg))))
                           (tr (th "Tags")
                               (td ,(tag-links (package-tags pkg))))
                           (tr (th "Last updated")
                               (td ,(utc->string (package-last-updated pkg))))
                           (tr (th "Ring")
                               (td ,(~a (or (package-ring pkg) "N/A"))))
                           (tr (th "Conflicts")
                               (td ,(package-links (package-conflicts pkg))))
                           (tr (th "Dependencies")
                               (td ,(package-links (package-dependencies pkg))))
                           (tr (th "Most recent build results")
                               (td (ul ((class "build-results"))
                                       ,@(maybe-splice
                                          (package-build-success-log pkg)
                                          `(li "Compiled successfully: "
                                            ,(buildhost-link (package-build-success-log pkg)
                                                             "transcript")))
                                       ,@(maybe-splice
                                          (package-build-failure-log pkg)
                                          `(li "Compiled unsuccessfully: "
                                            ,(buildhost-link (package-build-failure-log pkg)
                                                             "transcript")))
                                       ,@(maybe-splice
                                          (package-build-conflicts-log pkg)
                                          `(li "Conflicts: "
                                            ,(buildhost-link (package-build-conflicts-log pkg)
                                                             "details")))
                                       ,@(maybe-splice
                                          (package-build-dep-failure-log pkg)
                                          `(li "Dependency problems: "
                                            ,(buildhost-link (package-build-dep-failure-log pkg)
                                                             "details")))
                                       )))
                           ,@(let* ((vs (package-versions pkg))
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
                               (td ,(utc->string (package-last-checked pkg))))
                           (tr (th "Last edited")
                               (td ,(utc->string (package-last-edit pkg))))
                           (tr (th "Modules")
                               (td (ul ((class "module-list"))
                                       ,@(for/list ((mod (package-modules pkg)))
                                           (match-define (list kind path) mod)
                                           `(li ((class ,kind)) ,path)))))
                           ))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-package-page request [package-name-str ""])
  (authentication-wrap/require-login
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (cond
    [(and pkg (not (member (current-email) (package-authors pkg))))
     ;; Not ours. Show it instead.
     (bootstrap-redirect (view-package-url package-name))]
    [(not pkg)
     ;; Doesn't exist.
     (package-form #f (draft-package ""
                                     package-name-str
                                     ""
                                     (list (current-email))
                                     '()
                                     `(("default" ,default-empty-source-url))))]
    [else
     (package-form #f
                   (draft-package package-name-str
                                  package-name-str
                                  (package-description pkg)
                                  (package-authors pkg)
                                  (package-tags pkg)
                                  (for/list (((ver info) (in-hash (package-versions pkg))))
                                    (list (symbol->string ver) (@ info source)))))])))

(define (package-source-option source-type value label)
  `(option ((value ,value)
            ,@(maybe-splice (equal? source-type value) '(selected "selected")))
    ,label))

(define (put-default-first alist)
  (define default (assoc "default" alist))
  (cons default (remove default alist)))

(define (package-form error-message draft)
  (with-site-config
   (send/suspend/dispatch
    (lambda (embed-url)

      (define (build-versions-table)
        `(table ((class "package-versions"))
          (tr (th "Version")
              (th "Source"))
          ,@(for/list ((v (put-default-first
                           (draft-package-versions draft))))
              (match-define (list version source) v)
              (define (control-name c) (format "version__~a__~a" version c))
              (define (group-name c) (format "version__~a__~a__group" version c))
              (define (textfield name label-text value [placeholder ""])
                (row #:id (group-name name)
                     0 3
                     (and label-text (label (control-name name) label-text))
                     0 (if label-text 9 12)
                     (text-input (control-name name) value #:placeholder placeholder)))
              (define-values (source-type simple-url g-host g-user g-project g-branch)
                (match source
                  [(pregexp #px"github://github\\.com/([^/]*)/([^/]*)(/([^/]*)/?)?"
                            (list _ u p _ b))
                   (values "github" "" "github.com" u p (if (equal? b "master") "" (or b #f)))]
                  [(pregexp #px"git://([^/]*)/([^/]*)/([^/]*)(/([^/]*)/?)?"
                            (list _ h u p _ b))
                   (values "git" "" h u p (if (equal? b "master") "" (or b "")))]
                  [_
                   (values "simple" source "" "" "" "")]))
              `(tr
                (td ,version
                    ,@(maybe-splice
                       (not (equal? version "default"))
                       " "
                       `(button ((class "btn btn-danger btn-xs")
                                 (type "submit")
                                 (name "action")
                                 (value ,(control-name "delete")))
                         ,(glyphicon 'trash))))
                (td ,(row
                      0 3 `(div ((id ,(group-name "type")))
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
                      0 9 `(div ((id ,(group-name "fields")))
                            (div ((id ,(group-name "urlpreview"))
                                  (class "row"))
                                 (div ((class "col-sm-3"))
                                      ,(label #f "URL preview"))
                                 (div ((class "col-sm-9"))
                                      (span ((class "form-control disabled")
                                             (disabled "disabled")
                                             (id ,(control-name "urlpreview"))))))
                            ,(textfield "simple_url" #f simple-url)
                            ,(textfield "g_host" "Repo Host" g-host)
                            ,(textfield "g_user" "Repo User" g-user)
                            ,(textfield "g_project" "Repo Project" g-project)
                            ,(textfield "g_branch" "Repo Branch" g-branch "master"))))))

          (tr (td ((colspan "2"))
                  (div ((class "form-inline"))
                       ,(text-input "new_version" #:placeholder "x.y.z")
                       " "
                       (button ((class "btn btn-success btn-xs")
                                (type "submit")
                                (name "action")
                                (value "add_version"))
                               ,(glyphicon 'plus-sign) " Add new version"))))
          ))

      (parameterize ((bootstrap-page-scripts '("/editpackage.js")))
        (define old-name (draft-package-old-name draft))
        (define has-old-name? (not (equal? old-name "")))
        (bootstrap-response (if has-old-name?
                                (format "Edit package ~a" old-name)
                                "Create a new package")
                            #:body-class "package-form"
                            (if error-message
                                `(div ((class "alert alert-danger"))
                                  ,(glyphicon 'exclamation-sign) " " ,error-message)
                                "")
                            `(form ((id "edit-package-form")
                                    (method "post")
                                    (action ,(embed-url (update-draft draft)))
                                    (role "form"))
                              (div ((class "container")) ;; TODO: remove??
                                   (div ((class "row"))
                                        (div ((class "form-group col-sm-6"))
                                             ,(label "name" "Package Name")
                                             ,(text-input "name" (~a (draft-package-name draft))))
                                        (div ((class "form-group col-sm-6"))
                                             ,(label "tags" "Package Tags (space-separated)")
                                             ,(text-input "tags" (string-join
                                                                  (draft-package-tags draft)))))
                                   (div ((class "row"))
                                        (div ((class "form-group col-sm-6"))
                                             ,(label "description" "Package Description")
                                             (textarea ((class "form-control")
                                                        (name "description")
                                                        (id "description"))
                                                       ,(draft-package-description draft)))
                                        (div ((class "form-group col-sm-6"))
                                             ,(label "authors"
                                                     "Author email addresses (one per line)")
                                             (textarea ((class "form-control")
                                                        (name "authors")
                                                        (id "authors"))
                                                       ,(string-join (draft-package-authors draft)
                                                                     "\n"))))
                                   (div ((class "row"))
                                        (div ((class "form-group col-sm-12"))
                                             ,(label #f "Package Versions & Sources")
                                             ,(build-versions-table)))
                                   (div ((class "row"))
                                        (div ((class "form-group col-sm-12"))
                                             ,@(maybe-splice
                                                has-old-name?
                                                `(a ((class "btn btn-danger pull-right")
                                                     (href ,(embed-url
                                                             (confirm-package-deletion old-name))))
                                                  ,(glyphicon 'trash) " Delete package")
                                                " ")
                                             (button ((type "submit")
                                                      (class "btn btn-primary")
                                                      (name "action")
                                                      (value "save_changes"))
                                                     ,(glyphicon 'save) " Save changes")
                                             ,@(maybe-splice
                                                has-old-name?
                                                " "
                                                `(a ((class "btn btn-default")
                                                     (href ,(view-package-url old-name)))
                                                  "Cancel changes and return to package page"))))))
                            ))))))

(define ((confirm-package-deletion package-name-str) request)
  (with-site-config
   (send/suspend
    (lambda (k-url)
      (bootstrap-response "Confirm Package Deletion"
                          `(div ((class "confirm-package-deletion"))
                            (h2 ,(format "Delete ~a?" package-name-str))
                            (p "This cannot be undone.")
                            (a ((class "btn btn-default")
                                (href ,k-url))
                               "Confirm deletion")))))
   (jsonp-rpc! "/jsonp/package/del" `((pkg . ,package-name-str)))
   (define completion-ch (make-channel))
   (delete-package! completion-ch (string->symbol package-name-str))
   (channel-get completion-ch)
   (bootstrap-redirect (main-page-url))))

(define ((update-draft draft0) request)
  (define draft (read-draft-form draft0 (request-bindings request)))
  (define-form-bindings request (action new_version))
  (match action
    ["save_changes"
     (if (save-draft! draft)
         (with-site-config
           (bootstrap-redirect (view-package-url (draft-package-name draft))))
         (package-form "Save failed."
                       ;; ^ TODO: This is the worst error message.
                       ;;         Right up there with "parse error".
                       draft))]
    ["add_version"
     (cond
      [(equal? (string-trim new_version) "")
       (package-form "Please enter a version number to add." draft)]
      [(assoc new_version (draft-package-versions draft))
       (package-form (format "Could not add version ~a, as it already exists." new_version)
                     draft)]
      [else
       (package-form #f (struct-copy draft-package draft
                                     [versions (cons (list new_version default-empty-source-url)
                                                     (draft-package-versions draft))]))])]
    [(regexp #px"^version__(.*)__delete$" (list _ version))
     (package-form #f (struct-copy draft-package draft
                                   [versions (filter (lambda (v)
                                                       (not (equal? (car v) version)))
                                                     (draft-package-versions draft))]))]))

(define (read-draft-form draft bindings)
  (define (g key d)
    (cond [(assq key bindings) => cdr]
          [else d]))
  (define (read-version-source version)
    (define (vg name d)
      (g (string->symbol (format "version__~a__~a" version name)) d))
    (define type (vg 'type "simple"))
    (define simple_url (vg 'simple_url ""))
    (define g_host (vg 'g_host ""))
    (define g_user (vg 'g_user ""))
    (define g_project (vg 'g_project ""))
    (define g_branch0 (vg 'g_branch ""))
    (define g_branch (if (equal? g_branch0 "") "master" g_branch0))
    (match type
      ["github" (format "github://github.com/~a/~a/~a" g_user g_project g_branch)]
      ["git"    (format "git://~a/~a/~a/~a" g_host g_user g_project g_branch)]
      ["simple" simple_url]))
  (struct-copy draft-package draft
               [name (g 'name (draft-package-old-name draft))]
               [description (g 'description "")]
               [authors (string-split (g 'authors ""))]
               [tags (string-split (g 'tags ""))]
               [versions (for/list ((old (draft-package-versions draft)))
                           (match-define (list version _) old)
                           (list version
                                 (read-version-source version)))]))

(define (added-and-removed old new)
  (define old-set (list->set (or old '())))
  (define new-set (list->set new))
  (values (set->list (set-subtract new-set old-set))
          (set->list (set-subtract old-set new-set))))

(define (save-draft! draft)
  (match-define (draft-package old-name name description authors tags versions/default) draft)
  (define default-version (assoc "default" versions/default))
  (define source (cadr default-version))
  (define versions (remove default-version versions/default))

  (define old-pkg (package-detail (string->symbol old-name)))

  (define-values (added-tags removed-tags)
    (added-and-removed (package-tags old-pkg) tags))
  (define-values (added-authors removed-authors)
    (let ((old-authors (package-authors old-pkg)))
      (added-and-removed (if (null? old-authors)
                             (list (current-email))
                             old-authors)
                         authors)))

  (define old-versions-map (package-versions old-pkg))
  (define changed-versions
    (for/fold ((acc '())) ((v versions))
      (match-define (list version-str new-source) v)
      (define version-sym (string->symbol version-str))
      (define old-source (@ (@ref old-versions-map version-sym) source))
      (if (equal? old-source new-source)
          acc
          (cons v acc))))
  (define removed-versions
    (for/list ((k (in-hash-keys old-versions-map))
               #:when (not (assoc (symbol->string k) versions/default))) ;; NB versions/default !
      (symbol->string k)))

  ;; name, description, and default source are updateable via /jsonp/package/modify.
  ;; tags are added and removed via /jsonp/package/tag/add and .../del.
  ;; authors are added and removed via /jsonp/package/author/add and .../del.
  ;; versions other than default are added and removed via /jsonp/package/version/add and .../del.
  (and (or (equal? old-name name)
           ;; Don't let renames stomp on existing packages
           (not (package-detail (string->symbol name))))
       (jsonp-rpc! "/jsonp/package/modify" `((pkg . ,old-name)
                                             (name . ,name)
                                             (description . ,description)
                                             (source . ,source)))
       (andmap (lambda (t) (jsonp-rpc! "/jsonp/package/tag/add" `((pkg . ,name) (tag . ,t))))
               added-tags)
       (andmap (lambda (t) (jsonp-rpc! "/jsonp/package/tag/del" `((pkg . ,name) (tag . ,t))))
               removed-tags)
       (andmap (lambda (a) (jsonp-rpc! "/jsonp/package/author/add" `((pkg . ,name) (author . ,a))))
               added-authors)
       (andmap (lambda (a) (jsonp-rpc! "/jsonp/package/author/del" `((pkg . ,name) (author . ,a))))
               removed-authors)
       (andmap (lambda (e) (jsonp-rpc! "/jsonp/package/version/add" `((pkg . ,name)
                                                                      (version . ,(car e))
                                                                      (source . ,(cadr e)))))
               changed-versions)
       (andmap (lambda (v) (jsonp-rpc! "/jsonp/package/version/del" `((pkg . ,name)
                                                                      (version . ,v))))
               removed-versions)

       (let* ((new-pkg (or old-pkg (hash)))
              (new-pkg (hash-set new-pkg 'name name))
              (new-pkg (hash-set new-pkg 'description description))
              (new-pkg (hash-set new-pkg 'author (string-join authors)))
              (new-pkg (hash-set new-pkg 'authors authors))
              (new-pkg (hash-set new-pkg 'tags tags))
              (new-pkg (hash-set new-pkg 'versions (friendly-versions versions/default)))
              (new-pkg (hash-set new-pkg 'source source))
              (new-pkg (hash-set new-pkg 'search-terms (compute-search-terms new-pkg)))
              (new-pkg (hash-set new-pkg '_LOCALLY_MODIFIED_ #t))
              (completion-ch (make-channel)))
         (replace-package! completion-ch old-pkg new-pkg)
         (channel-get completion-ch)
         #t)))

;; Based on (and copied from) the analogous code in meta/pkg-index/official/static.rkt
(define (compute-search-terms ht)
  (let* ([st (hasheq)]
         [st (for/fold ([st st])
                       ([t (in-list (hash-ref ht 'tags (lambda () '())))])
               (hash-set st (string->symbol t) #t))]
         [st (hash-set
              st
              (string->symbol
               (format "ring:~a" (hash-ref ht 'ring (lambda () 2)))) #t)]
         [st (for/fold ([st st])
                       ([a (in-list (string-split (hash-ref ht 'author (lambda () ""))))])
               (hash-set
                st (string->symbol (format "author:~a" a)) #t))]
         [st (if (null? (hash-ref ht 'tags (lambda () '())))
                 (hash-set st ':no-tag: #t)
                 st)]
         [st (if (hash-ref ht 'checksum-error #f)
                 (hash-set st ':error: #t)
                 st)]
         [st (if (equal? "" (hash-ref ht 'description ""))
                 (hash-set st ':no-desc: #t)
                 st)]
         [st (if (null? (hash-ref ht 'conflicts (lambda () '())))
                 st
                 (hash-set st ':conflicts: #t))])
    st))

(define (friendly-versions draft-versions)
  (for/hash ((v draft-versions))
    (match-define (list version source) v)
    (values (string->symbol version)
            (hash 'checksum ""
                  'source source
                  'source_url (package-url->useful-url source)))))

;; Copied from meta/pkg-index/official/static.rkt
(define (package-url->useful-url pkg-url-str)
    (define pkg-url
      (string->url pkg-url-str))
    (match (url-scheme pkg-url)
      ["github"
       (match (url-path pkg-url)
         [(list* user repo branch path)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list* user repo (path/param "tree" '()) branch path)]))]
         [_
          pkg-url-str])]
      ["git"
       (match (url-path pkg-url)
         ;; xxx make this more robust
         [(list user repo)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list user repo (path/param "tree" '())
                        (path/param "master" '()))]))]
         [_
          pkg-url-str])]
      [_
       pkg-url-str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-page request)
  (parameterize ((bootstrap-active-navigation nav-search)
                 (bootstrap-page-scripts '("/searchbox.js")))
    (authentication-wrap
     #:request request
     (define-form-bindings request ([search-text q ""]
                                    [tags-input tags ""]))
     (define tags (for/list ((t (string-split tags-input)))
                    (match t
                      [(pregexp #px"!(.*)" (list _ tag)) (list (string->symbol tag) #f)]
                      [tag (list (string->symbol tag) #t)])))
     (bootstrap-response "Search Package Index"
                         #:body-class "search-page"
                         `(form ((class "form-horizontal")
                                 (role "form"))
                           ,(form-group 0 2 (label "q" "Search terms")
                                        0 10 (text-input "q" search-text
                                                         #:placeholder
                                                         "Enter free-form text to match here"))
                           ,(form-group 0 2 (label "tags" "Tags")
                                        0 10(text-input "tags" tags-input
                                                        #:placeholder
                                                        "tag1 tag2 tag3 ..."))
                           ,(form-group 2 10 (primary-button (glyphicon 'search) " Search"))
                           (div ((class "search-results"))
                                ,@(maybe-splice
                                   (or (pair? tags) (not (equal? search-text "")))
                                   (let ((package-name-list (package-search search-text tags)))
                                     `(div
                                       (p ((class "package-count"))
                                          ,(format "~a packages found" (length package-name-list)))
                                       ,(package-summary-table package-name-list))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (json-search-completions request)
  (define completions (set-union (list->set (map ~a (all-package-names))) (all-formal-tags)))
  (response/output #:mime-type #"application/json"
                   (lambda (response-port)
                     (write-json (set->list completions) response-port))))

(define (json-tag-search-completions request)
  (response/output #:mime-type #"application/json"
                   (lambda (response-port)
                     (write-json (set->list (all-tags)) response-port))))

(define (json-formal-tags request)
  (response/output #:mime-type #"application/json"
                   (lambda (response-port)
                     (write-json (set->list (all-formal-tags)) response-port))))

(define (pkgs-all-json request)
  (response/output #:mime-type #"application/json"
                   (lambda (response-port)
                     (write-json (packages-jsexpr) response-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (static-render! handler . named-url-args)
  (local-require racket/promise)
  (local-require racket/file)
  (local-require web-server/private/servlet)
  (local-require web-server/http/request-structs)
  (define request-url (apply named-url handler named-url-args))
  (log-info "Rendering static version of ~a" request-url)
  (define response
    (parameterize ((static-render #t))
      (call-with-continuation-barrier
       (lambda ()
         (call-with-continuation-prompt
          (lambda ()
            (apply handler
                   (request #"GET"
                            (string->url request-url)
                            '()
                            (delay '())
                            #f
                            "127.0.0.1"
                            0
                            "127.0.0.1")
                   named-url-args))
          servlet-prompt)))))
  (define filename (format "~a~a" static-cached-directory request-url))
  (make-parent-directory* filename)
  (call-with-output-file filename
    (response-output response)
    #:exists 'replace))

;; TODO: fold the collection of this information into the package
;; database itself.
(define (update-external-package-information! package-name)
  (define pkg (package-detail package-name))
  (define default-version (package-default-version pkg))
  (define external-information
    (and pkg
         (if (equal? (@ default-version source)
                     (@ default-version source_url))
             ;; We don't know where to look for a readme.
             (hash)
             ;; It's probably a github-like repo. Check for a readme.
             (let ((contents
                    (port->string
                     (get-pure-port (string->url (@ default-version source_url))
                                    #:redirections 10))))
               ;;(log-info "CONTENTS: ~a === ~a" (@ default-version source_url) contents)
               (if (regexp-match? #px"(?i:id=.readme.)" contents)
                   (let ((readme-url (string-append (@ default-version source_url) "#readme")))
                     (log-info "Package ~a has a readme at ~a" package-name readme-url)
                     (hash 'readme-url readme-url))
                   (hash))))))
  (set-package-external-information! package-name external-information))

(define (rerender-all!)
  (thread-send (package-change-handler-thread) 'rerender-all!))

(define (package-change-handler index-rerender-needed? pending-completions)
  (sync/timeout (and index-rerender-needed?
                     (lambda ()
                       (static-render! main-page)
                       (for ((completion-ch pending-completions))
                         (channel-put completion-ch (void)))
                       (package-change-handler #f '())))
                (handle-evt (thread-receive-evt)
                            (lambda (_)
                              (match (thread-receive)
                                ['upgrade
                                 (package-change-handler index-rerender-needed?
                                                         pending-completions)]
                                ['rerender-all!
                                 (log-info "rerender-all!")
                                 (for ((p (all-package-names)))
                                   (update-external-package-information! p)
                                   (static-render! package-page (symbol->string p)))
                                 (static-render! main-page)
                                 (package-change-handler index-rerender-needed?
                                                         pending-completions)]
                                [(list 'package-changed completion-ch package-name)
                                 (update-external-package-information! package-name)
                                 (static-render! package-page (symbol->string package-name))
                                 (package-change-handler
                                  #t
                                  (if completion-ch
                                      (cons completion-ch pending-completions)
                                      pending-completions))])))))

(when (not (package-change-handler-thread))
  (package-change-handler-thread (daemon-thread 'package-change-handler
                                                (lambda () (package-change-handler #f '())))))

(thread-send (package-change-handler-thread) 'upgrade) ;; switch to new code
