#lang racket/base

(provide request-handler
         on-continuation-expiry
         rerender!)

(require racket/runtime-path)
(require racket/list)
(require racket/set)
(require racket/match)
(require racket/format)
(require racket/date)
(require racket/string)
(require racket/port)
(require (only-in racket/list filter-map drop-right))
(require (only-in racket/exn exn->string))
(require (except-in net/url http-sendrecv/url))
(require net/uri-codec)
(require (except-in web-server/servlet http-sendrecv/url))
(require json)
(require "gravatar.rkt")
(require "bootstrap.rkt")
(require "html-utils.rkt")
(require "packages.rkt")
(require "sessions.rkt")
(require "json-rpc.rkt")
(require reloadable)
(require "daemon.rkt")
(require "config.rkt")
(require "hash-utils.rkt")
(require "static.rkt")
(require "package-source.rkt")
(require "http-utils.rkt")

(define static-urlprefix
  (or (@ (config) static-urlprefix)
      ""))

(define dynamic-urlprefix
  (or (@ (config) dynamic-urlprefix)
      ""))

(define dynamic-static-urlprefix
  (or (@ (config) dynamic-static-urlprefix)
      ""))

(define disable-cache?
  (or (@ (config) disable-cache?)
      #f))

(define nav-index "Packages")
(define nav-search "Search")

(define (navbar-header)
  `(a ((href "http://www.racket-lang.org/"))
    (img ((src ,(static-resource-url "/logo-and-text.png"))
          (height "60")
          (alt "Racket Package Index")))))

(define backend-baseurl
  (or (@ (config) backend-baseurl)
      "https://pkgd.racket-lang.org"))

(define default-empty-parsed-package-source
  (git-source "git://github.com/" #f 'git 'git "github.com" #f "" "" ""))

(define COOKIE "pltsession")

(define recent-seconds
  (or (@ (config) recent-seconds)
      (* 2 24 60 60))) ;; two days

(define pkg-build-baseurl
  (or (@ (config) pkg-build-baseurl)
      "http://pkg-build.racket-lang.org/"))

(struct draft-package (old-name name description authors tags versions) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (request-handler relative-named-url)
  (dispatch-rules
   [("") main-page]
   [("search") search-page]
   [("package" (string-arg)) package-page]
   [("package" (string-arg) "edit") edit-package-page]
   [("update-my-packages") update-my-packages-page]
   [("update-package-ring" (string-arg) (integer-arg)) #:method "post" update-package-ring-page]
   [("not-found") not-found-page]
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
  (with-site-config
    (bootstrap-continuation-expiry-handler request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send/suspend/dispatch/dynamic proc)
  (send/suspend/dispatch
   (lambda (embed-url)
     (proc (lambda args (string-append dynamic-urlprefix (apply embed-url args)))))))

(define (send/suspend/dynamic proc)
  (send/suspend
   (lambda (k-url)
     (proc (string-append dynamic-urlprefix k-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-url . args)
  (string-append dynamic-urlprefix (apply relative-named-url args)))

(define (static-resource-url suffix)
  (if (rendering-static-page?)
      (string-append static-urlprefix suffix)
      (string-append dynamic-static-urlprefix suffix)))

(define-syntax-rule (authentication-wrap #:request request body ...)
  (authentication-wrap* #f request (lambda () body ...)))

(define-syntax-rule (authentication-wrap/require-login #:request request body ...)
  (authentication-wrap* #t request (lambda () body ...)))

(define-syntax-rule (with-site-config body ...)
  (parameterize ((bootstrap-navbar-header (navbar-header))
                 (bootstrap-head-extra
                  `((link ((rel "alternate")
                           (type "application/atom+xml")
                           (title "Atom Feed")
                           (href ,(static-resource-url "/atom.xml"))))))
                 (bootstrap-navigation
                  `((,nav-index ,(main-page-url))
                    ("Documentation" "https://docs.racket-lang.org/")
                    (,nav-search ,(named-url search-page))
                    ("About"
                     (("The Racket Package System"
                       "http://docs.racket-lang.org/pkg/getting-started.html")
                      ("Package Builds" "https://pkg-build.racket-lang.org/about.html")))
                    ((div ,(glyphicon 'download-alt)
                          " Download Racket")
                     "http://download.racket-lang.org/")
                    ))
                 (bootstrap-static-urlprefix
                  (if (rendering-static-page?)
                      static-urlprefix
                      dynamic-static-urlprefix))
                 (bootstrap-dynamic-urlprefix
                  dynamic-urlprefix)
                 (bootstrap-inline-js
                  (string-append (format "PkgSiteDynamicBaseUrl = '~a';" dynamic-urlprefix)
                                 (format "PkgSiteStaticBaseUrl = '~a';" static-urlprefix)
                                 (format "IsStaticPage = ~a;" (if (rendering-static-page?)
                                                                  "true"
                                                                  "false")))))
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
      (login-or-register-flow* (string-append dynamic-urlprefix requested-url) login-form)
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
                                      (li (a ((href ,(named-url update-my-packages-page)))
                                             ,(glyphicon 'refresh) " Rescan all my packages"))
                                      (li ((class "divider")))
                                      (li (a ((href ,(named-url edit-package-page)))
                                             ,(glyphicon 'plus-sign) " New package"))
                                      (li (a ((href ,(tags-page-url
                                                      (list
                                                       (format "author:~a"
                                                               (session-email session))))))
                                             ,(glyphicon 'user) " My packages"))
                                      (li ((class "divider")))
                                      (li (a ((href
                                               ,(login-or-register-url
                                                 requested-url
                                                 (named-url logout-page))))
                                             ,(glyphicon 'log-out) " Log out"))))))]))
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
          (alist->form-urlencoded (list (cons 'k (string-append dynamic-urlprefix k))))))

(define (login-or-register-flow request thunk)
  (define-form-bindings request ([k (named-url main-page)]))
  (define session (request->session request))
  (if session
      (with-site-config
        (bootstrap-redirect k))
      (login-or-register-flow* k thunk)))

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
   (send/suspend/dispatch/dynamic
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
  (simple-json-rpc! #:sensitive? #t
                    #:include-credentials? #f
                    backend-baseurl
                    "/api/authenticate"
                    (hash 'email email
                          'passwd password
                          'code code)))

(define (create-session-from-authentication-success! email password success)
  ;; An "authentication success" is either #t, signalling a new user,
  ;; or a hash-table with interesting facts in it.
  (define user-facts (cond [(eq? success #t) (hasheq)]
                           [(hash? success) success]
                           [else (log-warning "Bad auth success for user ~v: ~v" email success)
                                 (hasheq)]))
  (create-session! email password
                   #:curator? (if (hash-ref user-facts 'curation #f) #t #f)
                   #:superuser? (if (hash-ref user-facts 'superuser #f) #t #f)))

(define (process-login-credentials request)
  (define-form-bindings request (email password))
  (if (or (equal? (string-trim email) "")
          (equal? (string-trim password) ""))
      (login-form "Please enter your email address and password.")
      (match (authenticate-with-server! email password "")
        [(or "wrong-code" (? eof-object?))
         (login-form "Something went awry; please try again.")]
        [(or "emailed" #f)
         (summarise-code-emailing "Incorrect password, or nonexistent user." email)]
        [success
         (create-session-from-authentication-success! email password success)])))

(define (register-form #:email [email ""]
                       #:code [code ""]
                       #:error-message [error-message #f])
  (with-site-config
   (send/suspend/dispatch/dynamic
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
      [(? eof-object?)
       (retry "Something went awry. Please try again.")]
      ["wrong-code"
       (retry "The code you entered was incorrect. Please try again.")]
      [(or "emailed" #f)
       (retry "Something went awry; you have been emailed another code. Please check your email.")]
      [success
       ;; The email and password combo we have been given is good to go.
       ;; Set a cookie and consider ourselves logged in.
       (create-session-from-authentication-success! email password success)])]))

(define (notify-of-emailing request)
  (define-form-bindings request (email_for_code))
  (authenticate-with-server! email_for_code "" "") ;; TODO check result?
  (summarise-code-emailing "Account registration/reset code emailed" email_for_code))

(define (summarise-code-emailing reason email)
  (with-site-config
   (send/suspend/dispatch/dynamic
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
      (format "~a/index.html" static-urlprefix)
      (named-url main-page)))

(define (view-package-url package-name)
  (define package-name-str (~a package-name))
  (if (use-cache?)
      (format "~a~a" static-urlprefix (relative-named-url package-page package-name-str))
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
       (href ,(format "~a~a" pkg-build-baseurl url-suffix))) ,label))

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

(define (get-implied-by-table package-names)
  (for/fold ([ht (hash)])
            ([pkg (package-batch-detail package-names)])
    (define pkg-name (package-name pkg))
    (define implied-names (package-implies pkg))
    (for/fold ([ht ht])
              ([implied-name implied-names])
      (define current-names (hash-ref ht implied-name set))
      (hash-set ht implied-name (set-add current-names pkg-name)))))

(define (get-implied-docs pkg)
  (define implied-names (map string->symbol (package-implies pkg)))
  (append-map package-docs (package-batch-detail implied-names)))

(define (get-docs/implied-docs pkg)
  (append (package-docs pkg) (get-implied-docs pkg)))

;; gathers the docs of all packages which imply this package
;; by calling get-docs/implied-docs on them.
(define (get-implied-by-docs pkg implied-by-tab)
  (define implied-by-names
    (set-map string->symbol (hash-ref implied-by-tab (package-name pkg) set)))
  (append-map get-docs/implied-docs
              (package-batch-detail (set->list implied-by-names))))

(define (get-all-docs pkg implied-by-tab)
  (remove-duplicates
   (append (get-docs/implied-docs pkg)
           (get-implied-by-docs pkg implied-by-tab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package hashtable getters.
;; TODO factor this stuff out into a proper data structure

;; Mandatory -- never #f
(define (package-name pkg)                   (@ pkg name))

;; Optional -- sometimes #f
(define (package-build-failure-log pkg)      (@ pkg build failure-log))
(define (package-build-success-log pkg)      (@ pkg build success-log))
(define (package-build-test-failure-log pkg) (@ pkg build test-failure-log))
(define (package-build-test-success-log pkg) (@ pkg build test-success-log))
(define (package-build-dep-failure-log pkg)  (@ pkg build dep-failure-log))
(define (package-build-conflicts-log pkg)    (@ pkg build conflicts-log))
(define (package-ring pkg)                   (@ pkg ring))
(define (package-checksum-error pkg)         (@ pkg checksum-error))

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
(define (package-implies pkg)           (or (@ pkg implies) '()))
(define (package-modules pkg)           (or (@ pkg modules) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (package-summary-table package-names)
  (define-values (pkg-rows num-todos)
    (build-pkg-rows/num-todos package-names))
  `(table
    ((class "packages sortable") (data-todokey ,(number->string num-todos)))
    (thead
     (tr
      (th 'nbsp)
      (th "Package")
      (th "Description")
      (th "Build")
      (th ((style "display: none")) 'nbsp))) ;; todokey
    (tbody
     ,@(maybe-splice (null? package-names)
                     `(tr (td ((colspan "4"))
                              (div ((class "alert alert-info"))
                                   "No packages found."))))
     ,@pkg-rows)))

(define (build-pkg-rows/num-todos package-names)
  ;; Builds the list of rows in the package table as an x-exp.
  ;; Also returns the total number of non-zero todo keys,
  ;; representing packages with outstanding build errors or
  ;; failing tests, or which are missing docs or tags.
  (define now (/ (current-inexact-milliseconds) 1000))
  (define implied-by-tab (get-implied-by-table package-names))
  (define-values (pkg-rows num-todos)
    (for/fold ([pkg-rows null] [num-todos 0])
              ([pkg (package-batch-detail package-names)])
      (define pkg-docs (get-all-docs pkg implied-by-tab))
      (define has-docs? (pair? pkg-docs))
      (define has-readme? (pair? (package-readme-url pkg)))
      (define has-tags? (pair? (package-tags pkg)))
      (define has-desc? (not (string=? "" (package-description pkg))))
      (define todokey
        (cond [(package-build-failure-log pkg) 5]
              [(package-build-test-failure-log pkg) 4]
              [(not (or has-docs? has-readme?)) 3]
              [(not has-desc?) 2]
              [(not has-tags?) 1]
              [else 0]))
      (define row-xexp
        `(tr
          ((data-todokey ,(number->string todokey)))
          (td (span ((class "last-updated-negated") (style "display: none"))
                    ,(~a (- (package-last-updated pkg))))
              ,@(maybe-splice
                 (< (- now (package-last-updated pkg)) recent-seconds)
                 (label-p "label-info" "New"))
              ,@(maybe-splice
                 (> 0 todokey)
                 (label-p (if (< todokey 5)
                              "label-warning"
                              "label-danger") "Todo")))
          (td (h2 ,(package-link (package-name pkg)))
              ,(authors-list (package-authors pkg)))
          (td (p ,(if (string=? "" (package-description pkg))
                      `(span ((class "label label-warning")) "This package needs a description")
                      (package-description pkg)))
              ,(if (not (or has-docs? has-readme?))
                   (label-p "label-warning" "This package needs documentation")
                   `(div
                     (span ((class "doctags-label")) "Docs: ")
                     ,(doc-links pkg-docs)
                     ,@(maybe-splice has-readme?
                                     " "
                                     `(a ((href ,(package-readme-url pkg)))
                                         "README"))))
              ,(if (not has-tags?)
                   (label-p "label-warning" "This package needs tags")
                   `(div
                     (span ((class "doctags-label")) "Tags: ")
                     ,(tag-links (package-tags pkg)))))
          ,(build-status-td pkg)
          (td ((style "display: none")) ,(number->string todokey))))
      (values (cons row-xexp pkg-rows)
              (+ num-todos (min todokey 1)))))
  ;; for/fold reverses pkg-rows, so un-reverse before returning.
  (values (reverse pkg-rows) num-todos))

(define (label-p cls txt)
  `(p (span ((class ,(string-append "label " cls))) ,txt)))

(define (build-status-td pkg)
  ;; Build the index page cell for summarizing a package's build status.
  ;; Nothing at all is for no information on build success or failure.
  ;; Green is for build succeeded along with everything else.
  ;; Red is for build failed.
  ;; Yellow is for build succeeded, but some other problems exist.

  (define failure-log-url (package-build-failure-log pkg))
  (define success-log-url (package-build-success-log pkg))
  (define dep-failure-log-url (package-build-dep-failure-log pkg))
  (define test-failure-log-url (package-build-test-failure-log pkg))
  (define test-success-log-url (package-build-test-success-log pkg))

  (define td-class (cond [failure-log-url "build_red"]
                         [(not success-log-url) ""]
                         [(or dep-failure-log-url test-failure-log-url) "build_yellow"]
                         [else "build_green"]))

  `(td ((class ,td-class))
       ,@(for/list [(e (list (list failure-log-url "" "fails")
                             (list success-log-url "" "succeeds")
                             (list dep-failure-log-url "; has " "dependency problems")
                             (list test-failure-log-url "; has " "failing tests")))]
           (match-define (list u p l) e)
           (if u `(span ,p ,(buildhost-link u l)) `(span)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-page request)
  (parameterize ((bootstrap-active-navigation nav-index)
                 (bootstrap-page-scripts (list (static-resource-url "/searchbox.js")
                                               (static-resource-url "/index.js")
                                               (static-resource-url "/todos.js"))))
    (define package-name-list (package-search "" '((main-distribution #f)
                                                   (main-tests #f)
                                                   (deprecated #f))))
    (authentication-wrap
     #:request request
     (if (and (not (rendering-static-page?)) (use-cache?))
         (bootstrap-redirect (main-page-url))
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
                ,(format "~a packages" (length package-name-list))
                " "
                (a ((href ,(format "~a?q=%20" (named-url search-page)))) "(see all, including packages tagged as \"deprecated\", \"main-distribution\", or \"main-test\")"))
             (p ((class "package-count") (id "todo-msg")) "")
             ,(package-summary-table package-name-list))
           `(div ((class "jumbotron"))
                 (p "Questions? Comments? Bugs? Email "
                    (a ((href "mailto:tonyg@ccs.neu.edu")) "tonyg@ccs.neu.edu")
                    " or "
                    (a ((href "mailto:jay.mccarthy@gmail.com")) "jay.mccarthy@gmail.com")
                    ".")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-status-button buildhost-url str label-type glyphicon-type)
  (buildhost-link buildhost-url
                  `(span " " (span ((class ,(format "build-status-button label label-~a" label-type)))
                                   ,(glyphicon glyphicon-type) " " ,str))))

(define (dependencies->package-names deps)
  (filter-map (lambda (dep)
                (match dep
                  [(? string? package-name) package-name]
                  [(cons (? string? package-name) _) package-name]
                  [_
                   (log-warning "dependencies->package-names: unknown dependency format: ~v" dep)
                   #f]))
              deps))

(define (clamp-ring r)
  (max 0 (min 2 r)))

(define (ring-change-link pkg proposed-new-ring link-content)
  (define new-ring (clamp-ring proposed-new-ring))
  `(form ((role "form")
          (class "ring-change-link")
          (method "post")
          (action ,(named-url update-package-ring-page (~a (package-name pkg)) new-ring)))
         (button ((class "btn btn-danger btn-xs")
                  ,@(maybe-splice
                     (= new-ring (package-ring pkg))
                     `(disabled "disabled"))
                  (type "submit")) ,link-content)))

(define (not-found-page request [package-name-str #f])
  (authentication-wrap
   #:request request
   (bootstrap-response #:code 404
                       #:message #"Page not found"
                       "Page not found"
                       `(div "The page you requested does not exist.")
                       `(ul (li (a ((href ,(main-page-url)))
                                   "Return to the package index"))))))

(define (current-user-may-edit? pkg)
  (or (member (current-email) (package-authors pkg))
      (and (current-session)
           (session-superuser? (current-session)))))

(define (package-page request package-name-str)
  (define package-name (string->symbol package-name-str))
  (define pkg (package-detail package-name))
  (authentication-wrap
   #:request request
   (cond
     [(not pkg)
      (bootstrap-response #:code 404
                          #:message #"No such package"
                          "Package not found"
                          (if package-name-str
                              `(div "The package " (code ,package-name-str) " does not exist.")
                              `(div "The requested package does not exist."))
                          `(ul (li (a ((href ,(main-page-url)))
                                      "Return to the package index"))))]
     [(and (not (rendering-static-page?)) (use-cache?))
      (bootstrap-redirect (view-package-url package-name))]
     [else
      (let ((default-version (package-default-version pkg)))
        (bootstrap-response (~a package-name)
          #:title-element ""
          `(div ((class "jumbotron"))
                (h1 ,(~a package-name))
                (p ,(package-description pkg))
                (p ((class "build-status"))
                   "Build status: "
                   ,@(for/list [(e (list (list package-build-failure-log
                                               "failed" "danger" "fire")
                                         (list package-build-success-log
                                               "ok" "success" "ok")
                                         (list package-build-dep-failure-log
                                               "dependency problems" "warning" "question-sign")
                                         (list package-build-test-failure-log
                                               "failing tests" "warning" "question-sign")
                                         (list package-build-test-success-log
                                               "passing tests" "success" "ok")))]
                       (match-define (list url-proc str label-type glyphicon-type) e)
                       (define u (url-proc pkg))
                       (if (not u) `(span) (build-status-button u str label-type glyphicon-type))))
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
                               (href ,(package-source->human-tree-url (@ default-version source))))
                              ,(glyphicon 'link) " Code"))

                     ,@(maybe-splice
                        (current-user-may-edit? pkg)
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

          (match (package-checksum-error pkg)
            [#f ""]
            [err
             `(div ((class "alert alert-danger")
                    (role "alert"))
                   (p (span ((class "label label-danger"))
                            "Checksum error")
                      " An error occurred while updating"
                      " the package checksum.")
                   (pre ,err))])

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
                      (td ,(~a (or (package-ring pkg) "N/A"))
                          ,@(maybe-splice
                             (and (package-ring pkg)
                                  (current-session)
                                  (session-curator? (current-session)))
                             " "
                             (ring-change-link pkg (- (package-ring pkg) 1) 'blacktriangledown)
                             (ring-change-link pkg (+ (package-ring pkg) 1) 'blacktriangle))))
                  (tr (th "Conflicts")
                      (td ,(package-links (package-conflicts pkg))))
                  (tr (th "Dependencies")
                      (td ,(package-links
                            (dependencies->package-names
                             (package-dependencies pkg)))))
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
                              ,@(maybe-splice
                                 (package-build-test-failure-log pkg)
                                 `(li "Tests failed: "
                                      ,(buildhost-link (package-build-test-failure-log pkg)
                                                       "transcript")))
                              ,@(maybe-splice
                                 (package-build-test-success-log pkg)
                                 `(li "Tests succeeded: "
                                      ,(buildhost-link (package-build-test-success-log pkg)
                                                       "transcript")))
                              )))
                  ,@(let* ((vs (package-versions pkg))
                           (empty-checksum "9f098dddde7f217879070816090c1e8e74d49432")
                           (vs (for/hash (((k v) (in-hash vs))
                                          #:when (not (equal? (@ v checksum)
                                                              empty-checksum)))
                                 (values k v))))
                      (maybe-splice
                       (not (hash-empty? vs))
                       `(tr (th (a ([href "https://docs.racket-lang.org/pkg/getting-started.html#%28part._.Version_.Exceptions%29"]) "Version Exceptions"))
                            (td (table ((class "package-versions"))
                                       (tr (th "Version")
                                           (th "Source")
                                           (th "Checksum"))
                                       ,@(for/list
                                             (((version-sym v) (in-hash vs)))
                                           `(tr
                                             (td ,(~a version-sym))
                                             (td (a ((href ,(package-source->human-tree-url
                                                             (@ v source))))
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
                  )))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-package-page request [package-name-str ""])
  (authentication-wrap/require-login
   #:request request
   (define package-name (string->symbol package-name-str))
   (define pkg (package-detail package-name))
   (cond
    [(and pkg (not (current-user-may-edit? pkg)))
     ;; Exists, isn't ours, and we're not superuser. Show it instead.
     (bootstrap-redirect (view-package-url package-name))]
    [(not pkg)
     ;; Doesn't exist.
     (package-form #f (draft-package ""
                                     package-name-str
                                     ""
                                     (list (current-email))
                                     '()
                                     `(("default" ,default-empty-parsed-package-source))))]
    [else
     ;; Exists, and either ours or we are superuser.
     (package-form #f
                   (draft-package package-name-str
                                  package-name-str
                                  (package-description pkg)
                                  (package-authors pkg)
                                  (package-tags pkg)
                                  (for/list (((ver info) (in-hash (package-versions pkg))))
                                    (define-values (parsed complaints)
                                      (parse-package-source (@ info source)))
                                    (list (symbol->string ver) parsed))))])))

(define (package-source-option source-type value label)
  `(option ((value ,value)
            ,@(maybe-splice (equal? source-type value) '(selected "selected")))
    ,label))

(define (put-default-first alist)
  (define default (assoc "default" alist))
  (cons default (remove default alist)))

(define (package-form error-message draft)
  (with-site-config
   (send/suspend/dispatch/dynamic
    (lambda (embed-url)

      (define (build-versions-table)
        `(table ((class "package-versions"))
          (tr (th "Version")
              (th "Source"))
          ,@(for/list ((v (put-default-first
                           (draft-package-versions draft))))
              (match-define (list version parsed-source) v)
              (define (control-name c) (format "version__~a__~a" version c))
              (define (group-name c) (format "version__~a__~a__group" version c))
              (define (textfield name label-text value [placeholder ""])
                (row #:id (group-name name)
                     0 3
                     (and label-text (label (control-name name) label-text))
                     0 (if label-text 9 12)
                     (text-input (control-name name) value #:placeholder placeholder)))
              (define-values (source-type simple-url g-transport g-host+port g-repo g-commit g-path)
                (match parsed-source
                  [#f
                   (values "simple" "" "" "" "" "" "")]
                  [(simple-url-source u _ _)
                   (values "simple" u "" "" "" "" "")]
                  [(git-source _ _ _ tr host port repo c path)
                   (values "git"
                           ""
                           (symbol->string tr)
                           (match* (tr port)
                             [(_ #f) host]
                             [(http 80) host]
                             [(https 443) host]
                             [(git 9418) host]
                             [(_ _) (format "~a:~a" host port)])
                           repo
                           (match c
                             ["master" ""]
                             [_ c])
                           path)]))
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
                            ,(textfield "g_host_port" "Host" g-host+port)
                            ,(textfield "g_repo" "Repository" g-repo "user/repo")
                            ,(textfield "g_commit" "Branch or commit" g-commit "master")
                            ,(textfield "g_path" "Path within repository" g-path)
                            ,(row #:id (group-name "g_transport")
                                  0 3
                                  (label (control-name "g_transport") "Transport")
                                  0 9
                                  `(select ((id ,(control-name "g_transport"))
                                            (name ,(control-name "g_transport")))
                                           ,@(for/list [(t (list "git" "https" "http"))]
                                               `(option ((value ,t)
                                                         ,@(maybe-splice (equal? t g-transport)
                                                                         '(selected "selected")))
                                                        ,t)))))))))

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

      (parameterize ((bootstrap-page-scripts (list (static-resource-url "/editpackage.js"))))
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
   (send/suspend/dynamic
    (lambda (k-url)
      (bootstrap-response "Confirm Package Deletion"
                          `(div ((class "confirm-package-deletion"))
                            (h2 ,(format "Delete ~a?" package-name-str))
                            (p "This cannot be undone.")
                            (a ((class "btn btn-default")
                                (href ,k-url))
                               "Confirm deletion")))))
   (simple-json-rpc! backend-baseurl "/api/package/del" (hash 'pkg package-name-str))
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
                          [versions (cons (list new_version default-empty-parsed-package-source)
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
    (define g_transport (vg 'g_transport ""))
    (define g_host_port (vg 'g_host_port ""))
    (define g_repo0 (vg 'g_repo ""))
    (define g_repo (cond
                     [(regexp-match #rx"[.]git$" g_repo0) g_repo0]
                     [(equal? g_transport "git") g_repo0]
                     [else (string-append g_repo0 ".git")]))
    (define g_commit0 (vg 'g_commit ""))
    (define g_path (vg 'g_path ""))
    (define g_commit (if (equal? g_commit0 "") "master" g_commit0))
    (define-values (g_host g_port)
      (match (string-split g_host_port ":")
        [(list host) (values host #f)]
        [(list host (? string->number port)) (values host (string->number port))]
        [_ (values "" #f)]))
    (define source
      (match type
        ["simple" simple_url]
        ["git" (unparse-package-source (git-source "" #f #f
                                                   (string->symbol g_transport)
                                                   g_host
                                                   g_port
                                                   g_repo
                                                   g_commit
                                                   g_path))]))
    (define-values (parsed complaints) (parse-package-source source))
    parsed)
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
  (define source (unparse-package-source (cadr default-version)))
  (define versions (remove default-version versions/default))
  (define old-pkg (package-detail (string->symbol old-name)))
  (and (or (equal? old-name name)
           ;; Don't let renames stomp on existing packages
           (not (package-detail (string->symbol name))))
       (eq? #t (simple-json-rpc! backend-baseurl
                                 "/api/package/modify-all"
                                 (hash 'pkg old-name
                                       'name name
                                       'description description
                                       'source source
                                       'tags tags
                                       'authors authors
                                       'versions (unparse-versions versions))))
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

(define (unparse-versions draft-versions)
  (for/list ((v draft-versions))
    (match-define (list version parsed) v)
    (list version (unparse-package-source parsed))))

(define (friendly-versions draft-versions)
  (for/hash ((v draft-versions))
    (match-define (list version parsed) v)
    (values (string->symbol version)
            (hash 'checksum ""
                  'source (unparse-package-source parsed)
                  ;; N.B. the source_url setting here survives only while we have saved it
                  ;; locally, before the package server catches up! The package server
                  ;; uses its own version of this code and generates its own source_url.
                  ;; However, we ignore source_url from the package server now that
                  ;; parsed-package-source-human-tree-url can do better.
                  'source_url (parsed-package-source-human-tree-url parsed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-my-packages-page request)
  (authentication-wrap/require-login
   #:request request
   (simple-json-rpc! backend-baseurl "/api/update" (hash))
   (bootstrap-response "Refresh All My Packages"
                       `(div
                         (p "All packages where you are listed as an author are now being rescanned.")
                         (p "The results will be available after the next index refresh, which is "
                            "scheduled for " ,(utc->string (/ (next-fetch-deadline) 1000))))
                       `(ul (li (a ((href ,(main-page-url)))
                                   "Return to the package index"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-package-ring-page request package-name-str proposed-new-ring)
  (define new-ring (clamp-ring proposed-new-ring))
  (authentication-wrap/require-login
   #:request request
   (when (session-curator? (current-session))
     (when (simple-json-rpc! backend-baseurl
                             "/api/package/curate"
                             (hash 'pkg package-name-str
                                   'ring new-ring))
       (define old-pkg (package-detail (string->symbol package-name-str)))
       (let* ((new-pkg (hash-set old-pkg 'ring new-ring))
              (completion-ch (make-channel)))
         (replace-package! completion-ch old-pkg new-pkg)
         (channel-get completion-ch))))
   (bootstrap-redirect (view-package-url package-name-str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-page request)
  (parameterize ((bootstrap-active-navigation nav-search)
                 (bootstrap-page-scripts (list (static-resource-url "/searchbox.js")
                                               (static-resource-url "/todos.js"))))
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
                                       (p ((class "package-count") (id "todo-msg")) "")
                                       ,(package-summary-table package-name-list))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cors-json-response f)
  (response/output #:mime-type #"application/json"
                   #:headers (list (header #"Access-Control-Allow-Origin" #"*"))
                   f))

(define (json-search-completions request)
  (define completions (set-union (list->set (map ~a (all-package-names))) (all-formal-tags)))
  (cors-json-response(lambda (response-port) (write-json (set->list completions) response-port))))

(define (json-tag-search-completions request)
  (cors-json-response(lambda (response-port) (write-json (set->list (all-tags)) response-port))))

(define (json-formal-tags request)
  (cors-json-response (lambda (response-port)
                        (write-json (set->list (all-formal-tags)) response-port))))

(define (pkgs-all-json request)
  (cors-json-response (lambda (response-port) (write-json (packages-jsexpr) response-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: fold the collection of this information into the package
;; database itself.
(define (update-external-package-information! package-name)
  (define pkg (package-detail package-name))
  (define default-version (package-default-version pkg))
  (define external-information
    (and pkg
         (match (source->readme-url (@ default-version source))
           [#f (hash)]
           [readme-url
            (log-info "Package ~a has a readme at ~a" package-name readme-url)
            (hash 'readme-url readme-url)])))
  (set-package-external-information! package-name external-information))

;; String -> (Option String)
;;
;; Attempt to discover a "nearby" README for a given git "source URL",
;; operating on the assumption that we have a vaguely github-like
;; setup. We can do better here once we get a feel for what other
;; possibilities exist out there, and how we can abstract over them.
;;
(define (source->readme-url s)

  ;; SYNTAX
  ;; If exn:fail:network is raised, logs a warning and returns #f
  (define-syntax-rule (ignore-network-errors body ...)
    (with-handlers ([exn:fail:network?
                     (lambda (e)
                       (log-warning
                        "Network error retrieving possible readme for source URL ~a:\n~a"
                        s
                        (exn->string e))
                       #f)])
      body ...))

  ;; URL -> (Option String)
  ;; Helper: Check for a "README.md" resource as a subresource of the
  ;; given URL. Return the README's URL if it is found; otherwise #f.
  (define (extant-readme-md-urlstring u)
    (ignore-network-errors
     (define readme-u (struct-copy url u
                        [path (append (url-path u) (list (path/param "README.md" '())))]))
     (log-info "Checking for readme at ~a ..." (url->string readme-u))
     (match/values (http/simple-interpret-response
                    (http/follow-redirects
                     #"HEAD"
                     (http-sendrecv/url readme-u #:method #"HEAD")))
       [('success _headers _body) (url->string readme-u)]
       [(_ _ _) #f])))

  ;; URL -> (Option String)
  ;; Helper: Retrieves the given resource and greps it for
  ;; id="readme", more or less, to determine whether there's a usable
  ;; fragment there.
  (define (horrible-readme-scraping-hack u)
    (ignore-network-errors
     (log-info "Checking for readme fragment at ~a ..." (url->string u))
     (match/values (http/simple-interpret-response
                    (http/follow-redirects
                     #"GET"
                     (http-sendrecv/url u #:method #"GET")))
       [('success _headers body)
        (and (regexp-match? #px"(?i:id=.readme.)" body)
             (string-append (url->string u) "#readme"))]
       [(_ _ _) #f])))

  (define-values (p _complaints) (parse-package-source s))
  (and (git-source? p)
       ;; Search from the location given up into parent directories
       ;; until we reach the repo root.
       (let* ((root-p (struct-copy git-source p [path ""]))
              (root-u (string->url (parsed-package-source-human-tree-url root-p)))
              (here-u (string->url (parsed-package-source-human-tree-url p))))
         (and (member (url-scheme here-u) (list "http" "https"))
              (let loop ((here-u here-u))
                ;; Strategy: Try to directly retrieve "README.md"
                ;; first. In principle, we could/should try other
                ;; github-supported READMEish names here, but if this
                ;; first check fails we go for a horrible
                ;; content-scraping strategy instead.
                (or (extant-readme-md-urlstring here-u)
                    (horrible-readme-scraping-hack here-u)
                    (and (not (equal? here-u root-u))
                         (loop (struct-copy url here-u
                                 [path (drop-right (url-path here-u) 1)])))))))))

(define (rerender! items-to-rerender)
  (thread-send (package-change-handler-thread) (list 'rerender! items-to-rerender)))

(define (internal:rerender-not-found!)
  ;; TODO: general-purpose error page instead.
  (static-render! #:mime-type "text/html"
                  relative-named-url not-found-page
                  #:ignore-response-code? #t)
  (log-info "Generating .htaccess")
  (static-put-file! "/.htaccess"
                    (string->bytes/utf-8
                     (format "ErrorDocument 404 ~a~a\n"
                             static-urlprefix
                             (relative-named-url not-found-page)))
                    "text/plain")
  (static-finish-update!))

(define (package-change-handler packages-to-render-before-issuing-completions ;; Setof Symbol
                                pending-completions ;; Listof (Channelof Void)
                                packages-to-render-in-idle-moments ;; Setof Symbol
                                )
  ;; In order for this daemon to stay responsive, I have changed its implementation to
  ;; avoid long-running tasks (such as refreshing every package!) between checks of the
  ;; mailbox.

  ;; Symbol String -> Void
  ;; Produces a static rendering of the named package.
  (define (rerender-package! p priority)
    (log-info "rerendering package ~a at ~a priority, ~a high and ~a low left to do with ~a waiters"
              p
              priority
              (set-count packages-to-render-before-issuing-completions)
              (set-count packages-to-render-in-idle-moments)
              (length pending-completions))
    (update-external-package-information! p)
    (static-render! #:mime-type "text/html"
                    relative-named-url
                    package-page
                    (symbol->string p)))

  ;; -> Void
  (define (rerender-index-and-flush!)
    (static-render! #:mime-type "text/html"
                    relative-named-url main-page
                    #:filename "/index.html")
    (static-render! #:mime-type "application/json"
                    relative-named-url json-search-completions)
    (static-finish-update!))

  ;; -> (U #f (-> Nothing))
  ;; Yield #f if no work can be done without an incoming message, or a procedure which
  ;; does some work and tail-calls package-change-handler otherwise.
  (define (compute-work-step)
    (cond
      [(not (set-empty? packages-to-render-before-issuing-completions))
       (lambda ()
         (define p (set-first packages-to-render-before-issuing-completions))
         (rerender-package! p "high")
         (package-change-handler (set-remove packages-to-render-before-issuing-completions p)
                                 pending-completions
                                 packages-to-render-in-idle-moments))]
      [(not (null? pending-completions))
       (lambda ()
         (rerender-index-and-flush!)
         (for ((completion-ch pending-completions))
           (channel-put completion-ch (void)))
         (package-change-handler packages-to-render-before-issuing-completions
                                 '()
                                 packages-to-render-in-idle-moments))]
      [(not (set-empty? packages-to-render-in-idle-moments))
       (lambda ()
         (define p (set-first packages-to-render-in-idle-moments))
         (define remaining-packages-to-render-in-idle-moments
           (set-remove packages-to-render-in-idle-moments p))
         (rerender-package! p "low")
         (when (set-empty? remaining-packages-to-render-in-idle-moments)
           (rerender-index-and-flush!))
         (package-change-handler packages-to-render-before-issuing-completions
                                 pending-completions
                                 remaining-packages-to-render-in-idle-moments))]
      [else
       #f]))

  ;; Any -> Nothing
  ;; Processes an incoming message, updating state and tailcalling package-change-handler.
  (define (handle-message message)
    (match message
      ['upgrade ;; Happens every time site.rkt is reloaded
       (internal:rerender-not-found!)
       (package-change-handler packages-to-render-before-issuing-completions
                               pending-completions
                               packages-to-render-in-idle-moments)]
      [(list 'rerender! items-to-rerender)
       (log-info "rerender! ~v" items-to-rerender)
       (define packages-to-rerender
         (list->seteq (if items-to-rerender
                          (filter symbol? items-to-rerender)
                          (all-package-names))))
       (package-change-handler packages-to-render-before-issuing-completions
                               pending-completions
                               (set-union packages-to-render-in-idle-moments
                                          packages-to-rerender))]
      [(list 'package-changed completion-ch package-name)
       (if completion-ch
           (package-change-handler (set-add packages-to-render-before-issuing-completions package-name)
                                   (cons completion-ch pending-completions)
                                   packages-to-render-in-idle-moments)
           (package-change-handler packages-to-render-before-issuing-completions
                                   pending-completions
                                   (set-add packages-to-render-in-idle-moments package-name)))]))

  ;; Wait for an event, which will be either the readiness of a pending work item or the
  ;; arrival of a new message (which will add to our sets of ready pending work items).
  (sync/timeout (compute-work-step)
                (handle-evt (thread-receive-evt)
                            (lambda (_)
                              (handle-message (thread-receive))))))

(when (not (package-change-handler-thread))
  (package-change-handler-thread (daemon-thread 'package-change-handler
                                                (lambda () (package-change-handler (seteq)
                                                                                   '()
                                                                                   (seteq))))))

(thread-send (package-change-handler-thread) 'upgrade) ;; switch to new code
