#lang racket/base
(require web-server/http
         web-server/dispatch
         racket/file
         racket/port
         racket/system
         racket/match
         racket/set
         json
         racket/date
         net/url
         xml
         racket/list
         racket/path
         racket/promise
         plt-service-monitor/beat
         infrastructure-userdb/display-name
         "basic.rkt"
         "common.rkt"
         "notify.rkt"
         "s3.rkt")

(define convert-to-json
  (match-lambda
   [(? list? l)
    (map convert-to-json l)]
   [(? string? s)
    s]
   [(? hash? ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (convert-to-json-key k)
              (convert-to-json v)))]
   [(? number? n)
    n]
   [(? boolean? b)
    b]
   [(? symbol? s)
    (symbol->string s)]
   [(? keyword? s)
    (hasheq 'kw (keyword->string s))]
   [(? regexp? r)
    (hasheq 'regexp (object-name r))]
   [x
    (error 'convert-to-json "~e" x)]))

(define convert-to-json-key
  (match-lambda
   [(? string? s)
    (string->symbol s)]
   [(? symbol? s)
    s]
   [x
    (error 'convert-to-json-key "~e" x)]))

(define (file->value* p dv)
  (if (file-exists? p)
      (file->value p)
      dv))

;; From pkg-build/summary
(struct doc/main (name path) #:prefab)
(struct doc/extract (name path) #:prefab)
(struct doc/salvage (name path) #:prefab)
(struct doc/none (name) #:prefab)
(struct conflicts/indirect (path) #:prefab)

(define (generate-static pkgs)
  (define all-pkg-list (package-list))
  (define these-pkg-list pkgs)
  (define pkg-ht (make-hash))
  (define build-summary
    (let* ([bs (file->value* SUMMARY-PATH #f)]
           [bs (and (hash? bs) bs)]
           [bs (or bs (hash))])
      bs))

  (for ([pkg-name (in-list all-pkg-list)])
    (log! "static: building ht for ~v" pkg-name)
    (define ht (read-package-info pkg-name))

    (define versions-ht
      (hash-set (hash-ref ht 'versions (hash))
                'default
                (hasheq 'source (hash-ref ht 'source "")
                        'checksum (hash-ref ht 'checksum ""))))

    (hash-set!
     pkg-ht pkg-name
     (hash-set* ht
                'name pkg-name
                'source (hash-ref ht 'source "")
                'checksum (hash-ref ht 'checksum "")
                'last-updated (hash-ref ht 'last-updated (current-seconds))
                'last-checked (hash-ref ht 'last-checked (current-seconds))
                'last-edit (hash-ref ht 'last-edit (current-seconds))
                'versions versions-ht
                'ring (hash-ref ht 'ring 2)
                'collection (hash-ref ht 'collection #f)
                'dependencies (hash-ref ht 'dependencies empty)
                'rt-dependencies (hash-ref ht 'rt-dependencies empty)
                'license (hash-ref ht 'license #f)
                'implies (hash-ref ht 'implies empty)
                'modules (hash-ref ht 'modules empty)
                'conflicts
                (hash-ref ht 'conflicts
                          (λ ()
                            (set! these-pkg-list (cons pkg-name these-pkg-list))
                            empty))
                'tags (hash-ref ht 'tags empty)
                'author (hash-ref ht 'author "")
                'authors (author->list (hash-ref ht 'author "")))))

  (define (package-info pn)
    (hash-ref pkg-ht pn))

  (define (format-time s)
    (if s
        (with-handlers ([exn:fail? (λ (x) "")])
          (parameterize ([date-display-format 'iso-8601])
            (date->string (seconds->date s #f) #t)))
        ""))

  (define (module-lists-conflict? left right)
    (define seen? (make-hash))
    (for ([l (in-list left)])
      (hash-set! seen? l #t))
    (for/or ([r (in-list right)])
      (hash-ref seen? r #f)))

  (define (string-min x y)
    (if (string<=? x y)
        x
        y))

  (define (string-max x y)
    (if (string<? x y)
        y
        x))

  (define (packages-conflict? left right)
    (log! "static: computing conflict between ~v and ~v" left right)
    (define left-i (package-info left))
    (define right-i (package-info right))
    (define left-m (and left-i (hash-ref left-i 'modules #f)))
    (define right-m (and right-i (hash-ref right-i 'modules #f)))
    (if (and left-m right-m)
        (module-lists-conflict? left-m right-m)
        ;; We have to say #t here because otherwise things with no
        ;; information won't be conflicting.
        #t))

  (define conflict-cache
    (make-hash))

  (define (packages-conflict?/cache left right)
    (define smin (string-min left right))
    (define smax (string-max left right))
    (hash-ref! conflict-cache
               (cons smin smax)
               (λ ()
                 (packages-conflict? smin smax))))

  (log! "static: computing ring-01")
  (define ring-01
    (filter (λ (p) (member (hash-ref (package-info p) 'ring) '(0 1))) all-pkg-list))

  (define (package-conflicts? pkg)
    (filter (λ (other-pkg)
              (if (equal? pkg other-pkg)
                  #f
                  (packages-conflict?/cache pkg other-pkg)))
            ring-01))

  (define changed-pkg-set (list->set these-pkg-list))
  (log! "static: computing conflicts")
  (define (compute-conflicts! some-pkgs)
    (log! "static: computing conflicts: new round")
    (define more-set (list->set '()))
    (for ([pkg (in-list some-pkgs)])
      (log! "static: computing conflicts for ~v" pkg)
      (hash-update!
       pkg-ht pkg
       (λ (ht)
         (define conflicts (package-conflicts? pkg))
         (set! more-set (set-union more-set (list->set conflicts)))
         (let ()
           (package-info-set! pkg
                              (hash-set (read-package-info pkg)
                                        'conflicts conflicts)))
         (hash-set ht 'conflicts conflicts))))
    (set! more-set (set-subtract more-set changed-pkg-set))
    (unless (set-empty? more-set)
      (set! changed-pkg-set (set-union changed-pkg-set more-set))
      (compute-conflicts! (set->list more-set))))
  (compute-conflicts! these-pkg-list)
  (define changed-pkg-list (set->list changed-pkg-set))

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
            [path (list* user repo (path/param "tree" empty) branch path)]))]
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
            [path (list user repo (path/param "tree" empty)
                        (path/param "master" empty))]))]
         [_
          pkg-url-str])]
      [_
       pkg-url-str]))

  (for ([pkg (in-list all-pkg-list)])
    (log! "static: computing detailed ht for ~v" pkg)
    (define pb (hash-ref build-summary pkg #f))
    (define (pbl k)
      (and pb (hash-ref pb k #f)))

    (hash-update!
     pkg-ht pkg
     (λ (ht)
       (define conflicts (hash-ref ht 'conflicts))
       (hash-set*
        ht
        'build
        (hash 'min-failure-log (pbl 'min-failure-log)
              'success-log (pbl 'success-log)
              'test-failure-log (pbl 'test-failure-log)
              'test-success-log (pbl 'test-success-log)
              'conflicts-log
              (match (pbl 'conflicts-log)
                [#f #f]
                [(? path-string? f) f]
                [(conflicts/indirect file)
                 (list "indirect" file)])
              'dep-failure-log (pbl 'dep-failure-log)
              'docs
              (for/list ([d (in-list (or (pbl 'docs) empty))])
                (match d
                  [(doc/main n p) (list "main" n p)]
                  [(doc/extract n p) (list "extract" n p)]
                  [(doc/salvage n p) (list "salvage" n p)]
                  [(doc/none n) (list "none" n)]))
              'failure-log (pbl 'failure-log))
        'versions
        (for/hash ([(v vht) (in-hash (hash-ref ht 'versions))])
          (values v
                  (with-handlers ([exn:fail? (λ (x) vht)])
                    (hash-set vht 'source_url
                              (package-url->useful-url (hash-ref vht 'source))))))
        'search-terms
        (let* ([st (for/fold ([st #hasheq()])
                             ([t (in-list (hash-ref ht 'tags))])
                     (hash-set st (string->symbol t) #t))]
               [st (hash-set
                    st
                    (string->symbol
                     (format "ring:~a" (hash-ref ht 'ring))) #t)]
               [st (for*/fold ([st st])
                             ([a (in-list (author->list (hash-ref ht 'author)))]
                              [tag (in-list (display-name-tags (email->display-name a)))])
                     (hash-set
                      st tag #t))]
               [st (if (empty? (hash-ref ht 'tags))
                       (hash-set st ':no-tag: #t)
                       st)]
               [st (if (hash-ref ht 'checksum-error #f)
                       (hash-set st ':error: #t)
                       st)]
               [st (if (equal? "" (hash-ref ht 'description ""))
                       (hash-set st ':no-desc: #t)
                       st)]
               [st (if (empty? conflicts)
                       st
                       (hash-set st ':conflicts: #t))]
               [st (if (pbl 'success-log)
                       (hash-set st ':build-success: #t)
                       st)]
               [st (if (pbl 'failure-log)
                       (hash-set st ':build-fail: #t)
                       st)]
               [st (if (pbl 'dep-failure-log)
                       (hash-set st ':build-dep-fail: #t)
                       st)]
               [st (if (pbl 'conflicts-log)
                       (hash-set st ':build-conflicts: #t)
                       st)]
               [pb-docs (pbl 'docs)]
               [st (if (and pb-docs (cons? pb-docs)
                            (andmap (λ (d)
                                      (or (doc/main? d)
                                          (doc/extract? d)
                                          (doc/salvage? d)))
                                    pb-docs))
                       (hash-set st ':docs: #t)
                       st)]
               [st (if (and pb-docs (cons? pb-docs)
                            (andmap (λ (d)
                                      (or (doc/extract? d)
                                          (doc/salvage? d)
                                          (doc/none? d)))
                                    pb-docs))
                       (hash-set st ':docs-error: #t)
                       st)])
          st)))))

  (define basic-dispatch
    (pkg-index/basic
     (λ () all-pkg-list)
     (λ (pkg-name) (hash-ref pkg-ht pkg-name))))

  (define (page/atom.xml req)
    (define ps
      (sort (map package-info all-pkg-list)
            >
            #:key (λ (i) (hash-ref i 'last-updated))))
    (define top (hash-ref (if (pair? ps) (first ps) (hash)) 'last-updated 0))
    (define (atom-format-time t)
      (format "~aZ" (format-time t)))
    (response/xexpr
     #:mime-type #"application/atom+xml"
     `(feed
       ([xmlns "http://www.w3.org/2005/Atom"])
       (title ,(cdata #f #f (format "<![CDATA[~a]]>"
                                    "Racket Package Updates")))
       (link ([href ,(get-config atom-self-link
                                 ;; TODO: is the following the correct URL to default to?
                                 "https://pkg.racket-lang.org/rss")]
              [rel "self"]))
       (link ([href ,(get-config atom-link "https://pkg.racket-lang.org/")]))
       (updated ,(atom-format-time top))
       (id ,(get-config atom-id "https://pkg.racket-lang.org/"))
       ,@(for/list ([i (in-list ps)])
           (define p (hash-ref i 'name))
           (define this-url
             ((get-config atom-compute-package-url
                          (lambda (p)
                            (format (get-config atom-package-url-format-string
                                                "http://pkg.racket-lang.org/#[~a]")
                                    p)))
              p))
           (define lu (atom-format-time (hash-ref i 'last-updated)))
           (define a 
             (match (author->list (hash-ref i 'author))
               [(cons a _) a]
               ['() "nobody"]))
           (match-define (regexp #rx"^([^@]+)" (list _ n)) a)
           `(entry
             (title ([type "html"])
                    ,(cdata #f #f (format "<![CDATA[~a]]>" p)))
             (link ([href ,this-url]))
             (updated ,lu)
             (author (name ,n) (email ,a))
             (id ,this-url)
             (content
              ([type "html"])
              ,(cdata #f #f
                      (format "<![CDATA[~a]]>"
                              (xexpr->string
                               `(div
                                 (p ,(format "~a package updated on ~a."
                                             p lu))
                                 (p ,(format
                                      "Checksum: ~a"
                                      (hash-ref (hash-ref (hash-ref i 'versions (hash))
                                                          'default (hasheq))
                                                'checksum "")))
                                 (p ,(hash-ref i 'description ""))))))))))))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [("atom.xml") page/atom.xml]
     [else basic-dispatch]))

  (define (url->request u)
    (make-request #"GET" (string->url u) empty
                  (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))

  (define (cache url file)
    (define p (build-path static-path file))
    (make-directory* (path-only p))

    (define bs
      (with-output-to-bytes
          (λ ()
            ((response-output (main-dispatch (url->request url)))
             (current-output-port)))))
    (unless (and (file-exists? p)
                 (bytes=? bs (file->bytes p))
                 (file-exists? (path-add-suffix p #".json")))
      (log! "static: caching ~v" p)
      (with-output-to-file p
        #:exists 'replace
        (λ () (display bs)))
      (with-output-to-file (path-add-suffix p #".json")
        #:exists 'replace
        (λ () (write-json (convert-to-json (file->value p))))))
    (void))

  (define (copy-directory/files+ src dest)
    (cond
     [(directory-exists? src)
      (cond [(directory-exists? dest)
             (void)]
            [(file-exists? dest)
             (error 'copy-directory/files+ "Can't copy dir ~v to file ~v" src dest)]
            [else
             (make-directory dest)])
      (for ([p (in-list (directory-list src))])
        (copy-directory/files+ (build-path src p) (build-path dest p)))]
     [(file-exists? src)
      (cond [(directory-exists? dest)
             (error 'copy-directory/files+ "Can't copy file ~v to dir ~v" src dest)]
            [(file-exists? dest)
             (copy-file src dest #t)]
            [else
             (copy-file src dest)])
      (file-or-directory-modify-seconds
       dest
       (file-or-directory-modify-seconds src))]
     [else
      (error 'copy-directory/files+ "Unknown kind of source ~v" src)]))

  (log! "static: copying ~v to ~v" static.src-path static-path)
  (copy-directory/files+
   static.src-path
   static-path)

  ;; xxx
  (set! changed-pkg-list all-pkg-list)

  (log! "static: caching database")
  (cache "/atom.xml" "atom.xml")
  (cache "/pkgs" "pkgs")
  (cache "/pkgs-all" "pkgs-all")
  (for ([p (in-list changed-pkg-list)])
    (cache (format "/pkg/~a" p) (format "pkg/~a" p)))

  (let ()
    (log! "static: removing deleted files")
    (define pkg-path (build-path static-path "pkg"))
    (when (not (directory-exists? pkg-path)) (make-directory pkg-path))
    (for ([f (in-list (directory-list pkg-path))]
          #:unless (regexp-match #"json$" (path->string f))
          #:unless (member (path->string f) all-pkg-list))
      (log! "static: removing ~v" f)
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file (build-path pkg-path f))
        (delete-file (build-path pkg-path (path-add-suffix f #".json"))))))

  changed-pkg-list)

(define (do-static pkgs)
  (notify! "update upload being computed: the information below may not represent all recent changes and updates")
  (define changed-pkgs (generate-static pkgs))
  (signal-s3! changed-pkgs))
(define (run-static! pkgs)
  (run! do-static pkgs)
  (heartbeat (get-config beat-upload-task-name "pkgd-upload")))
(define run-sema (make-semaphore 1))
(define (signal-static! pkgs)
  (safe-run! run-sema (λ () (run-static! pkgs))))

(provide do-static
         signal-static!)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "static"
   #:args pkgs
   (do-static pkgs)))
