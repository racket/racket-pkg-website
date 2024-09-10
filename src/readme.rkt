#lang racket/base
(require racket/match
         racket/list
         net/url
         (only-in racket/exn exn->string)
         "package-source.rkt"
         "http-utils.rkt")

(provide source->readme-url)

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
                     (custom-http-sendrecv/url readme-u #:method #"HEAD")))
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
                     (custom-http-sendrecv/url u #:method #"GET")))
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

