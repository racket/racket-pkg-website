#lang racket/base
(require racket/list
         (prefix-in http: net/http-easy)
         net/url
         pkg/lib
         (only-in racket/exn exn->string)
         "package-source.rkt")

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
  ;; Log network and HTTP client failures and return #f.
  (define-syntax-rule (ignore-network-errors body ...)
    (with-handlers ([(lambda (e) (or (exn:fail:network? e) (http:exn:fail:http-easy? e)))
                     (lambda (e)
                       (log-warning
                        "Network error retrieving possible readme for source URL ~a:\n~a"
                        s
                        (exn->string e))
                       #f)])
      (call-in-pkg-timeout-sandbox
       #:make-exn exn:fail:network
       (lambda ()
         ;; The sandbox can kill a request, so give it its own session.
         (parameterize ([http:current-session (http:make-session)])
           body ...)))))

  ;; URL -> (Option String)
  ;; Helper: Check for a "README.md" resource as a subresource of the
  ;; given URL. Return the README's URL if it is found; otherwise #f.
  (define (extant-readme-md-urlstring u)
    (ignore-network-errors
     (define readme-u (struct-copy url u
                        [path (append (url-path u) (list (path/param "README.md" '())))]))
     (log-info "Checking for readme at ~a ..." (url->string readme-u))
     (define response (http:head readme-u #:max-redirects 20))
     (and (<= 200 (http:response-status-code response) 299)
          (url->string readme-u))))

  ;; URL -> (Option String)
  ;; Helper: Retrieves the given resource and greps it for
  ;; id="readme", more or less, to determine whether there's a usable
  ;; fragment there.
  (define (horrible-readme-scraping-hack u)
    (ignore-network-errors
     (log-info "Checking for readme fragment at ~a ..." (url->string u))
     (define response (http:get u #:max-redirects 20))
     (and (<= 200 (http:response-status-code response) 299)
          (regexp-match? #px"(?i:id=.readme.)" (http:response-body response))
          (string-append (url->string u) "#readme"))))

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

(module+ test
  (require racket/async-channel
           racket/match
           rackunit
           web-server/http
           web-server/servlet-dispatch
           web-server/web-server)

  (test-case "README discovery follows a root-relative redirect on the fallback GET"
    (define (handler req)
      (match (list (request-method req) (url->string (request-uri req)))
        [(list #"HEAD" "/repo.git/README.md") (response/xexpr "missing" #:code 404)]
        [(list #"GET" "/repo.git") (redirect-to "/canonical" temporarily)]
        [(list #"GET" "/canonical") (response/xexpr '(div ([id "readme"]) "README"))]
        [_ (response/xexpr "unexpected request" #:code 400)]))

    (define ready (make-async-channel))
    (define stop
      (serve #:dispatch (dispatch/servlet handler)
             #:listen-ip "127.0.0.1" #:port 0 #:confirmation-channel ready))
    (after
     (let ([port (sync ready)])
       (when (exn? port) (raise port))
       (define repo-url (format "http://127.0.0.1:~a/repo.git" port))
       (check-equal? (source->readme-url repo-url) (string-append repo-url "#readme")))
     (stop))))
