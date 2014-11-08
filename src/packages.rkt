#lang racket/base

(provide @
         @ref
         all-package-names
         all-tags
         sorted-package-names
         package-detail
         package-search
         replace-package!
         delete-package!
         refresh-packages!
         next-fetch-deadline)

(require json)
(require racket/set)
(require racket/match)
(require racket/port)
(require racket/string)
(require racket/list)
(require web-server/private/gzip)
(require (only-in web-server/private/util exn->string))
(require net/url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax @
  (syntax-rules ()
    [(_ v) v]
    [(_ v k rest ...) (@ (@ref v 'k) rest ...)]))

(define (@ref v k)
  (and v (hash-ref v k (lambda () #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define package-index-url "http://pkgs.racket-lang.org/pkgs-all.json.gz")
(define package-fetch-interval (* 300 1000)) ;; 300 seconds = 300,000 milliseconds = 5 minutes
(define base-bogus-timeout (* 5 1000)) ;; 5 seconds

(define (fetch-remote-packages)
  (log-info "Fetching package list from ~a" package-index-url)
  (define result
    (with-handlers ((exn:fail? (lambda (e) #f)))
      (define response-bytes (port->bytes (get-pure-port (string->url package-index-url))))
      (define decompressed (gunzip/bytes response-bytes))
      (define decoded (bytes->jsexpr decompressed))
      decoded))
  (if (hash? result)
      (log-info "Fetched package list containing ~a packages." (hash-count result))
      (log-info "Fetched bogus package list"))
  result)

(define (tombstone? pkg)
  (eq? pkg 'tombstone))

(define (package-manager)
  (define remote-packages (hash))
  (define all-tags* (set))
  (define local-packages (hash))
  (define next-fetch-deadline 0)
  (define next-bogus-timeout base-bogus-timeout)

  (define (asynchronously-fetch-remote-packages!)
    (thread (lambda ()
              (define raw-remote-packages (fetch-remote-packages))
              (if (hash? raw-remote-packages)
                  (begin (set! next-bogus-timeout base-bogus-timeout)
                         (manager-rpc 'refresh-packages! raw-remote-packages))
                  (begin (set! next-fetch-deadline (+ (current-inexact-milliseconds)
                                                      next-bogus-timeout))
                         (log-info "Will retry in ~a ms" next-bogus-timeout)
                         (set! next-bogus-timeout (min package-fetch-interval
                                                       (* next-bogus-timeout 1.618)))
                         (manager-rpc 'ping)))))
    (set! next-fetch-deadline (+ (current-inexact-milliseconds) package-fetch-interval)))

  (define (refresh-packages! raw-remote-packages)
    (set! remote-packages
          (for/hash (((package-name pkg) (in-hash raw-remote-packages)))
            (values package-name
                    (hash-set pkg '_SEARCHABLE-TEXT_ (pkg->searchable-text pkg)))))
    (define all-package-names (set-union (list->set (hash-keys local-packages))
                                         (list->set (hash-keys remote-packages))))
    (set! local-packages
          (for/fold ((acc (hash))) ((package-name all-package-names))
            (define local-pkg (hash-ref local-packages package-name (lambda () #f)))
            (define remote-pkg (hash-ref remote-packages package-name (lambda () #f)))
            (define new-local-pkg
              (cond
               [(not local-pkg) remote-pkg]
               [(and (eq? local-pkg 'tombstone) (not remote-pkg)) #f]
               [(eq? local-pkg 'tombstone) 'tombstone]
               [(> (or (@ remote-pkg last-edit) 0) (or (@ local-pkg last-edit) 0)) remote-pkg]
               [else local-pkg]))
            (if new-local-pkg
                (hash-set acc package-name new-local-pkg)
                acc)))
    (rebuild-all-tags!))

  (define (rebuild-all-tags!)
    (set! all-tags*
          (for/fold ((ts (set))) ((pkg (in-hash-values local-packages)))
            (set-union ts (list->set (or (@ pkg tags) '()))))))

  (define (replace-package! old-pkg new-pkg)
    (set! local-packages
          (hash-set (if old-pkg
                        (hash-remove local-packages (string->symbol (@ old-pkg name)))
                        local-packages)
                    (string->symbol (@ (or new-pkg old-pkg) name))
                    (or new-pkg 'tombstone)))
    (rebuild-all-tags!))

  (define (delete-package! package-name)
    (when (hash-has-key? local-packages package-name)
      (set! local-packages (hash-set local-packages package-name 'tombstone))))

  (with-handlers ((exn:fail? (lambda (e)
                               (log-error "*** PACKAGE MANAGER CRASHED ***\n~a"
                                          (exn->string e))
                               (sleep 5)
                               (package-manager))))
    (let loop ()
      (match (sync (handle-evt (thread-receive-evt)
                               (lambda (_) (thread-receive)))
                   (handle-evt (alarm-evt next-fetch-deadline)
                               (lambda (_) (list #f 'refresh-packages!))))
        [(cons ch request)
         (define reply (match request
                         [(list 'ping)
                          'pong]
                         [(list 'next-fetch-deadline)
                          next-fetch-deadline]
                         [(list 'refresh-packages!)
                          (asynchronously-fetch-remote-packages!)]
                         [(list 'refresh-packages! raw)
                          (refresh-packages! raw)]
                         [(list 'packages)
                          local-packages]
                         [(list 'all-package-names)
                          (hash-keys local-packages)]
                         [(list 'all-tags)
                          all-tags*]
                         [(list 'package-detail name)
                          (define pkg (hash-ref local-packages name (lambda () #f)))
                          (if (tombstone? pkg)
                              #f
                              pkg)]
                         [(list 'replace-package! old-pkg new-pkg)
                          (replace-package! old-pkg new-pkg)]
                         [(list 'delete-package! package-name) (delete-package! package-name)]))
         (when ch (channel-put ch reply))
         (loop)]))))

(define package-manager-thread (thread package-manager))

(define (manager-rpc . request)
  (define ch (make-channel))
  (thread-send package-manager-thread (cons ch request))
  (channel-get ch))

(define (all-package-names) (manager-rpc 'all-package-names))
(define (all-tags) (manager-rpc 'all-tags))
(define (package-detail package-name) (manager-rpc 'package-detail package-name))
(define (replace-package! old-pkg new-pkg) (manager-rpc 'replace-package! old-pkg new-pkg))
(define (delete-package! package-name) (manager-rpc 'delete-package! package-name))
(define (refresh-packages!) (manager-rpc 'refresh-packages!))
(define (next-fetch-deadline) (manager-rpc 'next-fetch-deadline))

(define (sort-package-names names)
  (sort names (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (sorted-package-names)
  (sort-package-names (all-package-names)))

(define (pkg->searchable-text pkg)
  (string-join (flatten (list (or (@ pkg authors) '())
                              (map (match-lambda
                                    [(list _ path) path]
                                    [_ '()])
                                   (or (@ pkg modules) '()))
                              (or (@ pkg name) '())
                              (or (@ pkg description) '())
                              (or (@ pkg source) '())
                              (or (@ pkg tags) '())
                              (map (match-lambda
                                    [(list _ n _) n]
                                    [_ '()])
                                   (or (@ pkg build docs) '()))))))

(define ((package-text-matches? pkg) re)
  (and (not (tombstone? pkg))
       (regexp-match? re (@ pkg _SEARCHABLE-TEXT_))))

(define (package-search text tags)
  (define res (map (lambda (r) (pregexp (format "(?i:~a)" r))) (string-split text)))
  (define packages (manager-rpc 'packages))
  (sort-package-names
   (filter (lambda (package-name)
             (define pkg (hash-ref packages package-name))
             (andmap (package-text-matches? pkg) res))
           (hash-keys
            (for/fold ((ps packages)) ((tag-spec tags))
              (match-define (list tag-name include?) tag-spec)
              (for/hash (((package-name pkg) (in-hash ps))
                         #:when (and (not (tombstone? pkg))
                                     ((if include? values not) (@ref (@ pkg search-terms) tag-name))))
                (values package-name pkg)))))))
