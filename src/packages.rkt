#lang racket/base

(provide all-package-names
         all-tags
         all-formal-tags
         sorted-package-names
         package-detail
         package-batch-detail
         package-external-information
         set-package-external-information!
         package-search
         replace-package!
         delete-package!
         refresh-packages!
         next-fetch-deadline
         package-change-handler-thread
         packages-jsexpr)

(require json)
(require racket/set)
(require racket/match)
(require racket/port)
(require racket/string)
(require racket/list)
(require web-server/private/gzip)
(require net/url)
(require reloadable)
(require "config.rkt")
(require "daemon.rkt")
(require "rpc.rkt")
(require "hash-utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define package-index-url
  (or (@ (config) package-index-url)
      "https://pkgs.racket-lang.org/pkgs-all.json.gz"))

(define package-fetch-interval
  (* (or (@ (config) package-fetch-interval)
         300) ;; 300 seconds = 5 minutes
     1000)) ;; convert to milliseconds

(define base-bogus-timeout (* 5 1000)) ;; 5 seconds

(struct package-manager-state (local-packages
                               external-information
                               all-tags
                               all-formal-tags
                               next-fetch-deadline
                               next-bogus-timeout) #:prefab)

(define (fetch-remote-packages)
  (log-info "Fetching package list from ~a" package-index-url)
  (define result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       ((error-display-handler) (exn-message e) e)
                       #f)])
      (define response-port
        (get-pure-port (string->url package-index-url)))
      (define response-bytes (port->bytes response-port))
      (close-input-port response-port)
      (define decompressed (gunzip/bytes response-bytes))
      (define decoded (bytes->jsexpr decompressed))
      decoded))
  (if (hash? result)
      (log-info "Fetched package list containing ~a packages." (hash-count result))
      (log-info "Fetched bogus package list: ~e" result))
  result)

(define (tombstone? pkg)
  (eq? pkg 'tombstone))

(define (asynchronously-fetch-remote-packages state)
  (thread
   (procedure-rename
    (lambda ()
      (define raw-remote-packages (fetch-remote-packages))
      (manager-rpc 'refresh-packages! raw-remote-packages))
    (string->symbol (format "~v" (list 'asynchronously-fetch-remote-packages (current-inexact-milliseconds))))))
  (struct-copy package-manager-state state
               [next-fetch-deadline (+ (current-inexact-milliseconds) package-fetch-interval)]))

(define (package-manager)
  (package-manager-main (package-manager-state (hash)
                                               (hash)
                                               (set)
                                               (set)
                                               0
                                               base-bogus-timeout)))

(define (refresh-packages raw-remote-packages state)
  (define local-packages (package-manager-state-local-packages state))
  (define remote-packages (for/hash (((package-name pkg) (in-hash raw-remote-packages)))
                            (values package-name
                                    (hash-set pkg '_SEARCHABLE-TEXT_
                                              (pkg->searchable-text pkg)))))
  (define all-package-names (set-union (list->set (hash-keys local-packages))
                                       (list->set (hash-keys remote-packages))))
  (define new-local-packages
    (for/fold ((acc (hash))) ((package-name all-package-names))
      (define local-pkg (hash-ref local-packages package-name (lambda () #f)))
      (define remote-pkg (hash-ref remote-packages package-name (lambda () #f)))
      (define new-local-pkg
        (cond
         [(not local-pkg) remote-pkg]
         [(and (eq? local-pkg 'tombstone) (not remote-pkg)) #f]
         [(eq? local-pkg 'tombstone) 'tombstone]
         [(or (not (@ local-pkg _LOCALLY_MODIFIED_))
              ;; If it's locally modified, only take the remote site
              ;; when its edit-time is newer than the current one
              ;; (which is the unmodified timestamp of the version
              ;; upon which the local edits are based)
              (> (or (@ remote-pkg last-edit) 0) (or (@ local-pkg last-edit) 0)))
          remote-pkg]
         [else local-pkg]))
      (when (not (equal? new-local-pkg local-pkg))
        ;; Run in a separate thread to avoid deadlock, since the
        ;; renderer will undoubtably need to call the package manager.
        (notify-package-change! #f package-name))
      (if new-local-pkg
          (hash-set acc package-name new-local-pkg)
          acc)))
  (rebuild-indexes (struct-copy package-manager-state state
                                [local-packages new-local-packages])))

(define (rebuild-indexes state)
  (struct-copy package-manager-state state
               [all-tags
                (for/fold ((ts (set)))
                          ((pkg (in-hash-values (package-manager-state-local-packages state))))
                  (if (tombstone? pkg)
                      ts
                      (set-union ts (list->set
                                     (map symbol->string
                                          (hash-keys (or (@ pkg search-terms) (hash))))))))]
               [all-formal-tags
                (for/fold ((ts (set)))
                          ((pkg (in-hash-values (package-manager-state-local-packages state))))
                  (if (tombstone? pkg)
                      ts
                      (set-union ts (list->set (or (@ pkg tags) '())))))]))

(define (replace-package completion-ch old-pkg new-pkg state)
  (define local-packages (package-manager-state-local-packages state))
  (define old-package-name (string->symbol (or (@ old-pkg name) "")))
  (define new-package-name (string->symbol (@ (or new-pkg old-pkg) name)))
  (when (not (eq? old-package-name new-package-name))
    (notify-package-change! #f old-package-name))
  (notify-package-change! completion-ch new-package-name)
  (rebuild-indexes
   (struct-copy package-manager-state state
                [local-packages
                 (hash-set (if old-pkg
                               (hash-remove local-packages old-package-name)
                               local-packages)
                           new-package-name
                           (or new-pkg 'tombstone))])))

(define (delete-package completion-ch package-name state)
  (define local-packages (package-manager-state-local-packages state))
  (notify-package-change! completion-ch package-name)
  (if (hash-has-key? local-packages package-name)
      (struct-copy package-manager-state state
                   [local-packages (hash-set local-packages package-name 'tombstone)])
      state))

(define (lookup-package name local-packages)
  (define pkg (hash-ref local-packages name #f))
  (if (tombstone? pkg) #f pkg))

(define (package-manager-main state)
  (match-define (package-manager-state local-packages
                                       external-information
                                       all-tags
                                       all-formal-tags
                                       next-fetch-deadline
                                       next-bogus-timeout) state)
  (package-manager-main
   (rpc-handler (sync (rpc-request-evt)
                      (handle-evt (alarm-evt next-fetch-deadline)
                                  (lambda (_) (list #f 'refresh-packages!))))
     [('next-fetch-deadline)
      (values next-fetch-deadline state)]
     [('refresh-packages!)
      (values (void) (asynchronously-fetch-remote-packages state))]
     [('refresh-packages! (? hash? raw))
      (values (void)
              (struct-copy package-manager-state (refresh-packages raw state)
                           [next-bogus-timeout base-bogus-timeout]))]
     [('refresh-packages! _)
      (log-info "Will retry in ~a ms" next-bogus-timeout)
      (values (void)
              (struct-copy package-manager-state state
                           [next-fetch-deadline
                            (+ (current-inexact-milliseconds)
                               next-bogus-timeout)]
                           [next-bogus-timeout
                            (min package-fetch-interval
                                 (* next-bogus-timeout 1.618))]))]
     [('packages)
      (values local-packages state)]
     [('all-package-names)
      (values (hash-keys local-packages) state)]
     [('all-tags)
      (values all-tags state)]
     [('all-formal-tags)
      (values all-formal-tags state)]
     [('package-detail name)
      (values (lookup-package name local-packages) state)]
     [('package-batch-detail names)
      (values (filter values (for/list ((name names)) (lookup-package name local-packages))) state)]
     [('external-information name)
      (values (hash-ref external-information name (lambda () (hash))) state)]
     [('set-external-information! name info)
      (values (void) (struct-copy package-manager-state state
                                  [external-information
                                   (if info
                                       (hash-set external-information name info)
                                       (hash-remove external-information name))]))]
     [('replace-package! completion-ch old-pkg new-pkg)
      (values (void) (replace-package completion-ch old-pkg new-pkg state))]
     [('delete-package! completion-ch package-name)
      (values (void) (delete-package completion-ch package-name state))])))

(define package-manager-thread
  (make-persistent-state 'package-manager-thread
                         (lambda () (daemon-thread 'package-manager
                                                   (lambda () (package-manager))))))

;; Set to a thread in site.rkt (because the thread needs to call
;; routines only available from site.rkt)
(define package-change-handler-thread
  (make-persistent-state 'package-change-handler-thread
                         (lambda () #f)))

(define (notify-package-change! completion-ch package-name)
  (let retry ()
    (if (not (package-change-handler-thread))
        (begin (sleep 0.5)
               (retry))
        (thread-send (package-change-handler-thread)
                     (list 'package-changed completion-ch package-name)))))

(define (manager-rpc . request) (apply rpc-call (package-manager-thread) request))

(define (all-package-names) (manager-rpc 'all-package-names))
(define (all-tags) (manager-rpc 'all-tags))
(define (all-formal-tags) (manager-rpc 'all-formal-tags))
(define (package-detail package-name) (manager-rpc 'package-detail package-name))
(define (package-batch-detail package-names) (manager-rpc 'package-batch-detail package-names))
(define (package-external-information package-name)
  (manager-rpc 'external-information package-name))
(define (set-package-external-information! package-name info)
  (manager-rpc 'set-external-information! package-name info))
(define (replace-package! completion-ch old-pkg new-pkg)
  (manager-rpc 'replace-package! completion-ch old-pkg new-pkg))
(define (delete-package! completion-ch package-name)
  (manager-rpc 'delete-package! completion-ch package-name))
(define (refresh-packages!) (manager-rpc 'refresh-packages!))
(define (next-fetch-deadline) (manager-rpc 'next-fetch-deadline))

(define (sort-package-names names)
  (sort names (lambda (a b) (string-ci<? (symbol->string a) (symbol->string b)))))

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
       (regexp-match? re (or (@ pkg _SEARCHABLE-TEXT_)
                             ;; Packages lacking the _SEARCHABLE-TEXT_ key are _LOCALLY_MODIFIED_.
                             ;; Synthesise searchable text here; a better (?) alternative would be
                             ;; to do this at package save time, but this will do for now.
                             (pkg->searchable-text pkg)))))

;; prioritize-search :: string? (listof symbol?) -> (listof symbol?)
;; Precondition: names are sorted ascendingly
;; Postcondition: names are reordered so that exact match is prioritized first
;;   followed by those whose prefix matches the search text,
;;   followed by those that contain the search text,
;;   followed by other results
(define (prioritize-search text names)
  (match-define-values (prefixing non-prefixing)
    (partition (λ (name) (string-prefix? (symbol->string name) text)) names))
  (match-define-values (containing non-containing)
    (partition (λ (name) (string-contains? (symbol->string name) text)) non-prefixing))
  (append prefixing containing non-containing))

(define (package-search text tags)
  (define res (map (lambda (r) (regexp (regexp-quote r #f))) (string-split text)))
  (define packages (manager-rpc 'packages))
  (prioritize-search
   text
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
                 (values package-name pkg))))))))

(define (packages-jsexpr)
  (manager-rpc 'packages))
