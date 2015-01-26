#lang racket/base

(provide pkg-build-baseurl)

(require racket/match)
(require racket/file)
(require (only-in racket/port copy-port))
(require net/url)
(require "config.rkt")
(require "hash-utils.rkt")
(require reloadable)
(require "daemon.rkt")
(require "rpc.rkt")

(define pkg-build-baseurl
  (or (@ (config) pkg-build-baseurl)
      "http://pkg-build.racket-lang.org/"))

(define pkg-build-cache-path
  (or (@ (config) pkg-build-cache-path)
      (build-path (var-path) "cache")))

(make-directory* pkg-build-cache-path)

(define pkg-build-cache-refresh-interval
  (* 1000 (or (@ (config) pkg-build-cache-refresh-interval)
              3600))) ;; one hour

(define (compute-next-refresh-deadline)
  (+ (current-inexact-milliseconds) pkg-build-cache-refresh-interval))

(define cached-summary-path (build-path pkg-build-cache-path "summary.rktd"))
(define cached-etag-path (build-path pkg-build-cache-path "summary.rktd.etag"))

(define (extract-etag hs)
  (for/or ([h (in-list hs)])
    (match h
      [(regexp #rx#"^ETag: (.*?)$" (list _ tag-bytes)) tag-bytes]
      [_ #f])))

;; Returns #t if the summary file has been updated, or #f if it
;; remains the same as it was previously.
(define (refresh-build-server-summary!)
  (define summary-url (combine-url/relative (string->url pkg-build-baseurl) "summary.rktd"))

  (define HEAD-etag
    (let-values (((HEAD-status HEAD-headers HEAD-body-input-port)
                  (http-sendrecv/url summary-url #:method #"HEAD")))
      (extract-etag HEAD-headers)))

  (define cached-etag (and (file-exists? cached-etag-path) (file->bytes cached-etag-path)))

  (define need-refresh?
    (or (not HEAD-etag) ;; server didn't supply an ETag
        (not cached-etag) ;; we don't have a record of an ETag locally
        (not (equal? HEAD-etag cached-etag)))) ;; the ETag has changed

  (cond
   [need-refresh?
    (log-info "Build server summary.rktd ETag has changed. Refreshing.")
    (define-values (GET-status GET-headers GET-body-input-port)
      (http-sendrecv/url summary-url #:method #"GET"))

    (define new-file (make-temporary-file "summary-~a.rktd" #f pkg-build-cache-path))
    (call-with-output-file new-file
      (lambda (p) (copy-port GET-body-input-port p))
      #:exists 'replace)
    (with-output-to-file cached-etag-path
      (lambda () (write-bytes (extract-etag GET-headers)))
      #:exists 'replace)
    (rename-file-or-directory new-file cached-summary-path #t)]
   [else
    (log-info "Build server summary.rktd ETag has not changed.")])

  need-refresh?)

(define (load-build-server-summary)
  (if (file-exists? cached-summary-path)
      (file->value cached-summary-path)
      (hash)))

(struct build-server-state (summary-table
                            next-refresh-deadline
                            ) #:prefab)

(define (boot-build-server)
  (refresh-build-server-summary!)
  (build-server-main (build-server-state (load-build-server-summary)
                                         (compute-next-refresh-deadline))))

(define (send-change-notifications! old-table new-table)
  (log-info "HERE ~v ~v" old-table new-table))

(define (build-server-main state)
  (match-define (build-server-state summary-table next-refresh-deadline) state)
  (build-server-main
   (rpc-handler (sync (rpc-request-evt)
                      (handle-evt (alarm-evt next-refresh-deadline)
                                  (lambda (_) (list #f 'refresh!))))
     [('refresh!)
      (values (void)
              (if (refresh-build-server-summary!)
                  (let ((new-summary-table (load-build-server-summary)))
                    (send-change-notifications! summary-table new-summary-table)
                    (struct-copy build-server-state state
                                 [summary-table new-summary-table]
                                 [next-refresh-deadline (compute-next-refresh-deadline)]))
                  (struct-copy build-server-state state
                               [next-refresh-deadline (compute-next-refresh-deadline)])))]
     )))

(define build-server-thread
  (make-persistent-state 'build-server-thread
                         (lambda () (daemon-thread 'build-server
                                                   (lambda () (boot-build-server))))))

(sleep 5)
