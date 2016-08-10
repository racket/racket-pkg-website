#lang racket/base
;; Watching for control signals from the outside (Unix) environment

(provide poll-signal
	 start-restart-signal-watcher)

(require (only-in racket/file file->string))
(require reloadable)
(require "daemon.rkt")

(define (poll-signal signal-file-name message handler)
  (when (file-exists? signal-file-name)
    (define contents (file->string signal-file-name))
    (if (string=? contents "")
        (log-info "~a" message)
        (log-info "~a: ~a" message contents))
    (delete-file signal-file-name)
    (if (procedure-arity-includes? handler 1)
        (handler contents)
        (handler))))

(define (start-restart-signal-watcher)
  (daemon-thread
   'restart-signal-watcher
   (lambda ()
     (let loop ()
       (flush-output) ;; Somewhat gratuitous; help ensure timely stdout logging
       (poll-signal "../signals/.pull-required"
		    "Pull signal received"
		    (lambda ()
		      (local-require racket/system)
		      (system "git pull")
		      (exit 0)))
       (poll-signal "../signals/.restart-required"
		    "Restart signal received - attempting to restart"
		    (lambda () (exit 0)))
       (poll-signal "../signals/.reload"
                    "Reload signal received - attempting to reload code"
                    reload!)
       (poll-signal "../signals/.fetchindex"
                    "Index refresh signal received"
                    (reloadable-entry-point->procedure
                     (lookup-reloadable-entry-point 'refresh-packages! "packages.rkt")))
       (poll-signal "../signals/.rerender"
                    "Static rerender request received"
                    (lambda (request-body)
                      (define items-to-rerender (read (open-input-string request-body)))
                      ((reloadable-entry-point->procedure
                        (lookup-reloadable-entry-point 'rerender! "site.rkt"))
                       (if (eof-object? items-to-rerender)
                           #f
                           items-to-rerender))))
       (sleep 0.5)
       (loop)))))
