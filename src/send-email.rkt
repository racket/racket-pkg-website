#lang racket/base
(require racket/format
         net/head
         net/smtp
         net/sendmail
         openssl
         racket/tcp
         "config.rkt"
         "hash-utils.rkt"
         "default.rkt")

(provide send-email)

(define (send-email from-email
                    subject	 	 	 	 
                    to-emails	 	 
                    message)
  (define transport (or (@ (config) email-transport)
                        default-email-transport))
  (case transport
    [(smtp)
     (define server (or (@ (config) stmp-server)
                        default-smtp-server))
     (define port-no (or (@ (config) stmp-port)
                         default-smtp-port))
     (define sending-server (or (@ (config) stmp-sending-server)
                                default-smtp-sending-server))
     (define user+password-file (or (@ (config) smtp-user+password-file)
                                    default-smtp-user+password-file))
     (define-values (user password)
       (call-with-input-file user+password-file (lambda (i) (values (read i) (read i)))))
     (define (extract-address email)
       (car (extract-addresses email 'address)))
     (parameterize ([smtp-sending-server sending-server])
       (smtp-send-message server
                          #:port-no port-no
                          #:tcp-connect (lambda (host port)
                                          (ssl-connect host port 'secure))
                          #:auth-user user
                          #:auth-passwd password
                          (extract-address from-email)
                          (map extract-address to-emails)
                          (standard-message-header from-email
                                                   to-emails
                                                   null
                                                   null
                                                   subject)
                          message))]
    [else
     (send-mail-message from-email
                        subject	 	 	 	 
                        to-emails
                        null
                        null
                        message)]))
