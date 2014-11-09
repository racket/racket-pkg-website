#lang racket/base
;; Utilities for working with Twitter Bootstrap, http://getbootstrap.com/2.3.2/

(provide bootstrap-project-name
         bootstrap-project-link
         bootstrap-navbar-header
         bootstrap-navigation
         bootstrap-active-navigation
         bootstrap-navbar-extension
         bootstrap-page-stylesheets
         bootstrap-page-scripts
         bootstrap-cookies

         bootstrap-response
	 bootstrap-redirect

         add-classes
         glyphicon)

(require racket/match)
(require racket/string)
(require web-server/servlet)
(require "html-utils.rkt")
(require "xexpr-utils.rkt")

(define bootstrap-project-name (make-parameter "Project"))
(define bootstrap-project-link (make-parameter "/"))
(define bootstrap-navbar-header (make-parameter #f))
(define bootstrap-navigation (make-parameter '(("Home" "/"))))
(define bootstrap-active-navigation (make-parameter #f))
(define bootstrap-navbar-extension (make-parameter '()))
(define bootstrap-page-stylesheets (make-parameter '()))
(define bootstrap-page-scripts (make-parameter '()))
(define bootstrap-cookies (make-parameter '()))

;; String [#:title-element XExpr] [#:code Integer] [#:message Bytes] [XExpr ...] -> Response
(define (bootstrap-response title
			    #:title-element [title-element `(h1 ,title)]
                            #:code [code 200]
                            #:message [message #"Okay"]
			    .
			    body-contents)
  (response/xexpr
   #:code code
   #:message message
   #:cookies (bootstrap-cookies)
   #:preamble #"<!DOCTYPE html>\n"
   `(html
     (head (meta ((charset "utf-8")))
	   (meta ((http-equiv "X-UA-Compatible") (content "IE=edge")))
	   (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
	   (title ,title)
	   (link ((rel "stylesheet") (href "/bootstrap/css/bootstrap.min.css") (type "text/css")))
           (link ((rel "stylesheet") (href "/style.css") (type "text/css")))
	   ,@(for/list ((sheet (bootstrap-page-stylesheets)))
	       `(link ((rel "stylesheet") (href ,sheet) (type "text/css")))))
     (body
      (nav ((class "navbar navbar-inverse navbar-fixed-top") (role "navigation"))
	   (div ((class "container"))
		(div ((class "navbar-header"))
                     ,(or (bootstrap-navbar-header)
                          `(a ((class "navbar-brand") (href ,(bootstrap-project-link)))
                            ,(bootstrap-project-name))))
		(div ((id "navbar") (class "collapse navbar-collapse"))
		     (ul ((class "nav navbar-nav"))
			 ,@(for/list ((n (bootstrap-navigation)))
			     (match-define (list text url) n)
			     `(li ,@(maybe-splice (equal? text (bootstrap-active-navigation))
                                                  `((class "active")))
				  (a ((href ,url)) ,text))))
                     ,@(bootstrap-navbar-extension)
                     )))
      (div ((class "container"))
	   ,title-element
	   ,@body-contents)

      (script ((type "text/javascript") (src "/jquery.min.js")))
      (script ((type "text/javascript") (src "/jquery.tablesorter.min.js")))
      (script ((type "text/javascript") (src "/bootstrap/js/bootstrap.min.js")))
      (script ((type "text/javascript") (src "/site.js")))
      ,@(for/list ((script (bootstrap-page-scripts)))
          `(script ((type "text/javascript") (src ,script))))))))

;; String [#:permanent? Boolean] [#:headers (Listof Header)] -> Response
(define (bootstrap-redirect url
                            #:permanent? [permanent? #f]
                            #:headers [headers '()])
  (redirect-to url
               (if permanent? permanently temporarily)
               #:headers (append (map cookie->header (bootstrap-cookies))
                                 headers)))

;; (Listof (U Symbol String)) XExpr -> XExpr
(define (add-classes classes x)
  (define class-strs (map (lambda (c) (if (symbol? c) (symbol->string c) c)) classes))
  (xexpr-case x
              (lambda (content)
                `(span ((class ,(string-join class-strs))) ,content))
              (lambda (tag attrs kids)
                (match (assq 'class attrs)
                  [#f
                   `(,tag ((class ,(string-join class-strs))
                           ,@attrs)
                     ,@kids)]
                  [(and (list 'class existing-class) entry)
                   `(,tag ((class ,(string-join (cons existing-class class-strs)))
                           ,@(remove entry attrs))
                     ,@kids)]))))

;; Symbol -> XExpr
(define (glyphicon type)
  `(span ((class ,(format "glyphicon glyphicon-~a" type)))))
