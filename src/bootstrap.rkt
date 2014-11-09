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
	 bootstrap-radio
	 bootstrap-fieldset
	 bootstrap-button

         glyphicon)

(require racket/match)
(require web-server/servlet)
(require "html-utils.rkt")

(define bootstrap-project-name (make-parameter "Project"))
(define bootstrap-project-link (make-parameter "/"))
(define bootstrap-navbar-header (make-parameter #f))
(define bootstrap-navigation (make-parameter '(("Home" "/"))))
(define bootstrap-active-navigation (make-parameter #f))
(define bootstrap-navbar-extension (make-parameter '()))
(define bootstrap-page-stylesheets (make-parameter '()))
(define bootstrap-page-scripts (make-parameter '()))
(define bootstrap-cookies (make-parameter '()))

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

(define (bootstrap-redirect url
                            #:permanent? [permanent? #f]
                            #:headers [headers '()])
  (redirect-to url
               (if permanent? permanently temporarily)
               #:headers (append (map cookie->header (bootstrap-cookies))
                                 headers)))

;; String String XExpr ... -> XExpr
;; Constructs Bootstrap boilerplate for a radio button.
(define (bootstrap-radio #:checked [checked #f] field-name field-value . label-contents)
  `(label ((class "radio"))
	  (input ((type "radio")
		  (name ,field-name)
		  (value ,field-value)
		  ,@(maybe-splice checked '(checked "checked"))))
	  ,@label-contents))

;; [#:legend (Option String)] [#:style Style] XExpr ... -> XExpr
;; where Style is one of 'inline, 'horizontal, or 'normal.
(define (bootstrap-fieldset #:legend [legend #f]
			    #:style [style 'normal]
			    . contents)
  `(fieldset
    ,@(maybe-splice legend `(legend ,legend))
    ,@contents))

;; [#:id (Option String)] [#:type (Option String)] XExpr ... -> XExpr
(define (bootstrap-button #:id [id #f]
			  #:type [type "submit"]
			  . contents)
  `(button ((class "btn")
	    ,@(maybe-splice id `(id ,id))
	    ,@(maybe-splice type `(type ,type)))
	   ,@contents))

;; Symbol -> XExpr
(define (glyphicon type)
  `(span ((class ,(format "glyphicon glyphicon-~a" type)))))
