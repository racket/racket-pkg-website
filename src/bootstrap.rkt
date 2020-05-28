#lang racket/base
;; Utilities for working with Twitter Bootstrap, http://getbootstrap.com/2.3.2/

(provide bootstrap-static-urlprefix
         bootstrap-dynamic-urlprefix
         bootstrap-project-name
         bootstrap-project-link
         bootstrap-navbar-header
         bootstrap-navigation
         bootstrap-active-navigation
         bootstrap-navbar-extension
         bootstrap-page-stylesheets
         bootstrap-page-scripts
         bootstrap-cookies
         bootstrap-inline-js
         bootstrap-head-extra

         bootstrap-response
	 bootstrap-redirect
         bootstrap-continuation-expiry-handler

         add-classes
         glyphicon)

(require racket/match)
(require racket/string)
(require web-server/servlet)
(require "html-utils.rkt")
(require "xexpr-utils.rkt")

(define bootstrap-static-urlprefix (make-parameter ""))
(define bootstrap-dynamic-urlprefix (make-parameter ""))
(define bootstrap-project-name (make-parameter "Project"))
(define bootstrap-project-link (make-parameter "/"))
(define bootstrap-navbar-header (make-parameter #f))
(define bootstrap-navigation (make-parameter '(("Home" "/"))))
(define bootstrap-active-navigation (make-parameter #f))
(define bootstrap-navbar-extension (make-parameter '()))
(define bootstrap-page-stylesheets (make-parameter '()))
(define bootstrap-page-scripts (make-parameter '()))
(define bootstrap-cookies (make-parameter '()))
(define bootstrap-inline-js (make-parameter #f))
(define bootstrap-head-extra (make-parameter '()))

(define (static str)
  (string-append (bootstrap-static-urlprefix) str))
(define (dynamic str)
  (string-append (bootstrap-dynamic-urlprefix) str))

;; String [#:title-element XExpr] [#:code Integer] [#:message Bytes] [XExpr ...] -> Response
(define (bootstrap-response title
			    #:title-element [title-element `(h1 ,title)]
                            #:code [code 200]
                            #:message [message #"Okay"]
                            #:body-class [body-class #f]
                            #:description [description #f]
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
	   ,@(if (non-empty-string? description)
		 `(meta ((name "description")
			 (content ,description)))
		 '())
	   (link ((rel "stylesheet") (href ,(static "/bootstrap/css/bootstrap.min.css")) (type "text/css")))
	   (link ((rel "stylesheet") (href ,(static "/jquery-ui.min.css")) (type "text/css")))
           (link ((rel "stylesheet") (href ,(static "/style.css")) (type "text/css")))
	   ,@(for/list ((sheet (bootstrap-page-stylesheets)))
	       `(link ((rel "stylesheet") (href ,sheet) (type "text/css"))))
           ,@(bootstrap-head-extra))
     (body ,@(maybe-splice body-class `((class ,body-class)))
      (nav ((class "navbar navbar-inverse navbar-fixed-top") (role "navigation"))
	   (div ((class "container-fluid"))
		(div ((class "navbar-header"))
                     (button ((type "button")
                              (class "navbar-toggle collapsed")
                              (data-toggle "collapse")
                              (data-target "#navbar"))
                             (span ((class "sr-only")) "Toggle navigation")
                             (span ((class "icon-bar")))
                             (span ((class "icon-bar")))
                             (span ((class "icon-bar"))))
                     ,(or (bootstrap-navbar-header)
                          `(a ((class "navbar-brand") (href ,(bootstrap-project-link)))
                            ,(bootstrap-project-name))))
		(div ((id "navbar") (class "collapse navbar-collapse"))
		     (ul ((class "nav navbar-nav"))
                         ,@(render-nav-items (bootstrap-navigation)))
                     ,@(bootstrap-navbar-extension)
                     )))
      (div ((class "container"))
	   ,title-element
	   ,@body-contents)

      (script ,@(cond [(bootstrap-inline-js) => list] [else '()]))
      (script ((type "text/javascript") (src ,(static "/jquery.min.js"))))
      (script ((type "text/javascript") (src ,(static "/jquery.tablesorter.min.js"))))
      (script ((type "text/javascript") (src ,(static "/jquery-ui.min.js"))))
      (script ((type "text/javascript") (src ,(static "/bootstrap/js/bootstrap.min.js"))))
      (script ((type "text/javascript") (src ,(static "/site.js"))))
      ,@(for/list ((script (bootstrap-page-scripts)))
          `(script ((type "text/javascript") (src ,script))))))))

(define (render-nav-items items)
  (for/list ((n items))
    (match n
      [(list text (? string? url))
       `(li ,@(maybe-splice (equal? text (bootstrap-active-navigation))
                            `((class "active")))
            (a ((href ,url)) ,text))]
      ['separator
       `(li ((role "separator") (class "divider")))]
      [(list text (? list? subitems))
       `(li ((class "dropdown"))
            (a ((href "#")
                (class "dropdown-toggle")
                (data-toggle "dropdown")
                (role "button")
                (aria-haspopup "true")
                (aria-expanded "false"))
               ,text
               (span ((class "caret"))))
            (ul ((class "dropdown-menu"))
                ,@(render-nav-items subitems)))])))

;; String [#:permanent? Boolean] [#:headers (Listof Header)] -> Response
(define (bootstrap-redirect url
                            #:permanent? [permanent? #f]
                            #:headers [headers '()])
  (redirect-to url
               (if permanent? permanently temporarily)
               #:headers (append (map cookie->header (bootstrap-cookies))
                                 headers)))

;; Request -> Response
(define (bootstrap-continuation-expiry-handler request)
  (bootstrap-redirect (dynamic (url->string (strip-parameters (request-uri request))))))

;; URL -> URL
(define (strip-parameters u)
  (struct-copy url u
               [path (map (lambda (element)
                            (struct-copy path/param element
                                         [param '()]))
                          (url-path u))]))

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
