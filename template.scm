(use-modules (sxml simple))
(use-modules (srfi srfi-1))

(define insert-form
  `(form (@ (action "insert"))
         (p "Name:")
         (input (@ (type "text") (name "name")))
         (p "Number:")
         (input (@ (type "text") (name "number")))
         (br)
         (input (@ (type "submit")
                   (value "Submit")))))

(define lookup-form
  `(form (@ (action "lookup"))
         (p "Name:")
         (input (@ (type "text") (name "name")))
         (br)
         (input (@ (type "submit")
                   (value "Submit")))))

(define forms
  `(div (@ (class "pure-g"))
        (div (@ (class "pure-u-1 pure-u-md-1-2")) (h2 "Lookup") ,lookup-form)
        (div (@ (class "pure-u-1 pure-u-md-1-2")) (h2  "Insert"),insert-form)))

(define (output str)
  `(p ,str))

(define html-head
  `(head (title "phone book")
         (link (@ (rel "stylesheet")
                  (href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css")))
         (link (@ (rel "stylesheet")
                  (href "http://yui.yahooapis.com/pure/0.6.0/grids-responsive-min.css")))
         (meta (@ (name "viewport")
                  (content "width=device-width, initial-scale=1")))))

(define (list->ul strings)
  "Put stings from a list into an <ul> element."
  (match strings
    (()
     ;; output nothing if the string is empty
     '())
    (str
     (cons 'ul (map (lambda (s) `(li ,s)) str)))))

(define footer
  `(hr ,(output "Author: Sebastian Jordan")))

(define top
  `((h2 "Navigation")
    (div (@ (class "pure-menu pure-menu-horizontal"))
         (ul (@ (class "pure-menu-list"))
             (li (a (@ (href "/")
                       (class "pure-menu-link"))
                    "home"))
             (li (a (@ (href "/lookup")
                       (class "pure-menu-link"))
                    "bla"))))))

;; Complete templates

(define (lookup-page-found name number)
  "Generate an html page for the case the a queried number was found
in the database."
  `(html ,html-head
         (body
          ,(output (string-append name ": " number))
          ,forms
          ,footer)))

(define (lookup-page-not-found name)
  "Generate a page for the case that a query for a name was not
successful."
  `(html ,html-head
         (body
          ,(output (string-append "there is no entry for " name " stored"))
          ,forms
          ,footer)))

(define insert-page
  `(html ,html-head
         (body
          ,top
          ,(output "OK")
          ,forms
          ,footer)))

(define (index-page xml) 
  `(html ,html-head
         (body ,top
               ,xml
               ,forms
               ,footer)))
