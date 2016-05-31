(use-modules (web server))
(use-modules (web request))
(use-modules (web response))
(use-modules (web uri))
(use-modules (sxml simple))
(use-modules (srfi srfi-18)) ; mutex
(use-modules (ice-9 match))

(load "23tree.scm")


;; This is the comparison for our pairs of names and phone numbers.
;; Because this used as a way to tell, if two entries belong to the
;; same person, we don't care for the number.  Otherwise we won't be
;; able to find a number without knowing it already.
(define (comp x y)
  (cond ((string<? (car x) (car y)) -1)
        ((string>? (car x) (car y)) 1)
        (else 0)))


;; We want to hide the phone-book variable itself from the user (from
;; us).  Therefor we implement the following functions to access the
;; phone book.  We make them public with the following trivial
;; definitions and override these definitions in the next step.
(define lookup-name #f)
(define store-number #f)
(define with-tree #f)
;; Now we define the phone book and its accessors.
(let ((phone-book empty-tree)
      (lock (make-mutex)))
  (set! lookup-name
    (lambda (name)
      (begin (mutex-lock! lock)
             (let ((res (match (lookup comp (list name #f) phone-book)
                          ((_ number)
                           number)
                          (_
                           #f))))
               (begin (mutex-unlock! lock)
                      res)))))
  (set! store-number
    (lambda (name number)
      (begin (mutex-lock! lock)
             (set! phone-book (insert comp (list name number) phone-book))
             (mutex-unlock! lock))))
  (set! with-tree
    (lambda (fun)
      (begin (mutex-lock! lock)
             (let ((new-phone-book (fun phone-book)))
               (begin (set! phone-book new-phone-book)
                      (mutex-unlock! lock)
                      new-phone-book))))))


; html site structure
(define (html-templating port title contenttitle content)
  (sxml->xml `(html 
               (head
                (title ,title))
               (body
                (h1 ,contenttitle)
                ,content
                (br)(br)(br)(hr)
                (p (i "Scheme Adress Database"))
              ))
             port))


; return error message if val not available
(define (check-val-available val)
  (cond ((not val) "Error: entry not available")
        (else val)))


; display pair
(define (display-val port title pair)
  (html-templating port title title (string-append "Name: " (car pair) " Value: " (check-val-available (cdr pair)))))


; get path from uri (e.g. /set)
(define (request-path request)
  (uri-path (request-uri request)))


; split pair elements by '='
(define (split-vars ql)
  (map (lambda (string) 
         (string-split string #\= ))
       ql))


; split query into pair by '&'
(define (split-query query)
  (split-vars 
    (string-split query #\& )))


; get vars from uri (e.g. a=1&b=2)
(define (request-query request)
  (split-query (uri-query (request-uri request))))


; get pair of vars from querystring
(define (extract-name-number query)
  (cons 
    (cadr (car (request-query query)))
    (cadr (cadr (request-query query)))))


; extract name from query
(define (extract-name query)
  (cadr (car (request-query query))))


; new entry function
(define (new-entry port entry)
  (display-val port "Set value" entry)
  (store-number (car entry) (cdr entry)))


; read entry function
(define (read-entry port query)
  (display-val port "Get value" (cons (extract-name query) (lookup-name (extract-name query)))))


; main function, handles requests
(define (phonebook-handler request request-body)
  (values (build-response #:headers '((content-type . (text/html))))
          (lambda (port)
             (begin (display "<!DOCTYPE html>" port))
                    (cond ((equal? (request-path request)
                              "/get")
                              (read-entry port request))
                          ((equal? (request-path request)
                              "/set")
                              (new-entry port (extract-name-number request)))
                          ((equal? (request-path request)
                              "/")
                              (html-templating port "Home" "Welcome" "/set?name=ABC&number=123 | /get?name=ABC"))
                          (else 
                              (html-templating port "Error 404" "Error 404" "not found"))))))

(run-server phonebook-handler 'http '(#:port 8080))
