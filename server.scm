;; the following modules are used to implement our phonebook server.
;; The comments signal, which names are used from these modules.
(use-modules (web server))
;; run-server
(use-modules (web request))
;; request-uri
(use-modules (ice-9 match))
;; match
(use-modules (web uri))
;; uri-path, uri-query, split-and-decode-uri-path
(use-modules (srfi srfi-18))
;; mutexes

;; We have to load the 23tree module
(load "23tree.scm")

;; This is the comparison for our pairs of names and phone numbers.
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
               (begin (mutex-unlock!)
                      res)))))
  (set! store-number
    (lambda (name number)
      (begin (mutex-lock! lock)
             (set! phone-book (insert comp (list name number) phone-book))
             (mutex-unlock!))))
  (set! with-tree
    (lambda (fun)
      (begin (mutex-lock! lock)
             (let ((new-phone-book (fun phone-book)))
               (begin (set! phone-book new-phone-book)
                      (mutex-unlock! lock)
                      new-phone-book))))))

;; Split a uri query string into its components and sort the elements
;; by their name
;;
;; "name=a&age=b" -> (("age" "b") ("name" "a"))
(define (split-query query)
  (match query
    (#f
     "")
    (query-str
     (map (lambda (str)
            (string-split str #\=))
          (string-split query-str #\&)))))

;; get the request path components
;; "localhost:80/test/path" -> ("test" "path")
(define (request-path-components request)
  (sort (split-and-decode-uri-path (uri-path (request-uri request)))
        (lambda (x y) (string<? (car x) (car y)))))

;; get the request query components according to 'split-query
(define (request-uri-query-components request)
  (split-query (uri-query (request-uri request))))

;; This function is used to handle insert events.
(define (insert-handler name number)
  (pretty-show (with-tree
                (lambda (tree)
                  (insert comp (list name number) tree)))
               (lambda (elem) (string-append "#" (car elem)
                                             ": " (cadr elem)))))

;; This function is used to handle lookup events.
(define (lookup-handler name)
  (let ((looked-up-name (lookup-name name)))
    (cond ((not looked-up-name)
           (string-append name " is not in the phone book"))
          (else (string-append "number for " name
                               " is " (cadr looked-up-name))))))

;; This is the request handler
(define (phonebook-handler request request-body)
  (values '((content-type . (text/plain)))
          (match (request-path-components request)
            (("lookup")
             (match (request-uri-query-components request)
               ((("name" name))
                (lookup-handler name))
               (failed
                "unknown query")))
            (("insert")
             (match (request-uri-query-components request)
               ((("name" name) ("number" number))
                (insert-handler name number))
               (failed
                "unknown query")))
            (nothing
             "unknown request"))))

;; Run the server
(run-server phonebook-handler)
