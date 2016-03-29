;; the following modules are used to implement our phonebook server.
;; The comments signal, which names are used from these modules.
(use-modules (web server))
;; run-server
(use-modules (web request))
;; request-uri
(use-modules (web response))
;; build-response
(use-modules (ice-9 match))
;; match
(use-modules (web uri))
;; uri-path, uri-query, split-and-decode-uri-path
(use-modules (srfi srfi-18))
;; mutexes

;; Because we want to load the phone book in our custom tree data
;; structure, we have to load the definitions.
(load "23tree.scm")

;; This file stores all the html templates for the server.
(load "template.scm")

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
(define get-all #f)
;; Now we define the phone book and its accessors.
(let ((phone-book empty-tree)
      (lock (make-mutex)))
  (define (with-mutex-locked fun)
    (begin (mutex-lock! lock)
           (let ((return-value (fun)))
             (begin (mutex-unlock! lock)
                    return-value))))
  (set! lookup-name
    (lambda (name)
      (with-mutex-locked
       (lambda ()
         (match (lookup comp (list name #f) phone-book)
           ((_ number)
            number)
           (_
            #f))))))
  (set! store-number
    (lambda (name number)
      (with-mutex-locked
       (lambda ()
         (set! phone-book (insert comp (list name number) phone-book))))))
  (set! with-tree
    (lambda (fun)
      (with-mutex-locked
       (lambda () (set! phone-book (fun phone-book))))))
  (set! get-all
    (lambda ()
      (with-mutex-locked
       (lambda ()
         (fold-with-depth (lambda (d cur prev)
                            (cons cur prev))
                          '()
                          phone-book))))))

;; Split a uri query string into its components and sort the elements
;; by their name.
;;
;; (split-query "name=a&age=b") -> (("age" "b") ("name" "a"))
(define (split-query query)
  (match query
    (#f
     "")
    (query-str
     (map (lambda (str)
            (string-split str #\=))
          (string-split query-str #\&)))))

;; get the request path components.  The argument to this function has
;; to be a request object and *not a string*.  The following example
;; won't work for this reason, but shows how this function works on a
;; given path.

;; (request-path-components "localhost:80/test/path") -> ("test" "path")
;;
;; See the "phonebook-handler" function for a valid example.
(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

;; This is the composition of request-uri, uri-query and split-query.
;; See these functions for more explanation.
(define (request-uri-query-components request)
  (split-query (uri-query (request-uri request))))

;; Send a 404 message to the client with "msg" in its response body.
(define (not-found msg)
  (values (build-response #:code 404)
          msg))

(define* (xml-reply msg
                    #:key
                    (code 200))
  "Reply with xml.  This is useful for sending HTML documents to the
client."
  (let ((doctype "<!DOCTYPE html>\n"))
    (values (build-response #:code code
                            #:headers `((content-type . (text/html))))
            (lambda (port)
              (begin
                (display doctype port)
                (sxml->xml msg port))))))

;; This is the request handler.  Valid paths are /lookup and /insert.
;; "localhost:8080/insert?name=gnu&number=42" will save a pair of
;; ("gnu" . "42") in the "database" and
;; "localhost:8080/lookup?name=gnu" will look up "42".
(define (phonebook-handler request request-body)
  (match (request-path-components request)
    ;; match of the path components
    (()
     ;; the document root
     (xml-reply
      (index-page
       (list->ul (map (lambda (e)
                        (match e
                          ((name number)
                           (string-append name ": " number))
                          (_
                           "no match")))
                      (get-all))))))
    (("lookup")
     ;; this will match on "/lookup" and "/lookup/".
     (match (request-uri-query-components request)
       ((("name" name))
        (match (lookup-name name)
          (#f
           (xml-reply (lookup-page-not-found name)
                      #:code 404))
          (number
           (xml-reply (lookup-page-found name
                                         (lookup-name name))
                      #:code 200))))
       (failed
        (not-found "unknown query"))))
    (("insert")
     ;; matches "/insert" and "/insert/"
     (match (request-uri-query-components request)
       ((("name" name) ("number" number))
        (begin
          (store-number name number)
          (xml-reply insert-page)))
       (failed
        (not-found "unknown query"))))
    (nothing
     (not-found "unknown request path"))))

;; Run the server
(run-server phonebook-handler 'http '(#:port 8080))
