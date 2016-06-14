(use-modules (web server))
(use-modules (web request))
(use-modules (web response))
(use-modules (web uri))
(use-modules (sxml simple))
(load "../oo/oo-searchtree.scm")
(load "../trees.scm")

(define return-value "nix")

(define (request-path request)
  (uri-path (request-uri request)))

(define (request-query request)
  (uri-query (request-uri request)))

(define (query-list request-query)
  (sort (string-split request-query #\&) string-comp))

(define (string-comp string1 string2)
  (cond ((string<? string1 string2) #t)
        (else #f)))

(define (queries query-list)
  (map (lambda (s)
          (string-split s #\=)) query-list))

(define (queries-list-from-request request)
  (queries (query-list (request-query request))))

(define (pred-name list)
  (string=? (car list) "name"))

(define (pred-number list)
  (string=? (car list) "number"))

(define (extract-name request)
  (cond ((null? (filter pred-name (queries-list-from-request request))) #f)
    (else (cadar (filter pred-name (queries-list-from-request request))))))

(define (extract-number request)
  (cond ((null? (filter pred-number (queries-list-from-request request))) #f)
    (else (cadar (filter pred-number (queries-list-from-request request))))))


;; sxml ` heißt quotierung, ausdrücke werden nicht ausgewertet 
;; mit , kann man die quotierung gezielt aufheben

(define (return-html value port)
  (sxml->xml
    `(html 
        (head (title "This is LISP!"))
        (body (h1 "Welcome to your friendly telephone book") (h2 ,value)))
      port))

(define (key-comp pair1 pair2)
  (cond ((string=? (car pair1) (car pair2)) 0)
        ((string<? (car pair1) (car pair2)) -1)
        (else 1)))

(define (get-number name)
  (lookup-wrapper (make-keyvalue name #nil) phonebook))

(define (insert-number name number)
  (set! phonebook (insert-wrapper (make-keyvalue name number) phonebook)))

;; member funktioniert nicht mit listen von listen
(define (hello request request-body)
  (values (build-response #:headers '((content-type . (text/html))))
          (lambda (port)
            (begin (display "<DOCTYPE html>" port))
                    (cond ((equal? (request-path request)
                            "/")
                            (return-html "Start asking for numbers by using the syntax /get?name=NAME" port))
                          ((equal? (request-path request) "/get")
                            (if (extract-name request)
                                (return-html (get-value (get-number (extract-name request))) port)
                                ;; hier unterscheidung treffen, ob name im baum ist
                                (return-html (string-append "falscher query: " (uri->string (request-query request))) port)))
                          ((equal? (request-path request) "/set")
                              (if (and (extract-name request) (extract-number request))
                                  (begin (insert-number (extract-name request) (extract-number request))
                                         (return-html (string-append "OK") port))))
                    (else (return-html (string-append "404 for path: " (uri->string (request-uri request))) port))))))


(define phonebook 
  leaf)

;;; tests
;;(display (key-comp (cons "maier" "0871234") (cons "müller" "4629276")))
;;(newline)
;;(display (key-comp `("maier" . "0871234") `("müller" . "4629276")))
;;(newline)

;;(define phonebook (tree-from-list key-comp '(("maike" . "5") ("maike" . "2") ("tina" . "15"))))

;;(display (tree-from-list key-comp '(("maike" . "5") ("maike" . "2") ("tina" . "15"))))
;;(newline)
;;(display (get-number "tina"))
;;(newline)
;;(insert-number "tina" "04")
;;(insert-number "bene" "05")
;;(display (get-number "bene"))
;;(newline)
(display phonebook)
;;(newline)
;;(display (queries (query-list "name=lena&nummer=123")))
;;(newline)

(run-server hello)

;; dinge die zu tun sind:
;; dispatching für /insert?name=-...&number=... und lookup, insert mit nur name
;; 404
;; 200 output
;; index seite
