(use-modules (web server))
(use-modules (web request))
(use-modules (web uri))

(define return-value "nix")

(define (request-path request)
  (uri-path (request-uri request)))

(define (request-query request)
  (uri-query (request-uri request)))

(define (hello request request-body)
  (values '((content-type . (text/plain)))
  (cond ((equal? (request-path request)
              "/get")
              return-value)
        ((equal? (request-path request)
              "/set")
              (begin 
                (set! return-value (request-query request))
                (string-append "OK")))
        (else (string-append "404 " (uri->string (request-uri request)))))))

(run-server hello)

;; set benutzen, ohne die variable "public" sichtbar zu machen
;;(define set-val #f)
;;(let ((val #f))
;;  (set! set-val
;;      (lambda (x))))
;;        (set! val x)))) 
;;
;; HA- implementieren, mit einem getter 