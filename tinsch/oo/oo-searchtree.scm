(use-modules (oop goops))
(load "../trees.scm")

(define-class <keyvalue> ()
              (key #:getter get-key
              #:init-keyword #:key)
              (value #:getter get-value
              #:init-keyword #:value))

(define (make-keyvalue key value)
  (make <keyvalue> #:key key #:value value))

(define-method (< (x <keyvalue>) (y <keyvalue>))
               (< (get-key x)
                  (get-key y)))

(define-method (< (x <string>) (y <string>))
               (string<? x y))

(define-method (= (x <keyvalue>) (y <keyvalue>))
               (= (get-key x)
                  (get-key y)))

(define-method (= (x <string>) (y <string>))
                (string=? x y))

(define (> x y)
        (and (not (< x y)) (not (= x y))))

(define (<= x y)
        (or (< x y) (= x y)))

(define (>= x y)
        (not (< x y)))

(define (insert-wrapper elem tree)
  (insert int-comp elem tree))

(define (lookup-wrapper elem tree)
  (lookup int-comp elem tree))

