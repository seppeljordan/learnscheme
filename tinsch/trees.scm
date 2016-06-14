(load "learnscheme/lib/lists.scm")

(define leaf
  #nil)

(define leaf?
  null?)

(define (branch x l r)
  (list x l r))

(define (tree-from-list comp xs)
  (foldl (lambda (t e) (insert comp e t)) leaf xs))

(define (tree-from-list-old xs)
  (foldl (lambda (t e) (insert-easy e t)) leaf xs))

(define (insert-easy elem tree)
  (cond ((null? tree) (branch elem leaf leaf))
        ((> elem (car tree))
        (branch (car tree)
                (cadr tree)
                (insert-easy elem (caddr tree))))
        (else (branch (car tree)
                      (insert-easy elem (cadr tree))
                      (caddr tree)))))

(define (int-comp x y)
  (cond ((= x y) 0)
        ((< x y) -1)
        (else  1)))
;;(display (int-comp 1 2))

(define (insert comp elem tree)
  (cond ((null? tree) (branch elem leaf leaf))
        ((= (comp elem (car tree)) 0)
        (branch elem
                (cadr tree)
                (caddr tree)))
        ((> (comp elem (car tree)) 0)
        (branch (car tree)
                (cadr tree)
                (insert comp elem (caddr tree))))
        (else (branch (car tree)
                      (insert comp elem (cadr tree))
                      (caddr tree)))))

(define (lookup comp elem tree)
  (cond ((null? tree) #f)
        ((= (comp elem (car tree)) 0) (car tree))
        ((> (comp elem (car tree)) 0)
          (lookup comp elem (caddr tree)))
        ((< (comp elem (car tree)) 0)
          (lookup comp elem (cadr tree)))))


;;(display "make new tree")
;;(newline)
;;(display (tree-from-list int-comp '(5 12 3 8 1 15)))
;;(newline)
;;(display "use tree in lookup")
;;(newline)
;;(display (lookup int-comp 12 (tree-from-list int-comp '(5 12 3 8 1 15))))
;;(newline)
;;(display "23 in tree?")
;;(display (lookup int-comp 23 (tree-from-list int-comp '(5 12 3 8 1 15))))
;;(newline)

;;(define (swap e tree)
;;  (cond ((leaf? tree))
;;        (null? (cadr tree) 
;;        (cons (car tree) (branch e #nil (caddr tree))))
;;        (else (swap e (cadr tree)))))
        

;;(cons (cadr tree) (branch (car tree) e (caddr tree))))

(newline)
;;(display (swap 4 (tree-from-list int-comp '(12 5 7 8 1))))
(newline)

(define (fold-tree depth fun start tree)
  ;; (define (fun accu depth elememt)) -> wert der die struktur von accu hat
  (cond ((null? tree) start)
        (else
         (let*
             ((accul (fold-tree (1+ depth) fun start (cadr tree)))
              (accum (fun accul (1+ depth) (car tree)))
              (accur (fold-tree (1+ depth) fun accum (caddr tree))))
           accur))))

(define (depth tree)
  (define (xxxxx d a e)
    (max d a))
  (fold-tree 0 xxxxx 0 tree))

(define (delete-leftist-node tree)
  (cond ((null? tree) #f)
        ((leaf? (cadr tree))
          (cond ((leaf? (caddr tree)) leaf)
          (else (caddr tree))))
        (else (branch (car tree)
                      (delete-leftist-node (cadr tree))
                      (caddr tree)))))

;;(newline)
;;(display (tree-from-list int-comp '(12 5 7 8 1)))
;;(newline)
;;(display (delete-leftist-node (tree-from-list int-comp '(12 5 7 8 1))))
;;(newline)

(define (delete-leftist-node-and-return-pair tree)
  (cond ((null? tree) #f)
        ((leaf? (cadr tree)) (cons (car tree) (caddr tree)))
        (else 
          (let ((pair (delete-leftist-node-and-return-pair (cadr tree))))
            (cons (car pair)
                  (branch (car tree)
                          (cdr pair)
                          (caddr tree)))))))

;;(display "pair of node and tree")
;;(newline)
;;(display (delete-leftist-node-and-return-pair (tree-from-list int-comp '(12 5 7 8 1))))
;;(newline)


(define (print-with-ind n expr)
  (cond ((= n 0) (begin (display expr) 
                        (newline)))
  (else (begin (display " ") 
               (print-with-ind (1- n) expr)))))

;;(print-with-ind 5 "H")

(define (pretty-print-tree tree)
  (fold-tree 0 (lambda (accu depth element) (begin (print-with-ind depth element) #nil)) 0 tree))

;;(pretty-print-tree (tree-from-list int-comp '(12 5 7 8 1)))


(define (delete comp elem tree)
  (cond ((null? tree) leaf)
        ((> (comp elem (car tree)) 0)
          (branch (car tree)
                  (cadr tree)
                  (delete comp elem (caddr tree))))
        ((< (comp elem (car tree)) 0)
          (branch (car tree)
                  (delete comp elem (cadr tree))
                  (caddr tree)))
        (else 
          (let ((new-right (delete-leftist-node-and-return-pair (caddr tree))))
          (cond (new-right (branch (car new-right)
                                   (cadr tree)
                                   (cdr new-right)))
          (else (cadr tree)))))))

;;(display "delete 7")
;;(newline)
;;(pretty-print-tree (delete int-comp 7 (tree-from-list int-comp '(12 5 7 8 1))))
;;(newline)

;;(display "delete 13")
;;(newline)
;;(pretty-print-tree (delete int-comp 13 (tree-from-list int-comp '(12 5 7 8 1))))
;;(newline)
