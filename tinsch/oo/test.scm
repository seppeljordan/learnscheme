(load "oo-searchtree.scm")

(define inttree
  (insert-wrapper 1 (insert-wrapper 2 (insert-wrapper 3 leaf))))
(display (lookup-wrapper 1 inttree))
(newline)
(display (lookup-wrapper 2 inttree))
(newline)
(display (lookup-wrapper 3 inttree))
(newline)
(define phonebook #nil)
(display (get-key (lookup-wrapper (make-keyvalue "bene" #nil) (insert-wrapper (make-keyvalue "tina" "3")(insert-wrapper (make-keyvalue "bene" "05") phonebook)))))
