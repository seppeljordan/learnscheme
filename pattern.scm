(use-modules (ice-9 match))

(define leaf #nil)

(define (branch val left right)
  (list val left right))

(define (print-root tree)
  (match tree
    (#nil
     (display "leer"))
    ((x l r)
     (display x))
    (_
     (error "hallo"))))

