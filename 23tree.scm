;; this module contains an implementation of a-b-trees

(use-modules (ice-9 match)) ;; for the use of match
(use-modules (srfi srfi-1)) ;; to import fold

(define empty-tree (list 'leaf))

(define (lookup comp elem tree)
  (match tree
    (('leaf)
     #f)
    (('leaf1 a)
     (if (= 0 (comp elem a))
         a
         #f))
    (('leaf2 a b)
     (cond ((= 0 (comp elem a))
            a)
           ((= 0 (comp elem b))
            b)
           (else #f)))
    (('branch2 left a mid b right)
     (let ((a-comp (comp x a))
           (b-comp (comp x b)))
       (cond ((= -1 a-comp)
              (lookup comp x left))
             ((= 1 b-comp)
              (lookup comp x right))
             ((= 0 a-comp)
              a)
             ((= 0 b-comp)
              b)
             (else (lookup comp x mid)))))))

(define (insert comp elem t)
  (define (down-phase t zipper)
    ;; We have to traverse down the tree to the leaf node where the
    ;; element should be added
    (match t
      ;; check the structure of the node we want to change.  we don't
      ;; have to care about the case where we hit a "leaf" because
      ;; insert itself takes already care of this case
      (('leaf1 x)
       (let ((comp-val (comp elem x)))
         (cond ((= 0 comp-val)
                (zip-up (list 'leaf1 elem) zipper))
               ((= 1 comp-val)
                (zip-up (list 'leaf2 x elem) zipper))
               ((= -1 comp-val)
                (zip-up (list 'leaf2 elem x) zipper)))))
      (('leaf2 x y)
       (let ((comp-with-x (comp elem x))
             (comp-with-y (comp elem y)))
         (cond ((= -1 comp-with-x)
                (up-phase (list 'leaf1 elem)
                          x
                          (list 'leaf1 y)
                          zipper))
               ((= 1 comp-with-y)
                (up-phase (list 'leaf1 x)
                          y
                          (list 'leaf1 elem)
                          zipper))
               ((= 0 comp-with-x)
                (zip-up (list 'leaf2 elem y)
                        zipper))
               ((= 0 comp-with-y)
                (zip-up (list 'leaf2 x elem)
                        zipper))
               (else
                (up-phase (list 'leaf1 x)
                          elem
                          (list 'leaf1 y)
                          zipper)))))
      (('branch1 left x right)
       (let ((comp-val (comp elem x)))
         (cond ((= 0 comp-val)
                (zip-up (list 'branch1 left elem right)
                        zipper))
               ((= 1 comp-val)
                (down-phase right
                            (cons (list 'zip-1-right left x)
                                  zipper)))
               (else
                (down-phase left
                            (cons (list 'zip-1-left x right)
                                  zipper))))))
      (('branch2 left x mid y right)
       (let ((comp-with-x (comp elem x))
             (comp-with-y (comp elem y)))
         (cond ((= 0 comp-with-x)
                (zip-up (list 'branch2
                              left elem mid y right)
                        zipper))
               ((= 0 comp-with-y)
                (zip-up (list 'branch2
                              left x mid elem right)
                        zipper))
               ((= -1 comp-with-x)
                (down-phase left
                            (cons (list 'zip-2-left
                                        x mid y right)
                                  zipper)))
               ((= 1 comp-with-y)
                (down-phase right
                            (cons (list 'zip-2-right
                                        left x mid y)
                                  zipper)))
               (else
                (down-phase mid
                            (cons (list 'zip-2-mid
                                        left x y right)
                                  zipper))))))))
  (define (up-phase left x right zipper)
    ;; this function gets called when we already inserted the element
    ;; to the right place, but still need to rearrange the tree.
    (match zipper
      ;; check if we are already at the top
      (()
       ;; we are at the top
       (list 'branch1 left x right))
      ((zip zipper ...)
       ;; we are not at the top
       (match zip
         ;; check the kind of zip that is next
         (('zip-1-left y rightest)
          (zip-up (list 'branch2 left x right y rightest) zipper))
         (('zip-1-right leftest w)
          (zip-up (list 'branch2 leftest w left x right) zipper))
         (('zip-2-left y left2 z right2)
          (up-phase (list 'branch1
                          left x right)
                    y
                    (list 'branch1
                          left2 z right2)
                    zipper))
         (('zip-2-mid leftest w y rightest)
          (up-phase (list 'branch1
                          leftest w left)
                    x
                    (list 'branch1
                          right y rightest)
                    zipper))
         (('zip-2-right left1 v right2 w)
          (up-phase (list 'branch1
                          left1 v right2)
                    w
                    (list 'branch1
                          left x right)
                    zipper))))))
  (define (zip-up tree zipper)
    ;; This function gets called when we found the right place for the
    ;; element to insert and just have to zip up the rest of the tree.
    (match zipper
      (()
       ;; we are already at the top
       tree)
      ((zip zipper ...)
       (match zip
         ;; check the kind of zip we have
         (('zip-1-left x right)
          (zip-up (list 'branch1 tree x right) zipper))
         (('zip-1-right left x)
          (zip-up (list 'branch1 left x tree) zipper))
         (('zip-2-left x mid y right)
          (zip-up (list 'branch2 tree x mid y right) zipper))
         (('zip-2-mid left x y right)
          (zip-up (list 'branch2 left x tree right) zipper))
         (('zip-2-right left x mid y)
          (zip-up (list 'branch2 left x mid y tree) zipper))))))
  ;; we have to check if we have an empty tree.
  (match t
    (('leaf)
     (list 'leaf1 elem))
    (node
     (down-phase t '()))))

(define (int-comp x y)
  (cond ((< x y) -1)
        ((> x y) 1)
        (else 0)))

(define (fold-with-depth* depth fun start tree)
  ;; foldl over a tree applying the following function
  ;; (fun current-depth current-val previous),
  ;; producing the "previous" argument for the call of fun on the next
  ;; element in the tree
  (match tree
    (('leaf)
     start)
    (('leaf1 x)
     (fun depth x start))
    (('leaf2 x y)
     (fun depth y (fun depth x start)))
    (('branch1 left x right)
     (let* ((previous-x (fold-with-depth* (1+ depth)
                                          fun
                                          start
                                          left))
            (start-right (fun depth x previous-x))
            (result (fold-with-depth* (1+ depth)
                                      fun
                                      start-right
                                      right)))
       result))
    (('branch2 left x mid y right)
     (let* ((previous-x (fold-with-depth* (1+ depth)
                                          fun
                                          start
                                          left))
            (start-mid (fun depth x previous-x))
            (previous-y (fold-with-depth* (1+ depth)
                                          fun
                                          start-mid
                                          mid))
            (start-right (fun depth y previous-y))
            (result (fold-with-depth* (1+ depth)
                                      fun
                                      start-right
                                      right)))
       result))))

(define (fold-with-depth fun start tree)
  (fold-with-depth* 1 fun start tree))

(define (pretty-show tree show)
  (fold-with-depth (lambda (depth val accu)
                     (string-append accu
                                    "\n"
                                    (indent depth (show val))))
                   ""
                   tree))

(define (indent n str)
  (string-append (fold string-append "" (make-list n " "))
                 str))

(define (display-ind n obj)
  (if (<= n 0)
      (begin (display obj)
             (newline))
      (begin (display " ")
             (display-ind (1- n) obj))))

(define (pp t)
  (fold-with-depth (lambda (d x prev) (begin (display-ind (* 2 d) x) prev))
                   #nil
                   t))

(define (run-on-list list)
  (fold (lambda (n t)
          (let ((newt (insert int-comp n t)))
            (begin (pp newt)
                   (display "----")
                   (newline)
                   (insert int-comp n newt))))
        empty-tree
        list))
