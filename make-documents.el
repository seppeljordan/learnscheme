;; This script provides a elisp function that extracts the examples
;; from lessons.org and creates a pdf file
(defun make-examples ()
  ;; Create a lib directory to store our "libraries"
  (make-directory "lib" 1)
  (make-directory "examples" 1)
  ;; Extract examples from lessons.org
  (org-babel-tangle))  

(defun make-pdf ()
  ;; create nicely formatted pdf using latex
  (org-latex-export-to-pdf))

(defun make-html ()
  ;; create a nicely formatted html
  (org-html-export-to-html))
