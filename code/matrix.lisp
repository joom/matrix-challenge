;;;; Common Lisp version, to be completed

;;;; (sqMatrix size val) = a square matrix of size*size, all cells filled with val
(defun sqMatrix (size val)
  (make-list size :initial-element (make-list size :initial-element val)))

;;;; (randomSqMatrix size limit) = a square matrix of size*size, 
;;;;   cells filled with random numbers from 0 to `limit`
(defun randomSqMatrix (size limit)
  (map 'list (lambda (row) (map 'list (lambda (x) (random limit)) row)) (sqMatrix size 0)))


