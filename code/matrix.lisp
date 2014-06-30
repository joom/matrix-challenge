;;;; Common Lisp version

;;;; Helper functions
;;;; (you probably do not need them, but they just made things easier for me)

  ;;;; (range min max step) = [min, min + step..max]
  (defun range (min max &optional (step 1))
    (when (<= min max)
      (cons min (range (+ min step) max step))))

  ;;;; (unzip l) = a list that has two list, 
  ;;;;             the first one has the first elements of l, 
  ;;;;             the second has the second elements of l,
  ;;;;             only works for dual zip
  (defun unzip (l) 
    (if (eq l nil) 
      '(nil nil) 
      (let* ((next (unzip (rest l))) (a (first (first l))) (b (second (first l))))
        (list 
          (cons a (first next)) 
          (cons b (second next)) 
        ))
  )) 

  ;;; (zip ...) = a list that has zipped elements of the arguments it take
  (defun zip (&rest xs) (apply 'mapcar (cons 'list xs)))

  ;;;; (without xs n) = xs without nth element (n starting from 0)
  (defun without (xs n) 
    (second (unzip (remove-if (lambda (e) (eq (first e) n)) 
                              (zip (range 1 (length xs)) xs) )))
  )

;;;; Matrix functions

  ;;;; (sqMatrix size val) = a square matrix of size*size, all cells filled with val
  (defun sqMatrix (size val)
    (make-list size :initial-element (make-list size :initial-element val)))

  ;;;; (randomSqMatrix size limit) = a square matrix of size*size, 
  ;;;;   cells filled with random numbers from 0 to `limit`
  (defun randomSqMatrix (size limit)
    (map 'list (lambda (row) (map 'list (lambda (x) (random limit)) row)) 
              (sqMatrix size 0)))

  ;;;; (getMinor m i j) = the minor of the matrix m for (i,j)
  (defun getMinor (m i j)
    (mapcar (lambda (r) (without r j)) (without m i)))

  ;;;; (getFirstRowPairs m) = the list of (row, col) for the first row of the matrix
  (defun getFirstRowPairs (m)
    (zip (make-list (length m) :initial-element 1) (range 1 (length m))))

  ;;;;getCell m (i, j) = the cell in the matrix m in (i,j)
  (defun getCell (m i j)
    (nth (- j 1) (nth (- i 1) m)))

  ;;;; (evensNegative xs) = the same list with the elements with even indices negated
  ;;;;   indices start from 1
  (defun evensNegative (xs)
    (mapcar (lambda (e) (if (first e) (- 0 (second e)) (second e))) 
    (zip (mapcar 'evenp (range 1 (length xs))) xs)))

  ;;;; getDeterminant m = calculates determinant for matrix m
  (defun getDeterminant (m)
    (if (eq (length m) 1) (getCell m 1 1)
    (apply '+ (evensNegative 
                (mapcar (lambda (p) (* (getCell m (first p) (second p)) 
                                       (getDeterminant 
                                         (getMinor m 
                                                   (first p) 
                                                   (second p))))) 
                (getFirstRowPairs m))))
    )
  )

;;;; Program itself

;;;; (main) = the IO section of the program
(defun main ()
  (print "Specify a size for your n * n matrix:")
  (defparameter temp (randomSqMatrix (parse-integer (read-line)) 50))
  (print "Random Matrix:")
  (print (write-to-string temp))
  (print "Determinant of the matrix above:")
  (print (getDeterminant temp))
  (print "")
  (main)
)
(main)
