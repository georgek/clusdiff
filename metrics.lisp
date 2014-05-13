(in-package :gk-clusdiff)

;;;; metrics on vectors (lists of reals)
(defun Euclidean (x y)
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            x y))))

(defun manhattan (x y)
  (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b)))
                      x y)))

;;;; metrics on discrete lists

;;; heterogeneous Euclidean-overlap metric
;;; 
;;; numbers are considered real valued, symbols are considered discrete valued
;;; 
;;; range is a list of numbers representing the maximum range of each
;;; attribute, for discrete values it should be 1
;;; 
(defun HEOM (x y range)
  (sqrt
   (reduce 
    #'+ (mapcar
         #'(lambda (x) (expt x 2))
         (mapcar 
          #'/ (mapcar 
               #'(lambda (a b)
                   (if (numberp a)
                       (abs-dif a b)
                       (overlap a b)))
               x y)
          range)))))

;;; HEOM for already scaled ranges
(defun HEOM1 (x y)
  (HEOM x y (make-list (length x) :initial-element 1)))

;;;; metrics on reals
(defun abs-dif (x y)
  (abs (- x y)))

;;; metrics on discrete values
(defun overlap (x y)
  (if (equal x y)
      0
      1))

(defun squared (x)
  (* x x))
