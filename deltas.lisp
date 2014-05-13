(in-package :gk-clusdiff)

;;;; metrics on sets

;;; bounds result to [0,1]
(defun bound (val)
  (/ val (+ 1 val)))

;;; makes a bounded metric from any metric
(defmacro bounded (metric &rest rest)
  `(lambda (A B) (bound (,metric A B ,@rest))))

;;; symmetric difference
(defun symdif (A B)
  (length (set-exclusive-or A B)))

;;; normalised symmetric difference
(defun norm-symdif (A B)
  (/ (length (set-exclusive-or A B))
     (length (union A B))))

;;; distance from a point to the closest point in a set
(defun min-point->set (x A metric)
  (loop for y in A minimizing (funcall metric x y)))

;;; directed haussdorf distance
(defun haussdorf-> (A B metric)
  (loop for x in A maximizing (funcall #'min-point->set x B metric)))

;;; haussdorf metric
(defun haussdorf (A B &optional (metric #'euclidean))
  (max (haussdorf-> A B metric) (haussdorf-> B A metric)))

