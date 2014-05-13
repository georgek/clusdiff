(in-package :gk-clusdiff)
;;;; metrics on clusterings

;;; variation of information
(defun H (clustering &optional (base 2))
  (let ((n (loop for cluster in clustering summing (length cluster))))
    (- (loop for cluster in clustering
        summing (* (/ (length cluster) n)
                   (log (/ (length cluster) n) base))))))

(defun I (clustering1 clustering2 &optional (base 2))
  (let ((n (loop for cluster in clustering1 summing (length cluster))))
    (loop for cluster1 in clustering1
         summing (loop for cluster2 in clustering2 summing
                      (let ((int-len (length (intersection cluster1 cluster2))))
                        (* (/ int-len n)
                           (if (> int-len 0)
                               (log (/ (/ int-len n)
                                       (* (/ (length cluster1) n)
                                          (/ (length cluster2) n)))
                                    base)
                                 0)))))))

(defun VI (clustering1 clustering2 &optional (base 2))
  (- (+ (H clustering1 base) (H clustering2 base))
     (* (I clustering1 clustering2 base) 2)))


;;; assignment metric
(defun asgn-met (clustering1 clustering2 delta)
  "The assignment metric, using delta as the metric to compare matched sets."
  (let* ((k (length clustering1)) (kp (length clustering2))
         (k (max k kp))
         (mat (make-array (list k k) :initial-element 1)))
    ;; populate array with deltas
    (loop for cluster1 in clustering1 and i from 0 to (- k 1) do
         (loop for cluster2 in clustering2 and j from 0 to (- kp 1) do
              (setf (aref mat i j) (funcall delta cluster1 cluster2))))
    (dbg :am "~A~%" mat)
    (matching-cost mat (match-fast (matrix-max-to-min mat)))
    ;; (factorial-minimise mat)
    ))

;;; factorial (brute force enumeration) method of finding minimum
(defun factorial-minimise (mat)
  (if (not (= (array-total-size mat) 1))
      (let ((rows (array-dimension mat 0)) (cols (array-dimension mat 1)))
        (loop for j from 0 to (1- rows) minimizing
             (let ((rowl (make-array rows :initial-element t))
                   (coll (make-array cols :initial-element t)))
               (setf (aref rowl j) nil)
               (setf (aref coll 0) nil)
               (+ (aref mat j 0)
                  (factorial-minimise (sub-mat mat rowl coll))))))
      (aref mat 0 0)))

;;; returns a submatrix indexed by logical arrays
(defun sub-mat (matrix rows columns)
  (let ((newmat (make-array `(,(count t rows) ,(count t columns)))))
    (loop for p from 0 to (1- (length rows)) with i = 0 when (elt rows p) do
         (loop for q from 0 to (1- (length columns)) with j = 0 when (elt columns q) do
              (setf (aref newmat i j) (aref matrix p q))
              (incf j))
         (incf i))
    newmat))
