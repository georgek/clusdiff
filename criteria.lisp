(in-package :gk-clusdiff)

;;;; clustering criteria

;;; sum-of-squares (k-means, centroid-distance)
(defun sum-of-squares (clustering metric centroid)
  "Gives the sum-of-squares cost, metric should be a metric function which
  takes two data points and returns a real number, centroid should be a
  function which takes a list of data points and returns their centroid."
  (loop for c in clustering summing
       (let ((cent (funcall centroid c)))
         (loop for x in c summing (squared (funcall metric x cent))))))

;;; centroid separation (min sum-of-squares maximises this for Euclidean space)
(defun squared-separation (clustering metric centroid)
  (let ((dcent (funcall centroid (reduce #'append clustering))))
    (loop for c in clustering summing
         (* (squared (funcall metric (funcall centroid c) dcent)) (length c)))))

;;;; centroid functions

;;; Euclidean space
(defun Euclidean-centroid (points)
  (mapcar #'(lambda (c) (/ c (length points)))
          (reduce
           #'(lambda (l1 l2) (mapcar #'+ l1 l2))
           points)))

;;; mixed real/discrete valued
(defun mixed-centroid (points)
  (loop for i from 0 to (1- (length (car points))) collecting
       (if (numberp (elt (car points) i))
           ;; mean
           (/ (loop for p in points summing (elt p i)) (length points))
           ;; mode
           (let ((map (make-hash-table)))
             (loop for p in points do
                  (if (gethash (elt p i) map)
                      (incf (gethash (elt p i) map))
                      (setf (gethash (elt p i) map) 1)))
             ;; find most common
             (let ((max (list nil 0)))
               (loop for k being the hash-keys in map using (hash-value v) do
                    (if (> v (second max))
                        (setf max (list k v))))
               (first max))))))
