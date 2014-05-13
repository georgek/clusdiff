(in-package :gk-clusdiff)

;;; returns a clustering given by indices on the dataset
(defun clus (dset indices)
  "dset should be a vector of elements, indices should be a list of lists
  representing clusters"
  (loop for clus in indices collecting
       (loop for el in clus collecting (elt dset el))))

;;; enumerates all possible 2-clusterings for a given dataset
(defun 2-clusts (dset)
  (mapcar #'(lambda (x) (list x (set-difference dset x)))
   (loop for i from 1 to (floor (/ (length dset) 2)) append
        (subsets dset i))))

;;; enumerates all possible subsets of length l
(defun subsets (set l)
  (if (= l 1)
      (loop for i from 1 to (length set) collecting (list (elt set (1- i))))
      (loop for i on set append
           (mapcar #'(lambda (x) (append x (list (first i))))
                   (subsets (rest i)
                            (1- l))))))

;;; gives cd cost and sep for every possible 2-clustering
(defun costs (dset)
  (let* ((indices (loop for i from 0 to (1- (length dset)) collecting i))
         (clusts (2-clusts indices)))
    (loop for clust in clusts 
       for vals = (loop for clus in clust collecting
                       (mapcar #'(lambda (x) (elt dset x)) clus))
       for cost = (sum-of-squares vals #'heom1 #'mixed-centroid)
       for sep  = (squared-separation vals #'heom1 #'mixed-centroid)
       minimizing cost into min-cost
       maximizing sep  into max-sep
       do 
       (format t "~s : ~5,2f  ~5,2f~%" clust cost sep)
       finally
       (format t "Min cost: ~f~%" min-cost)
       (format t "Max sep: ~f~%" max-sep))))

(defvar *dbg-ids* nil
  "Identifiers used by DBG")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbgo (object)
  (format *debug-io* "~A~%" object)
  object)

(defun set-debug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on ids. With no ids, stop dbg altogether"
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-on-p (id)
  (member id *dbg-ids*))
