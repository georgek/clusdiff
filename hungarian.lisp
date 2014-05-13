(in-package :gk-clusdiff)

(defparameter *debug* nil)

(defconstant inf 'inf)

;;; a binary min that knows about infinity
(defun min-inf (x y)
  (cond
    ((eq x inf) y)
    ((eq y inf) x)
    (t (min x y))))

(defmacro dbg (&body body)
  `(when *debug*
     (format ,@body)))

(defun unique-calls-fn (fn-name)
  (let ((fn (symbol-function fn-name))
        (table (make-hash-table :test 'equal)))
    (setf (get fn-name 'unq-table) table)
    (lambda (&rest args)
      (setf (gethash args table) t)
      (apply fn args))))

(defun unique-calls (fn-name)
  (hash-table-count (get fn-name 'unq-table)))

(defun clear-unique-calls (fn-name)
  (let ((table (get fn-name 'unq-table)))
    (when table (clrhash table))))

(defun uniquify (fn-name)
  (setf (symbol-function fn-name)
        (unique-calls-fn fn-name)))

(defmacro defun-unq (fn args &body body)
  `(uniquify (defun ,fn ,args ,@body)))

(defun random-matrix (dimensions &optional (max 25))
  (assert (= (length dimensions) 2))
  
  (let ((matrix (make-array dimensions :element-type 'fixnum)))
    (loop for i from 0 to (1- (elt dimensions 0)) do
         (loop for j from 0 to (1- (elt dimensions 1)) do
              (setf (aref matrix i j) (the fixnum (random max)))))
    (the (simple-array fixnum (* *)) matrix)))

(defun random-matrix-float (dimensions &optional (max 25.0))
  (assert (= (length dimensions) 2))
  
  (let ((matrix (make-array dimensions :element-type 'single-float))
        (max (float max)))
    (loop for i from 0 to (1- (elt dimensions 0)) do
         (loop for j from 0 to (1- (elt dimensions 1)) do
              (setf (aref matrix i j) (the float (random max)))))
    (the (simple-array single-float (* *)) matrix)))

(defun print-matrix (matrix)
  (let* ((m (array-dimension matrix 0))
         (n (array-dimension matrix 1))
         (max-w (loop for i from 0 to (1- m) maximizing
                     (loop for j from 0 to (1- n) maximizing
                          (length (format nil "~A" (aref matrix i j))))))
         (fmt-str (concatenate 'string "~" (write-to-string max-w) "d "))
         )
    (loop for i from 0 to (1- m) do
         (format t "| ")
         (loop for j from 0 to (1- n) do
              (format t fmt-str (aref matrix i j)))
         (format t "|~%"))))

(defun matrix-squarep (matrix)
  (and (arrayp matrix)
       (= (length (array-dimensions matrix)) 2)
       (= (array-dimension matrix 0) (array-dimension matrix 1))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

;;; this macro indexes the mate array using the virtual indices:
;;; | 1 | 2 | ... | n | 1'| 2'| ... | n'|
;;; so e.g. (mate 2 p) to index the 2' position
(defmacro mate (index &optional (p nil))
  (if p
      `(aref mate (1- (+ ,index n)))
      `(aref mate (1- ,index))))

(defmacro aref1 (array &rest subscripts)
  (let ((new-subscripts (copy-list subscripts)))
    (loop for subscript on new-subscripts do
         (setf (car subscript) `(1- ,(car subscript))))
    `(aref ,array ,@new-subscripts)))

(defun matrix-max-to-min (matrix)
  (let* ((m (array-dimension matrix 0))
         (n (array-dimension matrix 1))
         (new-matrix (make-array (list m n)))
         max)
    (setf max (loop for i from 1 to m maximizing
                   (loop for j from 1 to n maximizing
                        (aref1 matrix i j))))
    
    (loop for i from 1 to m maximizing
         (loop for j from 1 to n maximizing
              (setf (aref1 new-matrix i j) (- max (aref1 matrix i j)))))
    
    new-matrix))

(defun augment (mate p j n)
  (let ((mate (copy-seq mate)) 
        i next)
    (loop do
         (setf i (aref1 p j))
         (setf (mate j p) i)
         (setf next (mate i))
         (setf (mate i) j)
         (when (/= next 0) (setf j next))
       while (/= next 0))
    mate))

;;; there are some sorts done which are not necessary, but do make the
;;; procedure deterministic (otherwise it relies on the order of lists
;;; following set operations)
(defun match (matrix &key (minimise nil))
  (assert (matrix-squarep matrix))
  
  ;; transform matrix if we are minimising
  (when minimise
    (setf matrix (matrix-max-to-min matrix)))
  
  (when *debug* (print-matrix matrix))
  
  (let* ((n (array-dimension matrix 0))
         (nrex n)
         (mate (make-array (* n 2) :initial-element 0))
         (u (make-array n))
         (v (make-array n :initial-element 0))
         (m (make-array n))
         (p (make-array n))
         (delta (make-array n))
         (aug nil)
         (setQ (list)))
    
    (dbg t "mate: ~A~%" mate)
    
    (loop for i from 1 to n do
         (setf (aref1 u i) (loop for j from 1 to n maximizing (aref1 matrix i j))))
    
    (dbg t "u: ~A~%~%" u)
    
    (loop while (/= nrex 0) do
         (loop for i from 1 to n do
              (setf (aref1 m i) nil)
              (setf (aref1 p i) 0)
              (setf (aref1 delta i) inf))
         
         (dbg t "m: ~A~%p: ~A~%delta: ~A~%~%" m p delta)
         
         (setf aug nil)
         ;; Q <- {i in S : mate(i)=0}
         (setf setQ (list))
         (loop for i from 1 to n do
              (when (= (mate i) 0)
                (setf setQ (cons i setQ))))
         (setf setQ (nreverse setQ))
         
         (dbg t "Q: ~A~%~%" setQ)
         
         (loop with i and j do
              ;; remove arbitrary vertex i from Q
              (setf i (first setQ))
              (setf setQ (rest setQ))
              (dbg t "removing i: ~A~%Q: ~A~%" i setQ)
              
              (setf (aref1 m i) t)
              (setf j 1)
              
              (loop while (and (not aug) (<= j n)) do
                   (when (/= (mate i) j)
                     (when (or (eq (aref1 delta j) inf)
                               (< (- (+ (aref1 u i) (aref1 v j)) (aref1 matrix i j)) (aref1 delta j)))
                       (setf (aref1 delta j) (- (+ (aref1 u i) (aref1 v j)) (aref1 matrix i j)))
                       (setf (aref1 p j) i)
                       (when (= (aref1 delta j) 0)
                         (if (= (mate j p) 0)
                             (progn
                               (setf mate (augment mate p j n))
                               (setf aug t)
                               (decf nrex))
                             (if *debug*
                                 (setf setQ (sort (union setQ (list (mate j p))) #'<))
                                 (setf setQ (union setQ (list (mate j p)))))))))

                   (incf j))
              (dbg t "delta: ~A~%" delta)
              (dbg t "p: ~A~%" p)
              (dbg t "mate: ~A~%~%" mate)
              
              (when (and (not aug) (null setQ))
                (dbg t "Q must be empty...~%Q: ~A~%~%" setQ)
                (let ((setT (loop for i from 1 to n collecting i))
                      (setJ (list))
                      (setK (list))
                      (setX (list))
                      delt)
                  ;; J <- {i in S : m(i) = true}
                  (loop for i from 1 to n do
                       (when (aref1 m i)
                         (setf setJ (cons i setJ))))
                  ;; K <- {j' in T : delta_j = 0}
                  (loop for j from 1 to n do
                       (when (= (aref1 delta j) 0)
                         (setf setK (cons j setK))))
                  (dbg t "J: ~A~%K: ~A~%~%" setJ setK)
                  
                  (setf delt (reduce #'min (mapcar #'(lambda (i) (aref1 delta i))
                                                   (set-difference setT setK))))
                  (dbg t "delt: ~A~%" delt)
                  
                  (loop for i in setJ do
                       (setf (aref1 u i) (- (aref1 u i) delt)))
                  (loop for j in setK do
                       (setf (aref1 v j) (+ (aref1 v j) delt)))
                  (loop for j in (set-difference setT setK) do
                       (setf (aref1 delta j) (- (aref1 delta j) delt)))
                  
                  (dbg t "u: ~A~%v: ~A~%delta: ~A~%%" u v delta)
                  
                  ;; X <- {j in T\K : delta_j = 0}
                  (loop for j in (set-difference setT setK) do
                       (when (= (aref1 delta j) 0)
                         (setf setX (cons j setX))))
                  
                  (if (every #'(lambda (j) (/= (mate j p) 0)) setX)
                      (loop for j in setX do
                           (if *debug*
                               (setf setQ (sort (union setQ (list (mate j p))) #'<))
                               (setf setQ (union setQ (list (mate j p))))))
                      (let ((j (loop for j in setX do (when (= (mate j p) 0) (return j)))))
                        (setf mate (augment mate p j n))
                        (setf aug t)
                        (decf nrex)))))
              
            until aug))
    mate))

(defun-unq aref1-matrix (matrix i j)
  (declare (type (simple-array fixnum (* *)) matrix)
           (fixnum i j))
  (aref1 matrix i j))

(defun match-fast (matrix)
  (declare (type (simple-array fixnum (* *)) matrix)
           (inline augment)
           (optimize speed (safety 0)))

  (assert (matrix-squarep matrix))
  
  (let* ((n (array-dimension matrix 0))
         (nrex n)
         (mate (make-array (* n 2) :initial-element 0 :element-type 'fixnum))
         (u (make-array n :element-type 'fixnum))
         (v (make-array n :initial-element 0 :element-type 'fixnum))
         (m (make-array n :initial-element nil :element-type 'boolean))
         (p (make-array n :element-type 'fixnum))
         (delta (make-array n :element-type 'fixnum))
         (aug nil)
         (setQ (list)))
    (declare (fixnum n nrex)
             (type (simple-array fixnum (*)) mate u v p delta)
             (type (simple-array boolean (*)) m)
             (boolean aug))
    
    (loop for i fixnum from 1 to n do
         (setf (aref1 u i) 
               (loop for j from 1 to n maximizing (aref1 matrix i j))))
    
    (loop while (/= nrex 0) do
         (loop for i from 1 to n do
              (setf (aref1 m i) nil)
              (setf (aref1 p i) 0)
              (setf (aref1 delta i) most-positive-fixnum))
         
         (setf aug nil)
         ;; Q <- {i in S : mate(i)=0}
         (setf setQ (list))
         (loop for i from 1 to n do
              (when (= (mate i) 0)
                (setf setQ (cons i setQ))))
         (setf setQ (nreverse setQ))
         
         (loop with i fixnum and j fixnum do
              ;; remove arbitrary vertex i from Q
              (setf i (first setQ))
              (setf setQ (rest setQ))
              
              (setf (aref1 m i) t)
              (setf j 1)
              
              (loop while (and (not aug) (<= j n)) do
                   (when (/= (mate i) j)
                     (when (< (the fixnum (- (+ (aref1 u i) (aref1 v j)) (aref1 matrix i j))) (aref1 delta j))
                       (setf (aref1 delta j) (- (+ (aref1 u i) (aref1 v j)) (aref1 matrix i j)))
                       (setf (aref1 p j) i)
                       (when (= (aref1 delta j) 0)
                         (if (= (mate j p) 0)
                             (progn
                               (setf mate (augment mate p j n))
                               (setf aug t)
                               (decf nrex))
                             (setf setQ (union setQ (list (mate j p))))))))

                   (incf j))
              
              (when (and (not aug) (null setQ))
                (let ((setT (loop for i from 1 to n collecting i))
                      (setJ (list))
                      (setK (list))
                      (setX (list))
                      (delt 0))
                  (declare (type list setJ setK setX)
                           (fixnum delt))
                  
                  ;; J <- {i in S : m(i) = true}
                  (loop for i from 1 to n do
                       (when (aref1 m i)
                         (setf setJ (cons i setJ))))
                  ;; K <- {j' in T : delta_j = 0}
                  (loop for j from 1 to n do
                       (when (= (aref1 delta j) 0)
                         (setf setK (cons j setK))))
                  
                  (setf delt (reduce #'min (mapcar #'(lambda (i) (declare (fixnum i)) (aref1 delta i))
                                                   (set-difference setT setK))))
                  
                  (loop for i fixnum in setJ do
                       (setf (aref1 u i) (- (aref1 u i) delt)))
                  (loop for j fixnum in setK do
                       (setf (aref1 v j) (+ (aref1 v j) delt)))
                  (loop for j fixnum in (set-difference setT setK) do
                       (setf (aref1 delta j) (- (aref1 delta j) delt)))
                  
                  ;; X <- {j in T\K : delta_j = 0}
                  (loop for j fixnum in (set-difference setT setK) do
                       (when (= (aref1 delta j) 0)
                         (setf setX (cons j setX))))
                  
                  (if (every #'(lambda (j) (declare (fixnum j)) (/= (mate j p) 0)) setX)
                      (loop for j fixnum in setX do
                           (setf setQ (union setQ (list (mate j p)))))
                      (let ((j (loop for j fixnum in setX do (when (= (mate j p) 0) (return j)))))
                        (setf mate (augment mate p j n))
                        (setf aug t)
                        (decf nrex)))))
              
            until aug))
    mate))

(defun match-fast2 (matrix)
  (declare (type (simple-array single-float (* *)) matrix)
           (inline augment)
           (optimize speed (safety 0)))

  (assert (matrix-squarep matrix))
  
  (let* ((n (array-dimension matrix 0))
         (nrex n)
         (mate (make-array (* n 2) :initial-element 0 :element-type 'fixnum))
         (u (make-array n :element-type 'single-float))
         (v (make-array n :initial-element 0.0 :element-type 'single-float))
         (m (make-array n :initial-element nil :element-type 'boolean))
         (p (make-array n :element-type 'fixnum))
         (delta (make-array n :element-type 'single-float))
         (aug nil)
         (setQ (list)))
    (declare (fixnum n nrex)
             (type (simple-array single-float (*)) u v delta)
             (type (simple-array fixnum (*)) mate p)
             (type (simple-array boolean (*)) m)
             (boolean aug))
    
    (loop for i fixnum from 1 to n do
         (setf (aref1 u i) (aref1 matrix i 1))
         (loop for j fixnum from 2 to n do
              (when (> (aref1 matrix i j) (aref1 u i))
                (setf (aref1 u i) (aref1 matrix i j)))))
    
    (loop while (/= nrex 0) do
         (loop for i from 1 to n do
              (setf (aref1 m i) nil)
              (setf (aref1 p i) 0)
              (setf (aref1 delta i) most-positive-single-float))
         
         (setf aug nil)
         ;; Q <- {i in S : mate(i)=0}
         (setf setQ (list))
         (loop for i from 1 to n do
              (when (= (mate i) 0)
                (setf setQ (cons i setQ))))
         (setf setQ (nreverse setQ))
         
         (loop with i fixnum and j fixnum do
              ;; remove arbitrary vertex i from Q
              (setf i (first setQ))
              (setf setQ (rest setQ))
              
              (setf (aref1 m i) t)
              (setf j 1)
              
              (loop while (and (not aug) (<= j n)) do
                   (when (/= (mate i) j)
                     (when (< (the single-float (- (+ (aref1 u i) (aref1 v j))
                                                   (aref1 matrix i j))) (aref1 delta j))
                       (setf (aref1 delta j) (- (+ (aref1 u i) (aref1 v j)) (aref1 matrix i j)))
                       (setf (aref1 p j) i)
                       (when (= (aref1 delta j) 0)
                         (if (= (mate j p) 0)
                             (progn
                               (setf mate (augment mate p j n))
                               (setf aug t)
                               (decf nrex))
                             (setf setQ (union setQ (list (mate j p))))))))

                   (incf j))
              
              (when (and (not aug) (null setQ))
                (let ((setT (loop for i from 1 to n collecting i))
                      (setJ (list))
                      (setK (list))
                      (setX (list))
                      (delt 0.0))
                  (declare (type list setJ setK setX)
                           (single-float delt))
                  
                  ;; J <- {i in S : m(i) = true}
                  (loop for i from 1 to n do
                       (when (aref1 m i)
                         (setf setJ (cons i setJ))))
                  ;; K <- {j' in T : delta_j = 0}
                  (loop for j from 1 to n do
                       (when (= (aref1 delta j) 0)
                         (setf setK (cons j setK))))
                  
                  (setf delt (reduce #'min (mapcar #'(lambda (i) (declare (fixnum i)) (aref1 delta i))
                                                   (set-difference setT setK))))
                  
                  (loop for i fixnum in setJ do
                       (setf (aref1 u i) (- (aref1 u i) delt)))
                  (loop for j fixnum in setK do
                       (setf (aref1 v j) (+ (aref1 v j) delt)))
                  (loop for j fixnum in (set-difference setT setK) do
                       (setf (aref1 delta j) (- (aref1 delta j) delt)))
                  
                  ;; X <- {j in T\K : delta_j = 0}
                  (loop for j fixnum in (set-difference setT setK) do
                       (when (= (aref1 delta j) 0)
                         (setf setX (cons j setX))))
                  
                  (if (every #'(lambda (j) (declare (fixnum j)) (/= (mate j p) 0)) setX)
                      (loop for j fixnum in setX do
                           (setf setQ (union setQ (list (mate j p)))))
                      (let ((j (loop for j fixnum in setX do (when (= (mate j p) 0) (return j)))))
                        (setf mate (augment mate p j n))
                        (setf aug t)
                        (decf nrex)))))
              
            until aug))
    mate))

(defun matching-cost (matrix mate)
  (loop for i from 1 to (/ (length mate) 2) summing
       (aref1 matrix i (aref1 mate i))))

