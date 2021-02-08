(in-package :progalgs)


(defun map-vec (fn vec)
  "Map function FN over each element of VEC
   and return the new vector with the results."
  (let ((rez (make-array (length vec))))
    (dotimes (i (length vec))
      (setf (aref rez i) (funcall fn (aref vec i))))
    rez))

(deftest map-vec ()
  (should be equalp #(2 3 4) (map-vec '1+ #(1 2 3))))

(defun clumsy-filter-vec (pred vec)
  "Return the vector with only those elements of VEC
   for which calling pred returns true."
  (let ((rez (make-array (length vec) :fill-pointer 0)))
    (dotimes (i (length vec))
      (when (funcall pred (aref vec i))
        (vector-push (aref vec i) rez)))
    rez))

(deftest clumsy-filter-vec ()
  (should be equalp #(1 3) (clumsy-filter-vec 'oddp #(1 2 3))))

(defun m* (m1 m2)
  (rtl:with ((n (array-dimension m1 1))
             (n1 (array-dimension m1 0))
             (n2 (array-dimension m2 1))
             (rez (make-array (list n1 n2))))
    (assert (= n (array-dimension m2 0)))
    (dotimes (i n1)
      (dotimes (j n2)
        (let ((acc 0))
          (dotimes (k n)
            (incf acc (* (aref m1 i k)
                         (aref m2 k j))))
          (setf (aref rez i j) acc))))
    rez))

(deftest m* ()
  (should be equalp #2A((1))
          (m* #2A((1)) #2A((1))))
  (should be equalp #2A((1 2) (3 4))
          (m* #2A((1 2)
                  (3 4))
              #2A((1 0)
                  (0 1)))))

(defun bin-search-v0 (val vec &optional (pos 0))
  (if (> (length vec) 1)
      (rtl:with ((mid (floor (length vec) 2))
                 (cur (aref vec mid)))
                (cond ((< cur val) (bin-search-v0 val
                                                  (rtl:slice vec mid)
                                                  (+ pos mid)))
                      ((> cur val) (bin-search-v0 val
                                                  (rtl:slice vec 0 mid)
                                                  pos))
                      (t (+ pos mid))))
      (when (= (aref vec 0) val)
        pos)))

(defun bin-search (val vec &key (less '<) (test '=) (key 'identity))
  (when (plusp (length vec))
    (let ((beg 0)
          (end (1- (length vec))))
      (do ()
          ((= beg end))
        (let ((mid (+ beg (floor (- end beg) 2))))
          (if (funcall less (funcall key (aref vec mid)) val)
              (setf beg (1+ mid))
              (setf end mid))))
      (values (aref vec beg)
              beg
              (funcall test (funcall key (aref vec beg)) val)))))

#+prototype
(defun bogosort (vec comp)
  (dolist (variant (all-permutations vec))
    (dotimes (i (1- (length variant))
                ;; this is the 3rd optional argument of dotimes header
                ;; that is evaluated only after the loop finishes normally
                ;; if it does we have found a completely sorted permutation!
                (return-from bogosort variant))
      (when (funcall comp (aref variant (1+ i)) (aref variant i))
        (return)))))  ; current variant is not sorted, skip it

(defun selection-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (let ((best (aref vec i))
          (idx i))
      (dotimes (j (- (length vec) i 1))
        (when (funcall comp (aref vec (+ i j 1)) best)
          (setf best (aref vec (+ i j 1))
                idx (+ i j 1))))
      (rotatef (aref vec i) (aref vec idx)))) ; this is the Lisp swap operator
  vec)

(defun insertion-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (do ((j i (1- j)))
        ((minusp j))
      (if (funcall comp (aref vec (1+ j)) (aref vec j))
          (rotatef (aref vec (1+ j)) (aref vec j))
          (return))))
  vec)

(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (rtl:with ((pivot-i 0)
           (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
        (when (funcall comp (aref vec i) pivot)
          (rotatef (aref vec i)
                   (aref vec pivot-i))
          (incf pivot-i)))
      ;; swap the pivot (last element) in its proper place
      (rotatef (aref vec (1- (length vec)))
               (aref vec pivot-i))
      (quicksort (rtl:slice vec 0 pivot-i) comp)
      (quicksort (rtl:slice vec (1+ pivot-i)) comp)))
  vec)

(defun 3-medians (vec comp)
  (rtl:with ((len (length vec))
             (lt (aref vec 0))
             (md (aref vec (floor len 2)))
             (rt (aref vec (1- len))))
    (rtl:switch ((elt (sort (rtl:vec lt md rt) comp) 1))
      (lt 0)
      (rt (1- len))
      (md (floor len 2)))))

(deftest 3-medians ()
  (should be = 1 (3-medians #(1 2 3) '<))
  (should be = 0 (3-medians #(2 1 3) '<))
  (should be = 2 (3-medians #(1 3 2) '<)))

(defun prod-sort (vec comp &optional (eq 'eql))
  (cond ((< (length vec) 2)
         vec)
        ((< (length vec) 10)
         (insertion-sort vec comp))
        (t
         (rotatef (aref vec (1- (length vec)))
                  (aref vec (3-medians vec comp)))
         (rtl:with ((pivot-i 0)
                    (pivot-count 1)
                    (last-i (1- (length vec)))
                    (pivot (aref vec last-i)))
           (do ((i 0 (1+ i)))
               ((> i (- last-i pivot-count)))
             (cond ((funcall comp (aref vec i) pivot)
                    (rotatef (aref vec i)
                             (aref vec pivot-i))
                    (incf pivot-i))
                   ((funcall eq (aref vec i) pivot)
                    (rotatef (aref vec i)
                             (aref vec (- last-i pivot-count)))
                    (incf pivot-count)
                    (decf i)))) ; decrement i to reprocess newly swapped point
           (dotimes (i pivot-count)
             (rotatef (aref vec (+ pivot-i i))
                      (aref vec (- last-i i))))
           (prod-sort (rtl:slice vec 0 pivot-i) comp eq)
           (prod-sort (rtl:slice vec (+ pivot-i pivot-count)) comp eq))))
  vec)

(defun test-sort-vec (fn)
  (should be equalp #(1 2 3 4 5)
          (funcall fn #(1 2 3 4 5) '<))
  (should be equalp #(1 2 3 4 5)
          (funcall fn #(2 1 3 5 4) '<))
  (should be equalp #(1 2 3 4 5)
          (funcall fn #(5 4 3 2 1) '<)))

(deftest sorting ()
  (test-sort-vec 'selection-sort)
  (test-sort-vec 'insertion-sort)
  (test-sort-vec 'quicksort)
  (test-sort-vec 'prod-sort))

(defun random-vec (size)
  (let ((vec (make-array size)))
    (dotimes (i size)
      (setf (aref vec i) (random size)))
    vec))

(defun print-sort-timings (sort-name sort-fn vec)
  ;; we'll use in-place modification of the input vector VEC
  ;; so we need to copy it to preserve the original for future use
  (let ((vec (copy-seq vec))
        (len (length vec)))
    (format t "= ~Asort of random vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec '<))
    (format t "= ~Asort of sorted vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec '<))
    (format t "= ~Asort of reverse sorted vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec '>))))
