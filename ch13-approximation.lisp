(in-package :progalgs)


(defstruct city
  name lat lon)

(defun earth-dist (c1 c2)
  (rtl:with ((lat1 (city-lat c1))
             (lat2 (ciyte-lat c2))
             (a (+ (expt (sin (/ (- lat2 lat1) 2))
                         2)
                   (* (cos lat1)
                      (cos lat2)
                      (expt (sin (/ (- (city-lon c2) (city-lon c1)) 2)) 
                            2)))))
    (* 1.2742e7  ; Earth diameter
       (atan (sqrt a) (sqrt (- 1 a)))))) 

(defun path-length (path)
  (let ((rez (earth-dist (aref path 0) (aref path -1))))
    (dotimes (i (1- (length path)))
      (incf rez (earth-dist (aref path i) (aref path (1+ i)))))
    rez))

(defun random-search (path n)
  (let ((min (path-length path))
        (arg path))
    (loop :repeat n :do
      (rtl:with ((path (rtl:shuffle path))
                 (len (path-length path)))
                (when (< len min)
                  (setf min len
                        arg path))))
    (values arg
            min)))

(defun local-search (path improve-fn)
  (let ((min (path-length path))
        (cc 0))  ; iteration count
    (loop
      (incf cc)
      (rtl:if-it (funcall improve-fn path)
                 (setf min (path-length rtl:it)
                       path rtl:it)
                 (return (values path
                                 min
                                 cc))))))

(defun 2-opt (path)
  (loop :repeat (* 2 (length path)) :do
    (rtl:with ((len (length path))
               (v1 (random len))
               (v1* (if (= (1+ v1) len) 0 (1+ v1)))
               (v2 (loop :for v := (random len)
                         :when (and (/= v v1) (/= v (1- v1)))
                           :do (return v)))
               (v2* (if (= #2=(1+ v2) len) 0 #2#)))
              (when (< (+ (path-length (vec (aref path v1) (aref path v2)))
                          (path-length (vec (aref path v1*) (aref path v2*))))
                       (+ (path-length (vec (aref path v1) (aref path v1*)))
                          (path-length (vec (aref path v2) (aref path v2*)))))
                (let ((beg (min v1* v2*))
                      (end (max v1* v2*)))
                  (return (concatenate 'vector 
                                       (subseq path 0 beg)
                                       (reverse (subseq path beg end))
                                       (subseq path end))))))))

(defun multi-local-search (path n)
  (let ((min (path-length path))
        (arg path))
    (loop :repeat n :do
      (rtl:with ((cur (local-search (rtl:shuffle path) '2-opt)))
        (when (< #1=(path-length cur) min)
          (setf min #1#
                arg cur))))
    (values arg
            min)))

;; TODO add tests for searches


(defun size (set)
  (length set))

(defun empty? (set)
  (null set))

(defun remove-item (set item)
  (rtl:removef item set))

(defun sample (n set &key (with-replacement t))
  (loop :repeat n
        :for i := (random (size set))
        :collect (rtl:? set i)
        :unless with-replacement :do
          (remove-item set i)
          (when (empty? set) (loop-finish))))

(defun sample-from-dist (n dist)
  ;; here, DIST is a hash-table with keys being items
  ;; and values — their probabilities
  (let ((scale (reduce '+ (rtl:vals dist))))
    (loop :repeat n
          :collect (let ((r (* scale (random 1.0)))
                         (acc 0))
                     (rtl:dotable (k v dist)
                       (incf acc v)
                       (when (>= acc r)
                         (return k)))))))

(defun reservoir-sample (n stream)
  (let ((rez (make-array n :initial-element nil)))  ; reservoir
    (handler-case
        (loop :for item := (read stream)
              :for i :from 0
              :for r := (random (1+ i))
              :do (cond
                    ;; fill the reservoir with the first N items
                    ((< i n) (setf (aref rez i) item))
                    ;; replace the R-th item with probability
                    ;; proportionate to (- 1 (/ R N))
                    ((< r n) (setf (aref rez r) item))))
      ;; sampling stops when the stream is exhausted
      ;; we'll use an input stream and read items from it
      (end-of-file () rez))))

(deftest sampling ()
  (let ((42-count 0)
        (foo-count 0)
        (bar-count 0)
        (baz-count 0)
        (count 10000))
    (loop :repeat count :do
      (let ((sample (sample 10 (rtl:range 0 100)))
            (rsample (with-input-from-string (in "foo foo foo foo bar bar baz")
                       (reservoir-sample 3 in))))
        (incf 42-count (count 42 sample))
        (incf foo-count (count 'foo rsample))
        (incf bar-count (count 'bar rsample))
        (incf baz-count (count 'baz rsample))))
    (should be approx= 1/100 (/ 42-count (* 10 count)))
    (should be approx= 4/7 (/ foo-count (* 3 count)))
    (should be approx= 2/7 (/ bar-count (* 3 count)))
    (should be approx= 1/7 (/ baz-count (* 3 count)))))


;; code prototypes

(defstruct branch
  (upper most-positive-fixnum)
  (lower 0)
  (edges (list)))

(defun b&b (g &key n)
  (rtl:with ((cur (vertices g))
             (min (cost cur)))
            (arg cur)
            (q (make-branch :upper min :lower (lower-bound g (list))))
    (loop :for i :from 0
          :for branch := (pop q) :while branch :do
            (when (eql i n) (return))
            (if (branchp branch)
                (dolist (item (branch-out branch))
                  ;; we leave only the subbranches that can,
                  ;; at least in theory, improve on the current solution
                  (when (< (branch-lower item) upper)
                    (push item q)))
                (let ((cost (branch-upper branch)))
                  (when (< cost lower)
                    (setf lower cost
                          arg branch)))))
    (values cur
            cost)))

(defun lower-bound (graph pinned-edges)
  (let ((cost 0)
        (forbidden-edges (apply 'rtl:hash-set 'eql pinned-edges)))
    (dolist (v (vertices graph))
      (let ((min1 most-positive-fixnum)
            (min2 most-positive-fixnum))
        (dolist (e (edges v))
          (unless (rtl:in# e forbidden-edges))
          (let ((len (edge-length e)))
            (cond ((< len min1) (setf min1 len))
                  ((< len min2) (setf min2 len))))))
      (incf cost (/ (+ min1 min2) 2)))
    (reduce '+ (mapcar 'edge-length pinned-edges)
            :initial-value cost)))

(defun gd (fn data &key n (learning-rate 0.1) (precision 1e-6))
  (let ((ws (init-weights fn))
        (cost (cost fn ws))
        (i 0))
    (loop
      (update-weights ws learning-rate
                      (grad fn ws data))
      (let ((prev cost))
        (setf cost (cost fn ws))
        (when (or (< (abs (- cost prev)) precision)
                  (eql n (incf i)))
          (return))))
    (values ws
            cost)))

;; TODO: add full GD variants
;; (let ((dws 0))
;;   (loop
;;     (rtl:with ((batch (sample data batch-size))
;;                (g (calculate-gradient batch)))
;;       (setf dws (- (* decay-rate dws)
;;                    (* learning-rate g)))
;;       (incf ws dws))))
;; (let ((dws 0))
;;   (loop
;;     (incf ws dws)
;;     (rtl:with ((batch (sample data batch-size))
;;                (g (- (* learning-rate (calculate-gradient batch)))))
;;       (setf dws (+ (* decay-rate dws) g)) 
;;       (incf ws g))))

(defun dft (vec)
  (rtl:with ((n (length vec))
             (rez (make-array n))
             (scale (/ (- (* 2 pi #c(0 1))) n)))
            ;; #c(0 1) is imaginary unit (i) - Lisp allows us
            ;; to operate on complex numbers directly
            (dotimes (i n)
              (setf (aref rez i)
                    (loop :for j :from 0 :below n
                          :sum (* (aref vec j)
                                  (exp (* scale i j))))))))

;; (let ((e (fft-of-even-indexed-part))
;;       (o (fft-of-odd-indexed-part))
;;       (scale (exp (/ (- (* 2 pi #c(0 1) i))
;;                      n)))
;;       (n/2 (floor n 2)))
;;   (setf (aref rez i) (+ (aref e i) (* scale (aref o i)))
;;         (aref rez (+ i n/2)) (- (aref e i) (* scale (aref o i)))))
