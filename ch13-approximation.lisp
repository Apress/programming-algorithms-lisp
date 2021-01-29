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

;; TODO add tests
;; (defparameter *wp-link* "https://en.wikipedia.org/w/index.php?title=List_of_state_and_territorial_capitols_in_the_United_States&action=edit&section=1")
;; (defparameter *cs*
;;   (rtl:with ((raw (drakma:http-request *wp-link*))
;;              (coords-regex (ppcre:create-scanner
;;                             "\\{\\{coord\\|(\\d+)\\|(\\d+)\\|([.\\d]+)\\|.\\|(\\d+)\\|(\\d+)\\|([.\\d]+)\\|.\\|type"))
;;              (capitals (list)))
;;     (flet ((dms->rad (vec off)
;;              (* (/ pi 180)
;;                 (+     (aref vec (+ off 0))
;;                        (/ (aref vec (+ off 1)) 60)
;;                        (/ (aref vec (+ off 2)) 3600)))))
;;       (dolist (line (rtl:split
;;                      #\Newline
;;                      (rtl:slice raw
;;                                 (search "{| class=\"wikitable sortable\"" 
;;                                         raw)
;;                                 (search "</textarea><div class='editOptions'>"
;;                                         raw))))
;;         (when (and (rtl:starts-with "|" line)
;;                    (search "{{coord" line))
;;           (rtl:with ((_ coords (ppcre:scan-to-strings coords-regex line))
;;                      (coords (rtl:map* 'read-from-string coords)))
;;             (push (make-city
;;                    :name (slice line (position-if 'alpha-char-p line)
;;                                 (position-if (lambda (ch)
;;                                                (member ch '(#\] #\|)))
;;                                              line :start 1))
;;                    :lat (dms->rad coords 0)
;;                    :lon (dms->rad coords 3))
;;                   capitals)))))
;;             (coerce capitals 'vector)))
;; CL-USER> (path-length *cs*)
;; 9.451802301259182d7
;; CL-USER> (path-length (rtl:shuffle *cs*))
;; 9.964776273250546d7
;; CL-USER> (path-length (rtl:shuffle *cs*))
;; 1.009761841183094d8

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

;; TODO (deftest approx-search ()
;; CL-USER> (random-search *cs* 1000)
;; (#S(CITY :NAME "Atlanta" :LAT 0.5890359059538811d0 ...)
;;    #S(CITY :NAME "Montpelier, Vermont" :LAT 0.772521512027179d0 ...) ...)
;; 7.756170773802838d7
;; CL-USER> (time (random-search *cs* 1000000))
;; Evaluation took:
;; 31.338 seconds of real time
;; ...
;; (#S(CITY :NAME "Boise, Idaho" :LAT 0.7612723873453388d0 ...)
;;    #S(CITY :NAME "Helena, Montana" :LAT 0.813073800024579d0 ...) ...)
;; 6.746660953705506d7
;; CL-USER> (local-search *cs* '2-opt)
;; #(#S(CITY :NAME "Jackson, Mississippi" :LAT 0.5638092223095238d0 ...)
;;   #S(CITY :NAME "Baton Rouge, Louisiana" :LAT 0.5315762080646039d0 ...) ...)
;; CL-USER> (random-search *cs* 111)
;; #(#S(CITY :NAME "Boise, Idaho" :LAT 0.7612723873453388d0 ...)
;;   #S(CITY :NAME "Springfield, Illinois" :LAT 0.6946151297363367d0 ...) ...)
;; 7.522044767585556d7
;; CL-USER> (random-search *cs* 444)
;; #(#S(CITY :NAME "Lansing, Michigan" :LAT 0.745844229097319d0 ...)
;;   #S(CITY :NAME "Springfield, Illinois" :LAT 0.6946151297363367d0 ...) ...)
;; 7.537249874357127d7
;; CL-USER> (time (multi-local-search *cs* 1000))
;; Evaluation took:
;; 22.394 seconds of real time
;; ...
;; #(#S(CITY :NAME "Atlanta" :LAT 0.5890359059538811d0 ...)
;;   #S(CITY :NAME "Montgomery, Alabama" :LAT 0.5650930224896327d0 ...) ...)
;; 2.8086843039667137d7


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
    (loop :repeat n :collect
                    (let ((r (* scale (random 1.0)))
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

;; TODO (deftest sampling ()
;; CL-USER> (with-input-from-string (in "foo foo foo foo bar bar baz")
;;            (reservoir-sample 3 in))
;; #(BAR BAZ FOO)
;; CL-USER> (with-input-from-string (in "foo foo foo foo bar bar baz")
;;            (reservoir-sample 3 in))
;; #(FOO FOO FOO)
;; CL-USER> (with-input-from-string (in "foo foo foo foo bar bar baz")
;;            (reservoir-sample 3 in))
;; #(BAZ FOO FOO)
;; CL-USER> (with-input-from-string (in (format nil "~{~A ~}"
;;                                              (loop :for i :from 0 :to 100
;;                                                    :collect i)))
;;            (reservoir-sample 10 in))
;; #(30 42 66 68 76 5 22 39 51 24)  ; note that 5 stayed at the same position
;;                                         ; where it was placed initially



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
