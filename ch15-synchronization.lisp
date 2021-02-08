(in-package :progalgs)


;; code protoypes

(defstruct lf-queue
  (head (error "No HEAD.") :type cons)
  (tail (error "No TAIL.") :type cons))

(defconstant +dummy+ '.dummy.)

(defun lf-enqueue (value queue)
  (let ((new (cons value nil)))
    (loop (when (eq nil (sb-ext:compare-and-swap
                         (cdr (lf-queue-tail queue))
                         nil new))
            (setf (lf-queue-tail queue) new)
            (return value)))))

(defun lf-dequeue (queue)
  (loop (rtl:with ((head (lf-queue-head queue))
                   (next (cdr head)))
          (typecase next
            ;; the queue always has at least one element:
            ;; a +dummy+ node, thus a non-empty queue
            ;; will have at least two elements,
            ;; so a null NEXT means that the queue was empty
            (null (return (values nil
                                  nil)))
            (cons (when (eq head (sb-ext:compare-and-swap
                                  (lf-queue-head queue)
                                  head next))
                    (let ((value (car next)))
                      (setf (car next) +dummy+)
                      (return (values value
                                      t)))))))))

(defun mapreduce-merge-sort (list n &key (pred '<))
  (lparallel:pmap-reduce
   (lambda (x) (merge-sort x pred))            ; map step: solve a sub-problem
   (lambda (x y) (merge (type-of x) x y pred)) ; reduce step: combine solutions
   (group (ceiling (length list) n) list)))    ; divide data into sub-problems

(defmacro cas (place old new)
  `(when (eql ,place ,old)
     (setf ,place ,new)))

(defmacro atomic-incf (place &optional i)
  (let ((cur (gensym "CUR"))
        (rez (gensym "REZ")))
    `(loop :for ,rez := (let ((,cur ,place))
                          (cas ,place ,cur (+ ,cur ,i)))
           :when ,rez :do (return ,rez))))

(defparameter *interest* (rtl:vec nil nil))
(defparameter *turn* nil)

(defun peterson-call (i fn)
  (let ((other (abs (1- i))))
    (setf (aref *interest* i) t
          *turn* other)
    ;; busy waiting
    (loop :while (and (aref *interest* other)
                      (= *turn* other))) 
    ;; critical section start
    (funcall fn)
    ;; critical section end
    (setf (aref *interest* i) nil)))

(defstruct (g-counter (:conc-name nil))
  ccs)

(defun make-gcc (n)
  (make-g-counter :ccs (make-array n)))

(defun gcc-val (gcc)
  (reduce '+ (ccs gcc)))

(defun gcc-merge (gcc1 gcc2)
  (rtl:map* 'max gcc1 gcc2))
