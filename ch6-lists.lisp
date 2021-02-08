(in-package :progalgs)

(defun dwim-map (fn seq &rest seqs)
  "A thin wrapper over MAP that uses the type of the first SEQ for the result."
  (apply 'map (type-of seq) fn seqs))

(defun simple-mapcar-v1 (fn list)
  (let ((rez (list)))
    (dolist (item list)
      (setf rez (cons (funcall fn item) rez)))
    (reverse rez)))

(defun simple-mapcar-v2 (fn list)
  (let ((rez (list)))
    (dolist (item list)
      (push (funcall fn item) rez))
    (reverse rez)))


(defstruct list-cell
  data
  next)

(defstruct our-own-list
  (head nil :type (or list-cell null))
  (tail nil :type (or list-cell null))
  (size 0 :type (integer 0)))

(defstruct (list-cell2 (:include list-cell))
  prev)

(defun our-cons2 (data list)
  (when (null list) (setf list (make-our-own-list)))
  (let ((new-head (make-list-cell2
                   :data data 
                   :next (rtl:? list 'head))))
    (when (rtl:? list 'head)
      (setf (rtl:? list 'head 'prev) new-head))
    (make-our-own-list
     :head new-head
     :tail (rtl:? list 'tail)
     :size (1+ (rtl:? list 'size)))))

(defstruct queue
  head
  tail)

(defun enqueue (item queue)
  (push item (rtl:? queue 'head)))

(defun dequeue (queue)
  ;; Here and in the next condition, we use the property that an empty list
  ;; is also logically false. This is discouraged by many Lisp style-guides,
  ;; but in many cases such code is not only more compact but also more clear.
  (unless (rtl:? queue 'tail)
    (do ()
        ;; this loop continues until the head becomes empty
        ((null (rtl:? queue 'head)))
      (push (pop (rtl:? queue 'head)) (rtl:? queue 'tail))))
  ;; By pushing all the items from the head to the tail,
  ;; we reverse their order — this is the second reversing
  ;; that cancels the reversing performed when we push the items
  ;; onto the head, so it restores the original order.
  (when (rtl:? queue 'tail)
    (values (pop (rtl:? queue 'tail))
            t)))  ; this second value is used to indicate
                  ; that the queue was not empty

(deftest queue ()
  (let ((q (make-queue)))
    (should be equalp (read-from-string "#S(QUEUE :HEAD NIL :TAIL NIL)")
            q)
    (enqueue 1 q)
    (enqueue 2 q)
    (enqueue 3 q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD (3 2 1) :TAIL NIL)")
            q)
    (dequeue q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD NIL :TAIL (2 3))")
            q)
    (enqueue 4 q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD (4) :TAIL (2 3))")
            q)
    (dequeue q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD (4) :TAIL (3))")
            q)
    (dequeue q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD (4) :TAIL NIL)")
            q)
    (dequeue q)
    (should be equalp (read-from-string "#S(QUEUE :HEAD NIL :TAIL NIL)")
            q)))

(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
   square brackets, arithmetic operations, and numbers."
  (let ((ops ())
        (vals ())
        (op nil)
        (val nil))
    (dolist (item expr)
      (case item
        ([ ) ; do nothing
        ((+ - * /) (push item ops))
        (] (setf op (pop ops)
                 val (pop vals))
         (case op
           (+ (incf val (pop vals)))
           (- (decf val (pop vals)))
           (* (setf val (* val (pop vals))))
           (/ (setf val (/ val (pop vals)))))
         (push val vals))
        (otherwise (push item vals))))
    (pop vals)))

(deftest arith-eval ()
  (should be = 101 (arith-eval '([ 1 + [ [ 2 + 3 ] * [ 4 * 5 ] ] ] ]))))

(defun sorted-union (s1 s2)
  (let ((rez ()))
    (do ()
        ((and (null s1) (null s2)))
      (let ((i1 (first s1))
            (i2 (first s2)))
        (cond ((null i1) (dolist (i2 s2)
                           (push i2 rez))
               (return))
              ((null i2) (dolist (i1 s1)
                           (push i1 rez))
               (return))
              ((= i1 i2) (push i1 rez)
               (setf s1 (rest s1)
                     s2 (rest s2)))
              ((< i1 i2) (push i1 rez)
               (setf s1 (rest s1)))
              ;; just T may be used instead
              ;; of the following condition
              ((> i1 i2) (push i2 rez)
               (setf s2 (rest s2))))))
    (reverse rez)))

(deftest sorted-union ()
  (should be equal '(0 1 2 3 5 6)
          (sorted-union '(1 2 3)
                        '(0 1 5 6))))

(defun merge-sort (list comp)
  (if (null (rest list))
      list
      (let ((half (floor (length list) 2)))
        (merge-lists (merge-sort (subseq list 0 half) comp)
                     (merge-sort (subseq list half) comp)
                     comp))))

(defun merge-lists (l1 l2 comp)
  (let ((rez ()))
    (do ()
        ((and (null l1) (null l2)))
      (let ((i1 (first l1))
            (i2 (first l2)))
        (cond ((null i1) (dolist (i l2)
                           (push i rez))
               (return))
              ((null i2) (dolist (i l1)
                           (push i rez))
               (return))
              ((funcall comp i1 i2) (push i1 rez)
               (setf l1 (rest l1)))
              (t (push i2 rez)
                 (setf l2 (rest l2))))))
    (reverse rez)))

(defun generic-merge-sort (seq comp)
  (if (or (null seq)  ; avoid expensive length calculation
          (<= (length seq) 1))
      seq
      (let ((half (floor (length seq) 2)))
        (merge (type-of seq)
               (merge-sort (subseq seq 0 half) comp)
               (merge-sort (subseq seq half) comp)
               comp))))

(defun parallel-merge-sort (seq comp)
  (if (or (null seq) (<= (length seq) 1))
      seq
      (rtl:with ((half (floor (length seq) 2))
                 (thread1 (eager-future2:pexec
                           (merge-sort (subseq seq 0 half) comp)))
                 (thread2 (eager-future2:pexec
                           (merge-sort (subseq seq half) comp))))
                (merge (type-of seq)
                       (eager-future2:yield thread1)
                       (eager-future2:yield thread2)
                       comp))))

(defun test-sort-list (fn)
  (should be equalp '(1 2 3 4 5)
          (funcall fn '(1 2 3 4 5) '<))
  (should be equalp '(1 2 3 4 5)
          (funcall fn '(2 1 3 5 4) '<))
  (should be equalp '(1 2 3 4 5)
          (funcall fn '(5 4 3 2 1) '<)))

(deftest merge-sort ()
  (test-sort-list 'merge-sort)
  (test-sort-list 'generic-merge-sort)
  (test-sort-list 'parallel-merge-sort))
