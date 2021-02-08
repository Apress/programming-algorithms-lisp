(in-package :progalgs)

(defun naive-fib (i)
  (check-type i (integer 0))
  (if (< i 2) 1
      (+ (naive-fib (- i 1))
         (naive-fib (- i 2)))))

(let ((fib (rtl:vec 1 1)))  ; our table will be an adjustable vector
  (defun fib (i)
    (when (< (length fib) i)
      (vector-push-extend (fib (- i 1)) fib))
    (+ (aref fib (- i 1))
       (aref fib (- i 2)))))

(let ((fib (rtl:vec 1 1)))
  (defun bottom-up-fib (i)
    (let ((off (length fib)))
      (adjust-array fib (1+ i) :fill-pointer t)
      (dotimes (j (- (1+ i) off))
        (let ((j (+ j off)))
          (setf (aref fib j)
                (+ (aref fib (- j 1))
                   (aref fib (- j 2)))))))
    (aref fib i)))

(deftest fib ()
  (should be = (fib 20) (naive-fib 20))
  (should be = (fib 22) (naive-fib 22))
  (should be = 165580141 (fib 40))
  (should be = 433494437 (fib 42))
  (should be = 165580141 (bottom-up-fib 40))
  (should be = 433494437 (bottom-up-fib 42)))

(defun shortest-first-restore-spaces (dict str)
  (dotimes (i (length str))
    (let ((word (rtl:slice str 0 (1+ i))))
      (when (rtl:? dict word)
        (return (rtl:cond-it
                  ((= (1+ i) (length str))
                   word)
                  ((shortest-first-restore-spaces dict (rtl:slice str (1+ i)))
                   (format nil "~A ~A" word rtl:it))))))))



(defun bt-shortest-first-restore-spaces (dict str)
  (dotimes (i (length str))
    (let ((word (rtl:slice str 0 (1+ i))))
      (when (rtl:in# word dict)
        (when (= (1+ i) (length str))
          (return word))
        (rtl:when-it (bt-shortest-first-restore-spaces dict (rtl:slice str (1+ i)))
          (return (format nil "~A ~A" word rtl:it)))))))

(defun dp-restore-spaces (dict str)
  (let ((dp (make-array (1+ (length str)) :initial-element nil))
        ;; in the production implementation, the following calculation
        ;; should be performed at the pre-processing stage
        (w (reduce 'max (mapcar 'length (rtl:keys dict))))
        (begs (list))
        (rez (list)))
    ;; the outer loop tries to find the next word
    ;; only starting from the ends of the words that were found previously
    (do ((i 0 (pop begs)))
        ((or (null i)
             (= i (length str))))
      ;; the inner loop checks all substrings of length 1..w
      (do ((j (1+ i) (1+ j)))
          ((>= j (1+ (min (length str)
                          (+ w i)))))
        (when (rtl:? dict (rtl:slice str i j))
          (setf (aref dp j) i)
          (push j begs)))
      (setf begs (reverse begs)))
    ;; the backward pass
    (do ((i (length str) (aref dp i)))
        ((null (aref dp i)))
      (push (rtl:slice str (aref dp i) i) rez))
    (rtl:strjoin #\Space rez)))
  
(deftest restore-spaces ()
  (let ((dict (rtl:hash-set 'equal "a" "i" "at" "is" "hi" "ate"
                                   "his" "sat" "test" "this")))
    (should be null (shortest-first-restore-spaces dict "thisisatest"))
    (should be string= "this is a test"
            (bt-shortest-first-restore-spaces dict "thisisatest"))
    (should be string= "this is a test"
            (dp-restore-spaces dict "thisisatest"))))

(defun tj-penalty (length limit)
  (if (<= length limit)
      (expt (- limit length) 3)
      most-positive-fixnum))

(defun justify (limit str)
  (rtl:with ((toks (reverse (rtl:split #\Space str)))
             (n (length toks))
             (penalties (make-array n))
             (backptrs (make-array n))
             (lengths (make-array n)))
    ;; forward pass (from the end of the string)
    (rtl:doindex (i tok toks)
      (let ((len (+ (length tok) (if (plusp i) (max 0 (aref lengths (1- i)))
                                     0))))
        (setf (aref lengths i) (1+ len))
        (if (<= len limit)
            (setf (aref penalties i) (tj-penalty len limit)
                  (aref backptrs i) -1)
            ;; minimization loop
            (let ((min most-positive-fixnum)
                  arg)
              (dotimes (j i)
                (rtl:with ((j (- i j 1))
                           (len (- (aref lengths i)
                                   (aref lengths j)))
                           (penalty (+ (tj-penalty len limit)
                                       (aref penalties j))))
                          (cond ((> len limit) (return))
                                ((< penalty min) (setf min penalty
                                                       arg j)))))
              (setf (aref penalties i) min
                    (aref backptrs  i) arg)))))
    ;; backward pass (decoding)
    (with-output-to-string (out)
      (loop :for end := (1- n) :then beg
            :for beg := (aref backptrs end)
            :do ;; if there's no path some words were longer thn the limit
                (unless beg (return-from justify))
                (format out "~A~%"
                        (rtl:strjoin #\Space (reverse (subseq toks 
                                                              (1+ beg)
                                                              (1+ end)))))
            :until (= -1 beg)))))

(deftest justify ()
  (let ((str "Common Lisp is the modern, multi-paradigm, high-performance, compiled, ANSI-standardized, most prominent descendant of the long-running family of Lisp programming languages."))
    (should be null (justify 0 str))
    (should be null (justify 10 str))
    (should be string= "Common Lisp
is the modern,
multi-paradigm,
high-performance,
compiled,
ANSI-standardized,
most prominent
descendant of the
long-running family
of Lisp programming
languages.
" (justify 20 str))
    (should be string= "Common Lisp is the modern, multi-paradigm,
high-performance, compiled, ANSI-standardized,
most prominent descendant of the long-running
family of Lisp programming languages.
" (justify 50 str))))

(defun lev-dist (s1 s2 &optional 
                         (i1 (1- (length s1)))
                         (i2 (1- (length s2)))
                         (ld (make-array (list (1+ (length s1))
                                               (1+ (length s2)))
                                         :initial-element nil)
                             ldp))  ; a flag indicating that the argument
                                        ; was supplied
  ;; initialization of the 0-th column and row
  (unless ldp
    (dotimes (k (1+ (length s1))) (setf (aref ld k 0) 0))
    (dotimes (k (1+ (length s2))) (setf (aref ld 0 k) 0)))
  (values (or (aref ld (1+ i1) (1+ i2))
              (setf (aref ld (1+ i1) (1+ i2))
                    (if (eql (aref s1 i1) (aref s2 i2))
                        (lev-dist s1 s2 (1- i1) (1- i2) ld)
                        (1+ (min (lev-dist s1 s2 (1- i1) (1- i2) ld)
                                 (lev-dist s1 s2 i1 (1- i2) ld)
                                 (lev-dist s1 s2 (1- i1) i2 ld))))))
          ld))

(defun align (s1 s2)
  (rtl:with ((i1 (length s1))
             (i2 (length s2))
             ;; our Levenstein distance procedure returns the whole DP matrix
             ;; as a second value
             (ld (nth-value 1 (lev-dist s1 s2)))
             (rez (list)))
            (loop
              (let ((min (min (aref ld (1- i1) (1- i2))
                              (aref ld     i1  (1- i2))
                              (aref ld (1- i1)     i2))))
                (cond ((= min (aref ld (1- i1) (1- i2)))
                       (push (rtl:pair (char s1 (1- i1))
                                       (char s2 (1- i2)))
                             rez)
                       (decf i1)
                       (decf i2))
                      ((= min (aref ld (1- i1) i2))
                       (push (rtl:pair (char s1 (1- i1)) nil)
                             rez)
                       (decf i1))
                      ((= min (aref ld i1 (1- i2)))
                       (push (rtl:pair nil (char s2 (1- i2)))
                             rez)
                       (decf i2))))
              (when (= 0 i1)
                (loop :for j :from (1- i2) :downto 0 :do
                  (push (rtl:pair #\* (char s2 j)) rez))
                (return))
              (when (= 0 i2)
                (loop :for j :from (1- i1) :downto 0 :do
                  (push (rtl:pair (char s1 j) nil) rez))
                (return)))
            ;; pretty output formatting
            (with-output-to-string (s1)
              (with-output-to-string (s2)
                (with-output-to-string (s3)
                  (loop :for (c1 c2) :in rez :do
                    (format s1 "~C " (or c1 #\.))
                    (format s2 "~C " (cond ((null c1) #\↓)
                                           ((null c2) #\↑)
                                           ((char= c1 c2) #\|)
                                           (t #\x)))
                    (format s3 "~C " (or c2 #\.)))
                  (format t "~A~%~A~%~A~%"
                          (get-output-stream-string s1)
                          (get-output-stream-string s2)
                          (get-output-stream-string s3)))))
            rez))

(deftest alignment ()
  (should be = 5 (lev-dist "democracy" "remorse"))
  (should print-to *standard-output* "d e m o c r a c y 
x | | | ↑ | ↑ x x 
r e m o . r . s e 
"
          (align "democracy" "remorse")))
