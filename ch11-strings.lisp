(in-package :progalgs)

(defstruct (mb-string (:conc-name mbs-))
  bytes
  bitmap)

(defparameter *mb-threshold* 10)

(defun mb-char-index (string i)
  (let ((off 0))
    (loop
      (rtl:with ((cnt (count 1 (mbs-bitmap string)
                             :start off :end (+ off i))))
                (diff (- i cnt)))
      (cond
        ((= cnt i)
         (return (+ off i)))
        ((< diff *mb-threshold*)
         (return (mb-linear-char-index string diff off)))
        ((< cnt (floor i 2))
         (incf off i)
         (decf i cnt))
        (t
         (incf off (floor i 2))
         (decf i cnt))))))

(defun mb-length (string)
  (count 1 (mbs-bitmap string)))

(defun naive-match (pat str)
  (dotimes (i (- (1+ (length str)) (length pat)))
    (let ((mis (mismatch pat (rtl:slice str i))))
      (when (or (null mis)
                (= mis (length pat)))
        (return-from naive-match i)))))

(defun kmp-table (pat)
  (let ((rez (make-array (length pat)))
        (i 0))  ; prefix length
    (setf (aref rez 0) -1)
    (loop :for j :from 1 :below (length pat) :do
      (if (char= (char pat i) (char pat j))
          (setf (aref rez j) (aref rez i))
          (progn ;; we have to use parallel version of setf here
            (psetf (aref rez j) i
                   i (aref rez i))
            (loop :while (and (>= i 0)
                              (not (char= (char pat i)
                                          (char pat j))))
                  :do (setf i (aref rez i)))))
      (incf i))
    rez))

(defun kmp-match (pat str)
  (let ((s 0)
        (p 0)
        (ff (kmp-table pat)))
    (loop :while (< s (length str)) :do
      (if (char= (char pat p) (char str s))
          ;; if the current chars match
          (if (= (1+ p) (length pat))
              ;; if we reached the end of the pattern - success
              (return (- s p))
              ;; otherwise, match the subsequent chars
              (setf p (1+ p)
                    s (1+ s)))
          ;; if the characters don't match
          (if (= -1 (aref ff p))
              ;; shift the pattern for the whole length 
              (setf p 0
                    ;; and skip to the next char in the string
                    s (1+ s))
              ;; try matching the current char again,
              ;; shifting the pattern to align the prefix
              ;; with the already matched part
              (setf p (aref ff p)))))))

(defun rk-match (pat str)
  (let ((len (length pat))
        (phash (rk-hash pat)))
    (loop :for i :from len :to (length str)
          :for beg := (- i len)
          :for shash := (rk-hash (rtl:slice str 0 len))
            :then (rk-rehash shash len
                             (char str (1- beg)) (char str (1- i)))
          :when (and (= phash shash)
                     (string= pat (rtl:slice str beg (+ beg len))))
            :collect beg)))

(defun rk-hash-naive (str)
  (loop :for ch :across str :sum (char-code ch)))

(defun rk-hash (str)
  (assert (> (length str) 0))
  (let ((rez (char-code (char str 0))))
    (loop :for ch :across (rtl:slice str 1) :do
      (setf rez (+ (rem (* rez 256) 101)
                   (char-code ch))))
    (rem rez 101)))

(defun rk-rehash (hash len ch1 ch2)
  (rem (+ (* 256
             (+ hash 101
                (- (rem (* (char-code ch1)
                           (loop :repeat (max 0 (- len 2))
                                 :with val := 256
                                 :do (setf val (rem (* val 256) 101))
                                 :finally (return val)))
                        101))))
           (char-code ch2))
       101))

(deftest match ()
  (should be = 0 (naive-match "foo" "foobar"))
  (should be = 3 (naive-match "bar" "foobar"))
  (should be null (naive-match "baz" "foobar"))
  (should be = 0 (kmp-match "foo" "foobar"))
  (should be = 3 (kmp-match "bar" "foobar"))
  (should be null (kmp-match "baz" "foobar"))
  (should be equal '(0) (rk-match "foo" "foobar"))
  (should be equal '(3) (rk-match "bar" "foobar"))
  (should be equal '(0 6) (rk-match "bar" "barfoobar"))
  (should be null (rk-match "baz" "foobar")))

(defun re-match (regex text)
  "Search for REGEX anywhere in TEXT."
  (if (rtl:starts-with "^" regex)
      (when (> (length regex) 1)
        (match-here (rtl:slice regex 1) text))
      (dotimes (i (length text))
        (when (match-here regex (rtl:slice text i))
          (return t)))))

(defun match-here (regex text)
  "Search for REGEX at beginning of TEXT."
  (cond ((= 0 (length regex))
         t)
        ((and (> (length regex) 1)
              (char= #\* (char regex 1)))
         (match-star (char regex 1) (rtl:slice regex 2) text))
        ((string= "$" regex)
         (= 0 (length text)))
        ((and (> (length text) 0)
              (member (char regex 0) (list #\. (char text 0)))
              (match-here (rtl:slice regex 1) (rtl:slice text 1))))))

(defun match-star (c regex text)
  "Search for C*REGEX at beginning of TEXT."
  (loop
    (when (match-here regex text) (return t))
    (setf text (rtl:slice text 1))
    (unless (and (> (length text) 0)
                 (member c (list #\. (char text 0))))
      (return))))

(deftest re-match ()
  (should be null (re-match "foo" "bar"))
  (should be rtl:true (re-match "foo" "foo"))
  (should be rtl:true (re-match "bar" "foobar"))
  (should be rtl:true (re-match "f.o" "foo"))
  (should be rtl:true (re-match "^foo" "foobar"))
  (should be null (re-match "^bar" "foobar"))
  (should be null (re-match "foo$" "foobar"))
  (should be rtl:true (re-match "bar$" "foobar"))
  (should be rtl:true (re-match "fo*" "foobar")))

(define-condition check-start-anchor () ())

(defgeneric th-part (next-state kind &rest args)
  (:documentation
   "Emit the TH-STATE structure of a certain KIND
    (which may be a keyword or a raw string)
    using the other ARGS and pointing to NEXT-STATE struct.")
  (:method (next-state (kind (eql :sequence)) &rest args)
    (apply 'th-part (if (rest args)
                        (apply 'th-part :sequence (rest args))
                        next-state)
           (first args)))
  (:method (next-state (kind (eql :greedy-repetition)) &rest args)
    ;; this method can handle *, +, {n}, and {n,m} regex modifiers
    ;; in any case, there's a prefix sequence of fixed nonnegative length
    ;; of identical elements that should unconditionally match,
    ;; followed by a bounded or unbounded sequence that,
    ;; in case of a failed match, transitions to the next state
    (apply 'th-part
           (let ((*initial-state* next-state))
             (apply 'th-part next-state :sequence
                    (loop :repeat (or (second args) 1)
                          :collect (rtl:mklist (third args)))))
           :sequence (loop :repeat (first args)
                           :collect (rtl:mklist (third args)))))
  (:method (next-state (kind character) &rest args)
    (th-state kind next-state
              ;; Usually, *initial-state* will be null,
              ;; i.e. further computations along this path will be aborted,
              ;; but, for some variants (? or *), they will just continue
              ;; normally to the next state.
              ;; The special variable controls this setting,
              ;; as you can see in the method for :greedy-repetition 
              t *initial-state*))
  (:method (next-state (kind (eql :end-anchor)) &rest args)
    (th-state nil *matched-state*
              t *initial-state*))
  (:method (next-state (kind (eql :start-anchor)) &rest args)
    ;; This part is unique as all the other parts consume the next character
    ;; (we're not implementing lookahead here), but this one shouldn't.
    ;; To implement such behavior without the additional complexity
    ;; of passing the search string to this function (which we'll still
    ;; probably need to do later on, but were able to avoid so far),
    ;; we can resort to a cool Lisp technique of signaling a condition
    ;; that can be handled specially in the top-level code
    (signal 'check-start-anchor)
    next-state))

(defun run-nfa (nfa str)
  (let ((i 0)
        (start 0)
        (matches (list))
        (states (list nfa)))
    ;; this is the counterpart for the start-anchor signal
    (handler-bind ((check-start-anchor
                     ;; there's no sense to proceed matching
                     ;; a ^... regex if the string is not
                     ;;at its start
                     (lambda (c)
                       (when (> i 0) (return-from run-nfa)))))
      (dovec (char (concatenate 'vector str
                                #(nil)))  ; end-anchor 
             (let ((new-states (list)))
               (dolist (state states)
                 (dolist (tr (th-state-transitions state))
                   (when (th-match tr char)
                     (case (rtl:rt tr)
                       (*matched-state* (push start matches))
                       ((nil) )  ; ignore it
                       (t (pushnew (rtl:rt tr) new-states)))
                     (return))))
               (if new-states
                   (setf states new-states)
                   (setf states (list nfa)
                         start nil)))
             (incf i)
             (unless start (setf start i))))
    matches))

;; TODO (deftest nfa ()

(defstruct grammar
  rules
  max-length)

(defmacro grammar (&rest rules)
  `(make-grammar
    :rules (rtl:pairs->ht (mapcar (lambda (rule)
                                    (rtl:pair (nthcdr 2 rule) (first rule)))
                                  ',rules)
                          :test 'equal)
    :max-length
    (let ((max 0))
      (dolist (rule ',rules)
        ;; Here, #1= and #1# are reader-macros for capturing
        ;; a form and re-evaluating it again
        (when (> #1=(length (nthcdr 2 rule)) max)
          (setf max #1#)))
      max)))

(defun parse (grammar queue)
  (let ((stack (list)))
    (loop :while queue :do
      (print stack)  ; diagnostic output
      (rtl:if-it (find-rule stack grammar)
                 ;; reduce
                 (dotimes (i (length (cdr rtl:it))
                             (push rtl:it stack))
                   (pop stack))
                 ;; shift
                 (push (pop queue) stack))
      :finally (return (find-rule stack grammar)))))

(defun find-rule (stack grammar)
  (let (prefix)
    (loop :for item in stack
          :repeat (grammar-max-length grammar) :do
       (push (first (rtl:mklist item)) prefix)
       (rtl:when-it (rtl:? grammar 'rules prefix)
         ;; otherwise parsing will fail with a stack
         ;; containing a number of partial subtrees
         (return (cons rtl:it (reverse (subseq stack 0 (length prefix)))))))))

(deftest parse ()
  (let ((*standard-output* (make-broadcast-stream)))
    (should be equal '(S (NP DET ADJ NOUN)
                         (VP VERB
                             (VP VERB
                                 (NP PRP$ NOUN)))
                         |.|)
            (parse (grammar (S -> NP VP |.|)
                            (NP -> DET ADJ NOUN)
                            (NP -> PRP$ NOUN)
                            (VP -> VERB VP)
                            (VP -> VERB NP))
                   '(DET ADJ NOUN VERB VERB PRP$ NOUN |.|)))))
