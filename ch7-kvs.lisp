(in-package :progalgs)

(defun alist-del (key alist)
  (loop :for tail := alist :then (rest tail) :while tail
        :for prev := alist :then tail
        ;; a more general version of the fuction will take
        ;; an additional :test argument instead of hardcoding EQL
        :when (eql key (car (first tail)))
          :do (return (if (eql prev alist)
                          ;; special case of the first item
                          (rest alist)
                          (progn (setf (rest prev) (rest tail))
                                 alist)))
        :finally (return alist)))

(deftest alist-del ()
  (should be null (alist-del :foo (list (cons :foo 42))))
  (should be equal '((:bar . :baz))
          (alist-del :foo (list (cons :foo 42) (cons :bar :baz)))))

(defun start-memoizing (fn)
  (stop-memoizing fn)
  (setf (symbol-function fn)
        (let ((table (make-hash-table :test 'equal))
              (vanilla-fn (symbol-function fn)))
          (setf (get fn :cache) table
                (get fn :fn) vanilla-fn)
          (lambda (&rest args)
            (rtl:getsethash (format nil "~{~A~^|~}" args)
                            table
                            (apply vanilla-fn args))))))

(defun stop-memoizing (fn)
  ;; WHEN-IT is a so called anaphoric macro, from RUTILS, that assigns
  ;; the value of its first argument to an implicitly created variable IT
  ;; and evaluates the body when IT isn't null
  (rtl:when-it (get fn :fn)
    (setf (symbol-function fn) rtl:it
          (get fn :fn) nil)))

;; TODO: add memoization tests

(defun find-candidate-second-chance (bitmap)
  (declare (type bit-vector bitmap))
  (position 0 bitmap))

(let ((i 0))
  (defun find-candidate-clock (bitmap)
    (declare (type (vector bit) bitmap))
    (loop :with len := (length bitmap)
          :until (zerop (aref bitmap i))
          :do (setf (aref bitmap i) 0)
              (setf i (mod (1+ i) len)))
    i))

;; TODO: add cache eviction code & tests
