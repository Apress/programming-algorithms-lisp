(in-package :cl-user)

(defpackage #:progalgs
  (:use :common-lisp #:should-test)
  (:export))


(in-package #:progalgs)

(defun approx= (x y)
  (< (/ (abs (- x y))
        (+ x y))
     0.1))
