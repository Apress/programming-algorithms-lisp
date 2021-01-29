(in-package #:asdf-user)

(defsystem #:progalgs
  :version "1.0"
  :description "Code for the book 'Programming Algorithms in Lisp'"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :depends-on (#:rutils #:eager-future2 #:sha1 #:lparallel #:should-test)
  :serial t
  :components ((:file "package")
               (:file "ch1-complexity")
               (:file "ch4-data-structures")
               (:file "ch5-arrays")
               (:file "ch6-lists")
               (:file "ch7-kvs")
               (:file "ch8-hash-tables")
               (:file "ch9-trees")
               (:file "ch10-graphs")
               (:file "ch11-strings")
               (:file "ch12-dynamic-programming")
               (:file "ch13-approximation")
               (:file "ch14-compression")
               (:file "ch15-synchronization")))
