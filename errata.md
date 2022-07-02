# Errata for *Programming Algorithms in Lisp*

On **page 79** Function `dwim-map` leaves the first sequence out. It should be:

```
(defun dwim-map (fn seq &rest seqs)
  "A thin wrapper over MAP that uses the type of the first SEQ for the result."
  (apply 'map (type-of seq) fn (cons seq seqs)))
```

***

On **page 109** Method `generic-elt` for vectors calculated index wrong for
negative keys. It should be:

```
(defmethod generic-elt ((obj vector) key &rest keys)
  (declare (ignore keys))
  ;; Python-like handling of negative indices as offsets from the end
  (when (minusp key) (setf key (+ (length obj) key)))
  (aref obj key))
```

***

On **page xx** [Summary of error]:
 
Details of error here. Highlight key pieces in **bold**.

***