(in-package :progalgs)

(defun birthday-collision-prob (n)
  (let ((rez 1))
    (dotimes (i n)
      (setf rez (* rez (/ (- 365 i) 365))))
    ;; don't forget that we want the complement of the probability
    ;; of no collisions, hence (- 1.0 ...)
    (- 1.0 rez)))

(defun hash-collision-prob (n size)
  (let ((rez 1))
    (dotimes (i n)
      (setf rez (* rez (/ (- size i) size))))
    (- 1.0 rez)))

(deftest collision-probs ()
  (should be = 0.4114384 (birthday-collision-prob 20))
  (should be = 0.9996371 (hash-collision-prob 10 10))
  (should be = 0.9345271 (hash-collision-prob 10 20))
  (should be = 0.37184352 (hash-collision-prob 10 100))
  (should be = 0.004491329 (hash-collision-prob 10 10000))
  (should be approx= 0.63 (hash-collision-prob 20 200)))

(defstruct ht
  array
  (count 0))

(defun ht (&rest kvs)
  (let ((rez (make-ht :array (make-array 16 :initial-element (list)))))
    (loop :for (k v) :in kvs :do
      (add-ht k v rez))
    rez))

(defun ht-get (key ht)
  (rtl:with ((size (length (rtl:? ht 'array)))
             (start (rem (hash key) size)))
    (do ((count 0 (1+ count))
         (i start (rem (1+ i) size))
         (item (rtl:? ht 'array start)
               (rtl:? ht 'array i)))
        ((or (null item)
             (= count size)))
      (when (eql key (car item))
        (return 
          (values (cdr item)
                  ;; the second value is an index, at which the item was found
                  ;; (also used to distinguish the value nil from not found,
                  ;; which is also represented by nil but with no second value)
                  i))))))

(defun ht-add (key val ht)
  (rtl:with ((array (ht-array ht))
             (size (length array)))
            ;; flet defines a local function that has access
            ;; to the local variables defined in HT-ADD
    (flet ((add-item (k v)
             (do ((i (rem (hash k) size)
                     (rem (1+ i) size)))
                 ((null (rtl:? ht 'array i))
                  (setf (rtl:? ht 'array i) (cons k v)))
               ;; this do-loop doesn't have a body
               )))
      (when (= (hash-table-count ht) size)
        ;; when the backing array is full
        ;; expand it to have the length equal to the next power of 2
        (setf size (expt 2 (ceiling (log (1+ count) 2)))
              (rtl:? ht 'array) (make-array size :initial-element nil))
        ;; and re-add its contents
        (rtl:dovec (item array)
          (add-item (car item) (cdr item)))
        ;; finally, add the new item
        (incf (rtl:? ht 'count))
        (add-item key val)))))
      
(defun ht-rem (key ht)
  ;; here, we use the index of the item returned as the 2nd value of HT-GET
  (rtl:when-it (nth-value 1 (ht-get key ht))
    (setf (rtl:? ht 'array rtl:it) nil)
    ;; return the index to indicate that the item was found
    rtl:it))

;; TODO add ht tests


(defparameter *fnv-primes*
  '((32 . 16777619)
    (64 . 1099511628211)
    (128 . 309485009821345068724781371)
    (256 . 374144419156711147060143317175368453031918731002211)))

(defparameter *fnv-offsets*
  '((32 . 2166136261)
    (64 . 14695981039346656037)
    (128 . 144066263297769815596495629667062367629)
    (256 . 100029257958052580907070968620625704837092796014241193945225284501741471925557)))

(defun fnv-1a (x &key (bits 32))
  (assert (member bits '(32 64 128 256)))
  (let ((rez (rtl:assoc1 bits *fnv-offsets*))
        (prime (rtl:assoc1 bits *fnv-primes*)))
    (dotimes (i (/ bits 8))
      (setf rez (ldb (byte bits 0)
                     (* (logxor rez (ldb (byte 8 (* i 8)) x))
                        prime))))
    rez))

(defun fnv-1a-str (str)
  (let ((rez (rtl:assoc1 32 *fnv-offsets*))
        (prime (rtl:assoc1 32 *fnv-primes*)))
    (rtl:dovec (char str)
               (setf rez (ldb (byte 32 0)
                              (* (logxor rez (char-code char))
                                 prime))))
    rez))

(defun djb2-str (str)
  (let ((rez 5381))  ; a DJB2 prime number
    (rtl:dovec (char str)
      (setf rez (ldb (byte 32 0)
                     (+ (char-code char)
                         (ldb (byte 32 0)
                              (+ (ash rez 5)
                                  rez))))))
    rez))

(deftest hash-functions ()
  )

(defstruct default-hash-table
  (table (make-hash-table))
  default-value)

(defun gethash-default (key ht)
  (gethash key (rtl:? ht 'table) (rtl:? ht 'default-value)))

(defmethod generic-elt ((kv default-hash-table) key &rest keys)
  (gethash-default key kv))

(deftest default-hash-table ()
  (should be = 42
          (gethash-default :foo (make-default-hash-table :default-value 42))))

(defstruct linked-hash-table-item
  key
  val
  next)

(defstruct linked-hash-table
  (table (make-hash-table))
  head
  tail)

(defun gethash-linked (key ht)
  ;; we use GETHASH instead of a shorter (rtl:? ht 'table key 'val)
  ;; to preserve the second return value
  (gethash key (rtl:? ht 'table)))

(defun sethash-linked (key ht val)
  ;; The initial order of items is the order of addition.
  ;; If we'd like to impose a different order, we'll have to perform reordering
  ;; after each addition or implement a custom sethash function.
  (with-slots (table head tail) ht
    (rtl:if-it (gethash key table)
               (setf (rtl:? rtl:it 'val) val)
               (let ((new (make-linked-hash-table-item
                           :key key :val val)))
                 (rtl:sethash key table new)
                 (when (null head)
                   (setf (rtl:? ht 'head) new))
                 (setf (rtl:? ht 'tail)
                       (if tail
                           (setf (rtl:? ht 'tail 'next) new)
                           new))))))

(deftest linked-ht ()
  (let ((ht (make-linked-hash-table)))
    (sethash-linked :foo ht 42)
    (sethash-linked :bar ht 43)
    (sethash-linked :baz ht 44)
    (should be equal '(42 43 44)
            (loop :for cur := (linked-hash-table-head ht)
                    :then (linked-hash-table-item-next cur)
                  :collect (linked-hash-table-item-val cur)
                  :until (eql cur (linked-hash-table-tail ht))))))

(defmethod mapkv (fn (ht linked-hash-table))
  (let ((rez (make-linked-hash-table
              :table (make-hash-table
                      :test (hash-table-test (rtl:? ht 'table))))))
    (do ((item (rtl:? ht 'head) (rtl:? item 'next)))
        ((null item))
      (let ((k (rtl:? item 'key)))
        (sethash-linked k rez (funcall fn k (rtl:? item 'val)))))
    rez))

(defun content-address (object)
  (sha1:sha1-hex (with-output-to-string (out)
                   (format out "~A:" (class-of object))
                   (print-object object out))))

(defun ca-get-object (address repo)
  (gethash address repo))

(defun ca-add-object (object repo)
  (let ((addr (content-address object)))
    (values (rtl:set# addr repo object)
            addr)))

(defun ca-rem-object (object repo)
  (remhash (content-address object) repo))

(defun content-address2 (object)
  ;; here, we use SHA1-DIGEST to get the numeric
  ;; value (as a sequence of bytes) of the hash
  ;; instead of its string representation
  ;; that was previously obtained from SHA1-HEX
  (let ((hash (sha1:sha1-digest
               (with-output-to-string (out)
                 (format out "~A:" (class-of object))
                 (print-object object out)))))
    (rtl:pair (elt hash 0)
              ;; the cryptic format ~{~2,'0X~} is used
              ;; to print numbers in hex (X) with a fixed length
              ;; of 2 chars padded by zeroes from the left
              (format nil "~{~2,'0X~}" (subseq hash 1)))))

(defun ca-get-object2 (address2 repo)
  (apply 'rtl:? repo address2))

(defun ca-add-object2 (object repo)
  (rtl:with (((top addr) (content-address2 object))
             (subrepo (rtl:getset# top repo
                                   (make-hash-table :test 'equal))))
            (values (rtl:set# addr subrepo object)
                    (rtl:pair top addr))))

(defun ca-rem-object2 (object repo)
  (rtl:with (((top addr) (content-address2 object)))
            (rtl:when-it (gethash top repo)
                         (remhash addr rtl:it))))

(deftest content-adressing ()
  (let ((repo (make-hash-table :test 'equal))
        (repo2 (make-hash-table :test 'equal)))
    (should be string= "test" "514BE1254CC9825EE125651650B5F9F6CF5C55D9"
            (ca-add-object "test" repo))
    (should be string= "test"
            (gethash "514BE1254CC9825EE125651650B5F9F6CF5C55D9" repo))
    (ca-add-object2 "foo" repo2)
    (ca-add-object2 "bar" repo2)
    (should be string= "foo"
            (gethash "8AB31BA5528396616249FCA3879C734FF3440D" (gethash 138 repo2)))
    (should be string= "bar"
            (gethash "F50F210FA56B285C6DA1B09C72782791BBB15A" (gethash 195 repo2)))))
