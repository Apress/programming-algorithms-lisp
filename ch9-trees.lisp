(in-package :progalgs)


(defstruct (tree-node (:conc-name nil))
  key
  children)  ; instead of linked list's next

(defun dfs-node (fn root)
  (funcall fn (key root))
  (dolist (child (children root))
    (dfs-node fn child)))
  
(defmacro dotree-dfs ((value root) &body body)
  (let ((node (gensym))) ; GENSYM is a fresh symbol
                         ; used to prevent possible symbol
                         ; collisions for NODE
    `(dfs-node (lambda (,node)
                 (let ((,value (key ,node)))
                   ,@body))
               ,root)))

(defun rank (node)
  (let ((size 0))
    (dotree-dfs (_ node)
      (incf size))
    (log size 2)))

(defun dfs-list (fn tree)
  ;; we need to handle both subtrees (lists) and
  ;; leaves (atoms) — so, we'll just convert
  ;; everything to a list
  (let ((tree (rtl:mklist tree)))
    (funcall fn (first tree))
    (dolist (child (rest tree))
      (dfs-list fn child))))

(defun post-dfs (fn node)
  (dolist (child (children node))
    (post-dfs fn child))
  (funcall fn (key node)))

(deftest dfs ()
  (let ((tree (rtl:with ((f (make-tree-node :key "f"))
                         (e (make-tree-node :key "e"))
                         (d (make-tree-node :key "d"))
                         (c (make-tree-node :key "c" :children (list f)))
                         (b (make-tree-node :key "b" :children (list d e))))
                (make-tree-node :key "a"
                                :children (list b c)))))
    (should print-to *standard-output* "
\"a\" 
\"b\" 
\"d\" 
\"e\" 
\"c\" 
\"f\" " (dfs-node 'print tree))
    (should print-to *standard-output* "
DEFUN 
FOO 
BAR 
\"Foo function.\" 
BAZ 
BAR " (dfs-list 'print '(defun foo (bar)
                        "Foo function."
                        (baz bar))))
    (should print-to *standard-output* "
\"d\" 
\"e\" 
\"b\" 
\"f\" 
\"c\" 
\"a\" " (post-dfs 'print tree))))


(defun bfs (fn nodes)
  (let ((next-level (list)))
    (dolist (node (rtl:mklist nodes))
      (funcall fn (key node))
      (dolist (child (children node))
        (push child next-level)))
    (when next-level
      (bfs fn (reverse next-level)))))

(deftest bfs ()
  (let ((tree (rtl:with ((f (make-tree-node :key "f"))
                         (e (make-tree-node :key "e"))
                         (d (make-tree-node :key "d"))
                         (c (make-tree-node :key "c" :children (list f)))
                         (b (make-tree-node :key "b" :children (list d e))))
                (make-tree-node :key "a"
                                :children (list b c)))))
    (should print-to *standard-output* "
\"a\" 
\"b\" 
\"c\" 
\"d\" 
\"e\" 
\"f\" " (bfs 'print tree))))

(defstruct (bst-node (:conc-name nil)
                     (:print-object (lambda (node out)
                                      (format out "[~a-~@[~a~]-~@[~a~]]"
                                              (key node)
                                              (lt node)
                                              (rt node)))))
  key
  val  ; we won't use this slot in the examples,
                                        ; but without it, in real-world use cases,
                                        ; such a tree doesn't have any value ;)
  lt   ; left child
  rt)  ; right child

(defun tree-rotate (node parent grandparent)
  (cond
    ((eql node (lt parent)) (setf (lt parent) (rt node)
                                  (rt node) parent))
    ((eql node (rt parent)) (setf (rt parent) (lt node)
                                  (lt node) parent))
    (t (error "NODE (~A) is not the child of PARENT (~A)"
              node parent)))
  (cond 
    ((null grandparent) (return-from tree-rotate node))
    ((eql parent (lt grandparent)) (setf (lt grandparent) node))
    ((eql parent (rt grandparent)) (setf (rt grandparent) node))
    (t (error "PARENT (~A) is not the child of GRANDPARENT (~A)"
              parent grandparent))))

(defun splay (node &rest chain)
  (loop :for (parent grandparent) :on chain :do
    (tree-rotate node parent grandparent))
  node)

(defun node-chain (item root &optional chain)
  "Return as the values the node equal to ITEM or the closest one to it
   and the chain of nodes leading to it, in the splay tree based in ROOT."
  (if root
      (with-slots (key lt rt) root
        (let ((chain (cons root chain)))
          (cond ((= item key) (values root
                                      chain))
                ((< item key) (node-chain item lt chain))
                ((> item key) (node-chain item rt chain)))))
      (values nil
              chain)))

(defun st-search (item root)
  (rtl:with ((node chain (node-chain item root)))
    (values (when node (apply 'splay chain))
            chain)))

(defun st-insert (item root)
  (assert root nil "Can't insert item into a null tree")
  (rtl:with ((node chain (st-search item root)))
    (unless node
      (let ((parent (first chain)))
        ;; here, we use the property of the := expression
        ;; that it returns the item being set
        (push (setf (rtl:? parent (if (> (key parent) item)
                                      'lt
                                      'rt))
                    (make-bst-node :key item))
              chain)))
            (apply 'splay chain)))

(defun idir (dir)
  (case dir
    (rtl:lt 'rt)
    (rtl:rt 'lt)))

(defun closest-child (node)
  (dolist (dir '(lt rt))
    (let ((parent nil)
          (current nil))
      (do ((child (funcall dir node) (funcall (idir dir) child)))
          ((null child) (when current
                          (return-from closest-child
                            (values dir
                                    current
                                    parent))))
        (setf parent current
              current child)))))

(defun st-delete (item root)
  (rtl:with ((node chain (st-search item root))
             (parent (second chain)))
            (if (null node)
                root  ; ITEM was not found
                (rtl:with ((dir child child-parent (closest-child node))
                           (idir (idir dir)))
                  (when parent
                    (setf (rtl:? parent (if (eql (lt parent) node)
                                            'lt
                                            'rt))
                          child))
                  (when child
                    (setf (rtl:? child idir) (rtl:? node idir))
                    (when child-parent
                      (setf (rtl:? child-parent idir) (rtl:? child dir))))
                  (if parent
                      (apply 'splay (rest chain))
                      child)))))

(defun st-update (old new root)
  (st-insert new (st-delete old root)))

(defun pprint-bst (node &optional (level 0) (skip-levels (make-hash-table)))
  (when (= 0 level)
    (format t "~A~%" (key node)))
  (let ((term (make-bst-node :key #\.)))
    (when (or (lt node) (rt node))
      (rtl:doindex (i child (remove nil (list (or (lt node) term)
                                              (or (rt node) term))))
        (let ((last-child-p (= 1 i)))
          (dotimes (j level)
            (format t "~C    " (if (gethash j skip-levels) #\Space #\│)))
          (format t "~C── ~A~%"
                  (if last-child-p #\└ #\├)
                  (key child))
          (:= (gethash level skip-levels) last-child-p)
          (unless (eql child term)
            (pprint-bst child
                        (1+ level)
                        skip-levels)))))))

(deftest splay-tree ()
  (let ((st (make-bst-node :key 5)))
    (should print-to *standard-output* "
[5--] "
            (print st))
    (setf st (st-insert 1 st))
    (should print-to *standard-output* "1
├── .
└── 5
" (pprint-bst st))
    (setf st (st-insert 10 st))
    (should print-to *standard-output* "10
├── 1
│    ├── .
│    └── 5
└── .
" (pprint-bst st))
    (setf st (st-insert 3 st))
    (should print-to *standard-output* "3
├── 1
└── 10
     ├── 5
     └── .
" (pprint-bst st))
    (setf st (st-insert 7 st))
    (should print-to *standard-output* "7
├── 3
│    ├── 1
│    └── 5
└── 10
" (pprint-bst st))
    (setf st (st-insert 8 st))
    (should print-to *standard-output* "8
├── 7
│    ├── 3
│    │    ├── 1
│    │    └── 5
│    └── .
└── 10
"(pprint-bst st))
    (setf st (st-insert 2 st))
    (should print-to *standard-output* "2
├── 1
└── 8
     ├── 7
     │    ├── 3
     │    │    ├── .
     │    │    └── 5
     │    └── .
     └── 10
"(pprint-bst st))
    (setf st (st-insert 4 st))
    (should print-to *standard-output* "4
├── 2
│    ├── 1
│    └── 3
└── 8
     ├── 7
     │    ├── 5
     │    └── .
     └── 10
"(pprint-bst st))
    (should print-to *standard-output* "
[4-[2-[1--]-[3--]]-[8-[7-[5--]-]-[10--]]] "
            (print st))
    (should print-to *standard-output* "5
├── 4
│    ├── 2
│    │    ├── 1
│    │    └── 3
│    └── .
└── 8
     ├── 7
     └── 10
" (pprint-bst (st-search 5 st)))))
 

(defun hparent (i)
  "Calculate the index of the parent of the heap element with an index I."
  (floor (- i 1) 2))

(defun hrt (i)
  "Calculate the index of the right child of the heap element with an index I."
  (* (+ i 1) 2))

(defun hlt (i)
  "Calculate the index of the left child of the heap element with an index I."
  (- (hrt i) 1))

(defun heapify (vec)
  (let ((mid (floor (length vec) 2)))
    (dotimes (i mid)
      (heap-down vec (- mid i 1))))
  vec)

(defun heap-down (vec beg &optional (end (length vec)))
  (let ((l (hlt beg))
        (r (hrt beg)))
    (when (< l end)
      (let ((child (if (or (>= r end)
                           (> (aref vec l)
                              (aref vec r)))
                       l r)))
        (when (> (aref vec child)
                 (aref vec beg))
          (rotatef (aref vec beg)
                   (aref vec child))
          (heap-down vec child end)))))
  vec)

(defun heap-up (vec i)
  (when (> (aref vec i)
           (aref vec (hparent i)))
    (rotatef (aref vec i)
             (aref vec (hparent i)))
    (heap-up vec (hparent i)))
  vec)

(defun draw-heap (vec)
  (format t "~%")
  (rtl:with ((size (length vec))
             (h (+ 1 (floor (log size 2)))))
            (dotimes (i h)
              (let ((spaces (make-list (- (expt 2 (- h i)) 1)
                                       :initial-element #\Space)))
                (dotimes (j (expt 2 i))
                  (let ((k (+ (expt 2 i) j -1)))
                    (when (= k size) (return))
                    (format t "~{~C~}~2D~{~C~}"
                            spaces (aref vec k) spaces)))
                (format t "~%"))))
  (format t "~%")
  vec)

(defun check-heap (vec)
  (dotimes (i (floor (length vec) 2))
    (when (= (hlt i) (length vec)) (return))
    (assert (not (> (aref vec (hlt i)) (aref vec i)))
            () "Left child (~A) is > parent at position ~A (~A)."
            (aref vec (hlt i)) i (aref vec i))
    (when (= (hrt i) (length vec)) (return))
    (assert (not (> (aref vec (hrt i)) (aref vec i)))
            () "Right child (~A) is > than parent at position ~A (~A)."
            (aref vec (hrt i)) i (aref vec i)))
  vec)

(defun heap-push (node vec)
  (vector-push-extend node vec)
  (heap-up vec (1- (length vec))))

(defun heap-pop (vec)
  (rotatef (aref vec 0) (aref vec (- (length vec) 1)))
  ;; PROG1 is used to return the result of the first form
  ;; instead of the last, like it happens with PROGN
  (prog1 (vector-pop vec)
    (heap-down vec 0)))

(defun heapsort (vec)
  (heapify vec)
  (dotimes (i (length vec))
    (let ((last (- (length vec) i 1)))
      (rotatef (aref vec 0)
               (aref vec last))
      (heap-down vec 0 last)))
  vec)

(deftest heap ()
  (should signal simple-error
          (check-heap #(10 5 8 2 3 7 1 9)))
  (should be equalp #(22 13 10 9 3 7 8 5 7 1)
          (check-heap (heapify #(1 22 10 5 3 7 8 9 7 13))))
  (should be equalp #(1 3 5 7 7 8 9 10 13 22)
          (heapsort #(1 22 10 5 3 7 8 9 7 13))))

(defstruct (tr-node (:conc-name nil))
  val
  (children (list)))

(defun tr-lookup (key root)
  (rtl:dovec (ch key
                 ;; when iteration terminates normally
                 ;; we have found the node we were looking for
                 (val root))
             (rtl:if-it (rtl:assoc1 ch (children root))
                        (setf root rtl:it)
                        (return))))

(defun tr-add (key val root)
  (let ((i 0))
    (rtl:dovec (ch key)
               (rtl:if-it (rtl:assoc1 ch (children root))
                          (setf root rtl:it
                                i (1+ i))
                          (return)))
    (if (= i (length key))
        ;; something has already being stored at key -
        ;; so we signal a continuable error that 
        ;; gives the user two options: overwrite or abort
        (cerror "Assign a new value"
                "There was already a value at key: ~A" (val root))
        (rtl:dovec (ch (rtl:slice key i))
                   (let ((child (make-tr-node)))
                     (push (cons ch child) (children root))
                     (setf root child))))
    (setf (val root) val)))

(deftest trie ()
  (let ((trie (make-tr-node)))
    (should be equalp trie
            (read-from-string "#S(TR-NODE :VAL NIL :CHILDREN NIL)"))
    (should be null (tr-lookup "word" trie))
    (should be = 42 (tr-add "word" 42 trie))
    (should be eql #\w (caar (children trie)))
    (should be eql #\o (caar (children (cdar (children trie)))))
    (should be eql #\r (caar (children (cdar (children
                                              (cdar (children trie)))))))
    (should be eql #\d (caar (children (cdar (children
                                              (cdar (children
                                                     (cdar (children trie)))))))))
    (should be = 42 (tr-lookup "word" trie))
    (should signal simple-error (tr-add "word" :foo trie))
    (should be eql :baz (tr-add "we" :baz trie))
    (should be = 2 (length (children (cdar (children trie)))))))

