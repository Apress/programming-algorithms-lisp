(in-package :progalgs)


(defstruct node
  id edges)

(defstruct edge
  src dst label)

(defstruct (graph (:conc-name nil) (:print-object pprint-graph))
  (nodes (make-hash-table)))  ; mapping of node ids to nodes

(defun pprint-graph (graph stream)
  (let ((ids (sort (rtl:keys (nodes graph)) '<)))
    (format stream "~{    ~A~}~%" ids)  ; here, Tab is used for space
    (dolist (id1 ids)
      (let ((node (rtl:? graph 'nodes id1)))
        (format stream "~A" id1)
        (dolist (id2 ids)
          (format stream "    ~:[~;x~]"   ; here, Tab as well
                  (find id2 (rtl:? node 'edges) :key 'edge-dst)))
        (terpri stream)))))

(defun init-graph (edges)
  (rtl:with ((rez (make-graph))
             (nodes (nodes rez)))
            (loop :for (src dst) :in edges :do
              (let ((src-node (rtl:getsethash src nodes (make-node :id src))))
                (rtl:getset# dst nodes (make-node :id dst))
                (push (make-edge :src src :dst dst)
                      (rtl:? src-node 'edges))))
            rez))

(deftest graph ()
  (should print-to *standard-output*
          "
    1    2    3    4    5    6    7    8
1        x    x                    
2                x    x            
3                x    x            
4                        x        
5                x        x        
6                                
7                                x
8                                
 "
          (print (init-graph '((7 8)
                               (1 3)
                               (1 2)
                               (3 4)
                               (3 5)
                               (2 4)
                               (2 5)
                               (5 4)
                               (5 6)
                               (4 6))))))

(defun topo-sort (graph)
  (let ((nodes (nodes graph))
        (visited (make-hash-table))
        (rez (rtl:vec)))
    (rtl:dokv (id node nodes)
      (unless (gethash id visited)
        (visit node nodes visited rez)))
    rez))

(defun visit (node nodes visited rez)
  (dolist (edge (node-edges node))
    (rtl:with ((id (edge-dst edge))
               (child (gethash id nodes)))
      (unless (find id rez)
        (assert (not (gethash id visited)) nil
                "The graph isn't acyclic for vertex: ~A" id)
        (setf (gethash id visited) t)
        (visit child nodes visited rez))))
  (vector-push-extend (node-id node) rez)
  rez)

(deftest topo-sort ()
  (should be equalp #(8 7 6 4 5 2 3 1)
          (topo-sort (init-graph '((7 8)
                                   (1 3)
                                   (1 2)
                                   (3 4)
                                   (3 5)
                                   (2 4)
                                   (2 5)
                                   (5 4)
                                   (5 6)
                                   (4 6))))))

(defvar *heap-indices*)

(defun prim-mst (graph)
  (let ((initial-weights (list))
        (mst (list))
        (total 0)
        (*heap-indices* (make-hash-table))
        weights
        edges
        cur)
    (rtl:dokv (id node (nodes graph))
              (if cur
                  (push (rtl:pair id (or (elt edges id)
                                         ;; a standard constant that is
                                         ;; a good enough substitute for infinity
                                         most-positive-fixnum))
                        initial-weights)
                  (setf cur id
                        edges (node-edges node))))
    (setf weights (heapify initial-weights))
    (loop
      (rtl:with (((id weight) (heap-pop weights)))
                (unless id (return))
                (when (elt edges id)
                  ;; if not, we have moved to the new connected component
                  ;; so there's no edge connecting it to the previous one
                  (push (rtl:pair cur id) mst)
                  (incf total weight))
                (rtl:dokv (id w edges)
                          (when (< w weight)
                            (heap-decrease-key weights id w)))
                (setf cur id
                      edges (rtl:? graph 'nodes id 'edges))))
    (values mst
            total)))

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
          (rotatef (gethash (aref vec beg) *heap-indices*)
                   (gethash (aref vec child) *heap-indices*))
          (rotatef (aref vec beg)
                   (aref vec child))
          (heap-down vec child end)))))
  vec)

(defun heap-decrease-key (vec key decrement)
  (let ((i (pop (gethash key *heap-indices*))))
    (unless i (error "No key ~A found in the heap: ~A" key vec))
    (when (null (gethash key *heap-indices*))
      (remhash key *heap-indices*))
    (push i (gethash (- key decrement) *heap-indices*))
    (decf (aref vec i) decrement)
    (heap-up vec i)))

(defun heap-up (vec i)
  (rtl:with ((i-key (aref vec i))
             (parent (hparent i))
             (parent-key (aref vec parent)))
    (when (> i-key parent-key)
      (rtl:removef (gethash i-key *heap-indices*) i)
      (rtl:removef (gethash parent-key *heap-indices*) parent)
      (push i (gethash parent-key *heap-indices*))
      (push parent (gethash i-key *heap-indices*))
      (rotatef (aref vec i)
               (aref vec parent))
      (heap-up vec parent)))
  vec)

(defun heap-up-correct (vec i)
  (let ((parent (hparent i)))
    (when (> (aref vec i)
             (aref vec parent))
      (rotatef (gethash (aref vec i) *heap-indices*)
               (gethash (aref vec parent) *heap-indices*)))
    (rotatef (aref vec i)
             (aref vec parent))
    (heap-up vec parent))
  vec)

(defun heap-decrease-key-correct (vec key decrement)
  (let ((i (gethash key *heap-indices*)))
    (unless i (error "No key ~A found in the heap: ~A" key vec))
    (remhash key *heap-indices*)
    (setf (gethash (- key decrement) *heap-indices*) i)
    (decf (aref vec i) decrement)
    (heap-up vec i)))

(defstruct heap-item
  key val)

(defun heap-up (vec i)
  (rtl:with ((i-key (heap-item-key (aref vec i)))
             (parent (hparent i))
             (parent-key (heap-item-key (aref vec parent)))) 
    (when (> i-key parent-key)
      (rtl:removef (gethash i-key *heap-indices*) i)
      (rtl:removef (gethash parent-key *heap-indices*) parent)
      (push i (gethash parent-key *heap-indices*))
      (push parent (gethash i-key *heap-indices*))
      (rotatef (aref vec i)
               (aref vec parent))
      (heap-up vec parent)))
  vec)

;; TODO test heap

(defstruct (spf-node (:include node))
  (weight most-positive-fixnum)
  (path (list)))

(defun spf (graph src dst)
  (rtl:with ((nodes (graph-nodes graph))
             ;; the following code should express initialize the heap
             ;; with a single node of weight 0 and all other nodes
             ;; of weight MOST-POSITIVE-FIXNUM
             ;; (instead of running a O(n*log n) HEAPIFY)
             (weights (init-weights-heap nodes src)))
    (loop
      (rtl:with (((id weight) (heap-pop weights)))
        (cond ((eql id dst)
               (let ((dst (elt nodes dst)))
                 ;; we return two values: the path and its length
                 (return (values (cons dst (spf-node-path dst))
                                 (spf-node-weight dst)))))
              ((= most-positive-fixnum weight)
               (return)))       ; no path exists
        (dolist (edge (rtl:? nodes id 'edges))
          (rtl:with ((cur (edge-dst edge))
                     (node (elt nodes cur))
                     (w (+ weight (spf-node-weight cur))))
            (when (< w (spf-node-weight node))
              (heap-decrease-key weights cur w) 
              (setf (spf-node-weight node) w
                    (spf-node-path node) (cons (rtl:? nodes id)
                                               (rtl:? nodes id 'path))))))))))

;; TODO test spf

(defstruct mf-edge
  beg end capacity)

(defun max-flow (g)
  (assert (= (array-dimension g 0)
             (array-dimension g 1)))
  (let ((rg (rtl:copy-array g))             ; residual graph
        (rez 0))
    (loop :for path := (aug-path rg) :while path :do
      (let ((flow most-positive-fixnum))
        ;; the flow along the path is the residual capacity of the thinnest edge
        (dolist (edge path)
          (let ((cap (mf-edge-capacity edge)))
            (when (< (abs cap) flow)
              (setf flow (abs cap)))))
        (dolist (edge path)
          (with-slots (beg end) edge
            (decf (aref rg beg end) flow)
            (incf (aref rg end beg) flow)))
        (incf rez flow)))
    rez))

(defun aug-path (g)
  (rtl:with ((sink (1- (array-dimension g 0)))
             (visited (make-array (1+ sink) :initial-element nil)))
    (labels ((dfs (g i)
               (setf (aref visited i) t)
               (if (zerop (aref g i sink))
                   (dotimes (j sink)
                     (unless (or (zerop (aref g i j))
                                 (aref visited j))
                       (rtl:when-it (dfs g j)
                         (return (cons (make-mf-edge
                                        :beg i :end j
                                        :capacity (aref g i j))
                                       rtl:it)))))
                   (list (make-mf-edge
                          :beg i :end sink
                          :capacity (aref g i sink))))))
      (dfs g 0))))

(deftest max-flow ()
  (should be = 7 (max-flow #2A((0 4 4 0 0 0)
                               (0 0 0 4 2 0)
                               (0 0 0 1 2 0)
                               (0 0 0 0 0 3)
                               (0 0 0 0 0 5)
                               (0 0 0 0 0 0)))))

;; code prototypes

(defun pagerank (g &key (d 0.85) (repeat 100))
  (rtl:with ((nodes (nodes g))
             (n (length nodes))
             (pr (make-array n :initial-element (/ 1 n))))
    (loop :repeat repeat :do
      (let ((pr2 (map 'vector (lambda (x) (- 1 (/ x n)))
                      pr)))
        (rtl:dokv (i node nodes)
          (let ((p (aref pr i))
                (m (length (node-children node))))
            (rtl:dokv (j _ (node-children node))
              (incf (aref pr2 j) (* d (/ p m))))))
        (setf pr pr2)))
    pr))

(defun pr1 (node n p &key (d 0.85))
  (let ((pr (make-array n :initial-element 0))
        (m (hash-table-count (node-children node))))
    (rtl:dokv (j child (node-children node))
      (setf (aref pr j) (* d (/ p m))))
    pr))

(defun pagerank-mr (g &key (d 0.85) (repeat 100))
  (rtl:with ((n (length (nodes g)))
             (pr (make-array n :initial-element (/ 1 n))))
    (loop :repeat repeat :do
      (setf pr (map 'vector (lambda (x) (- 1 (/ x n)))
                    (reduce 'vec+ (map 'vector (lambda (node p)
                                                 (pr1 node n p :d d))
                                       (nodes g)
                                       pr)))))
    pr))
