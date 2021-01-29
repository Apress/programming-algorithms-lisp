(in-package :progalgs)


(defstruct point-v0
  parent)  ; if the parent is null the point is the root

(defun uf-union-v0 (point1 point2)
  "Join the subsets of POINT1 and POINT2."
  (setf (point-v0-parent point1) (or (point-v0-parent point2)
                                     point2)))

(defun uf-find-v0 (point)
  "Determine the id of the subset that a POINT belongs to."
  (let ((parent (point-v0-parent point)))
    (if parent
        (uf-find-v0 parent)
        point)))

(defstruct point
  parent
  (size 1))

(defun uf-find (point)
  (let ((parent (point-parent point)))
    (if parent
        ;; here, we use the fact that the assignment will also return
        ;; the value to perform both path compression and find
        (setf (point-parent point) (uf-find parent))
        point)))

(defun uf-union (point1 point2)
  (rtl:with ((root1 (uf-find point1))
             (root2 (uf-find point2))
             (major minor (if (> (point-size root1)
                                 (point-size root2))
                              (values root1 root2)
                              (values root2 root1))))
            (incf (point-size major) (point-size minor))
            (setf (point-parent minor) major)))

(defun uf-disjoint (points)
  "Return true if all of the POINTS belong to different subsets."
  (let ((roots (list)))
    (dolist (point points)
      (let ((root (uf-find point)))
        (when (member root roots)
          (return-from uf-disjoint nil))
        (push root roots))))
  t)

;; TODO: add tests for Union-Find
