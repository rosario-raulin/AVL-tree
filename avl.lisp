(defpackage :rosario-raulin-de-avl
  (:use :cl)
  (:export
   :make-avl-tree
   :avl-find
   :avl-insert
   :avl-remove))

(in-package :rosario-raulin-de-avl)

(defstruct (avl-node (:constructor make-avl-node (key value)))
  key value left right (height 1))

(defstruct (avl-tree (:constructor make-avl-tree (compare-fn)))
  head compare-fn)

(defun avl-find-node (node key compare-fn)
  (when node
    (let ((cmp (funcall compare-fn key (avl-node-key node))))
      (cond ((zerop cmp) node)
	    ((< cmp 0) (avl-find-node (avl-node-left node) key compare-fn))
	    ((> cmp 0) (avl-find-node (avl-node-right node) key compare-fn))))))

(defun avl-find (tree key)
  (let ((n (avl-find-node (avl-tree-head tree) key (avl-tree-compare-fn tree))))
    (when n (avl-node-value n))))

(defun new-height (node)
  (let ((left (avl-node-left node))
	(right (avl-node-right node)))
    (1+ (max (if left (avl-node-height left) 0)
	     (if right (avl-node-height right) 0)))))

(defun balance (node)
  (if node
      (with-accessors ((left avl-node-left) (right avl-node-right)) node
	(- (if right (avl-node-height right) 0)
	   (if left (avl-node-height left) 0)))
      0))

(defun rotate-left (node)
  (let ((right (avl-node-right node)))
    (setf (avl-node-right node) (avl-node-left right)
	  (avl-node-left right) node
	  (avl-node-height node) (new-height node)
	  (avl-node-height right) (new-height right))
    right))

(defun rotate-right (node)
  (let ((left (avl-node-left node)))
    (setf (avl-node-left node) (avl-node-right left)
	  (avl-node-right left) node
	  (avl-node-height node) (new-height node)
	  (avl-node-height left) (new-height left))
    left))

(defun rebalance (node)
  (with-accessors ((left avl-node-left) (right avl-node-right)) node
    (ecase (balance node)
      ((0 1 -1) node)
      (2 (when (< (balance right) 0)
	   (setf right (rotate-right right)))
       (rotate-left node))
      (-2 (when (> (balance left) 0)
	    (setf left (rotate-left left)))
       (rotate-right node)))))

(defun avl-modify-node (null-value zerop-fn node key value compare-fn)
  (if (null node)
      null-value
      (let ((cmp (funcall compare-fn key (avl-node-key node))))
	(with-accessors ((left avl-node-left) (right avl-node-right)) node
	  (cond
	    ((zerop cmp)
	     (setf node (funcall zerop-fn node value left right compare-fn)))
	    ((< cmp 0)
	     (setf left (avl-modify-node
			 null-value zerop-fn left key value compare-fn)))
	    (t (setf right (avl-modify-node
			    null-value zerop-fn right key value compare-fn))))
	  (unless (zerop cmp)
	    (setf node (rebalance node)
		  (avl-node-height node) (new-height node))))
	node)))

(defun avl-insert-node (node key value compare-fn)
  (avl-modify-node
   (make-avl-node key value)
   (lambda (node value left right compare-fn)
     (declare (ignore left right compare-fn))
     (setf (avl-node-value node) value))
   node key value compare-fn))

(defun avl-insert (tree key value)
  (setf
   (avl-tree-head tree)
   (avl-insert-node (avl-tree-head tree) key value (avl-tree-compare-fn tree))))

(defun leaf-p (node)
  (not (or (avl-node-left node) (avl-node-right node))))

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun half-leaf-p (node)
  (xor (avl-node-left node) (avl-node-right node)))

(defun find-replacement (node compare-fn)
  (labels ((find-it (curr parent)
	     (cond ((null (avl-node-left curr))
		    (avl-remove-node node (avl-node-key curr) compare-fn)
		    (if (eq parent node)
			(setf (avl-node-right parent) nil)
			(setf (avl-node-left parent) nil))
		    curr)
		   (t (find-it (avl-node-left curr) curr)))))
    (find-it (avl-node-right node) node)))

(defun remove-node (node compare-fn &optional left right)
  (cond ((leaf-p node) nil)
	((half-leaf-p node) (or left right))
	(t (let ((rep (find-replacement node compare-fn)))
	     (setf (avl-node-left rep) left
		   (avl-node-right rep) (unless (eq rep right) right)
		   (avl-node-height rep) (new-height rep))
	     rep))))

(defun avl-remove-node (node key compare-fn)
  (avl-modify-node
   nil
   (lambda (node value left right compare-fn)
     (declare (ignore value))
     (remove-node node compare-fn left right))
   node key nil compare-fn))

(defun avl-remove (tree key)
  (setf (avl-tree-head tree)
	(avl-remove-node (avl-tree-head tree) key (avl-tree-compare-fn tree))))
