(defpackage :rosario-raulin-de-avl
  (:use :cl)
  (:export
   :make-avl-tree
   :avl-find
   :avl-insert))

(in-package :rosario-raulin-de-avl)

(defstruct (avl-node (:constructor make-avl-node
			 (key value &key left right (height 1))))
  key value left right height)

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

(defun avl-insert-node (node key value compare-fn)
  (if (null node)
      (make-avl-node key value)
      (let ((cmp (funcall compare-fn key (avl-node-key node))))
	(with-accessors ((left avl-node-left) (right avl-node-right)) node
	  (cond ((zerop cmp) (setf (avl-node-value node) value))
		((< cmp 0)
		 (setf left (avl-insert-node left key value compare-fn))
		 (setf node (rebalance node))
		 (setf (avl-node-height node) (new-height node)))
		((> cmp 0)
		 (setf right (avl-insert-node right key value compare-fn))
		 (setf node (rebalance node))
		 (setf (avl-node-height node) (new-height node)))))
	node)))

(defun avl-insert (tree key value)
  (setf
   (avl-tree-head tree)
   (avl-insert-node (avl-tree-head tree) key value (avl-tree-compare-fn tree))))
