(defpackage :rosario-raulin-de-avl-test
  (:use
   :cl
   :rosario-raulin-de-avl)
  (:export
   :avl-test-insert
   :avl-test-remove))

(in-package :rosario-raulin-de-avl-test)

(defun fill-table-and-tree (size)
  (let ((ht (make-hash-table :size (* 2 size)))
	(tree (make-avl-tree #'-)))
    (dotimes (i size (values ht tree))
      (let ((r (random 10000)))
	(setf (gethash i ht) r)
	(avl-insert tree i r)))))

(defun content-alike (ht tree)
  (maphash (lambda (k v) (assert (= (avl-find tree k) v))) ht))

(defun avl-test-insert (size)
  (multiple-value-bind (ht tree) (fill-table-and-tree size)
    (content-alike ht tree)))

(defun avl-test-remove (size)
  (multiple-value-bind (ht tree) (fill-table-and-tree size)
    (let ((deleted (list)))
      (dotimes (i (truncate size 8))
	(push i deleted)
	(remhash i ht)
	(avl-remove tree i))
      (content-alike ht tree)
      (mapc (lambda (key) (assert (not (avl-find tree key)))) deleted))))
