(defpackage :rosario-raulin-de-avl-test
  (:use
   :cl
   :rosario-raulin-de-avl)
  (:export
   :avl-test
   :*comparisions*))

(in-package :rosario-raulin-de-avl-test)

(defparameter *comparisions* 0)

(defun string-compare (s1 s2)
  (incf *comparisions*)
  (cond ((string= s1 s2) 0)
	((string< s1 s2) -1)
	(t 1)))

(defun avl-test ()
  (let ((tree (make-avl-tree #'string-compare)))
    (with-open-file (s "faust.txt")
      (loop for w = (read-line s nil) while w
	    do (avl-insert tree w (random 100))))
    (setf *comparisions* 0)
    (avl-find tree "Kraft")))
