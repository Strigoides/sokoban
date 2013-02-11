(defpackage sokoban
  (:use :common-lisp))

(in-package :sokoban)

(defun level-from-string (string)
  (declare (optimize (debug 3)))
  (let ((level '(())))
    (loop for char across string
          if (char= char #\Newline)
            do (push () level)
          else
            do (push (ecase char
                       (#\# 'wall)
                       (#\. 'storage)
                       (#\Space 'empty)
                       (#\@ 'man)
                       (#\o 'crate))
                     (car level)))
    (reverse (mapcar #'reverse level))))
