(defpackage sokoban
  (:use :common-lisp))

(in-package :sokoban)

(defstruct level
  (walls   '() :type list)
  (crates  '() :type list)
  (storage '() :type list)
  (man (vector 0 0) :type (simple-vector 2)))

(defun level-from-string (string)
  (declare (optimize (debug 3)))
  (let ((level (make-level)))
    (loop for char across string
          for x from 0
          with y = 0
          when (char= char #\Newline)
            do (incf y)
            and do (setf x 0)
          else
          do (let ((here (vector x y)))
               (ecase char
                 (#\# (push here (level-walls level)))
                 (#\. (push here (level-storage level)))
                 (#\o (push here (level-crates level)))  
                 (#\* (push here (level-crates level))
                  (push here (level-storage level)))  
                 (#\+ (push here (level-storage level))
                  (setf (level-man level) here))
                 (#\@ (setf (level-man level) here))
                 (#\Space))))
    level))

(defun move-from-char (char level)
  (case char
    (#\w (move level #(0 -1)))
    (#\a (move level #(-1 0)))
    (#\s (move level #(0 1)))
    (#\d (move level #(1 0)))))

(defun move (level delta)
  
  )
