(defpackage sokoban
  (:use :common-lisp))

(in-package :sokoban)

(defstruct level
  (walls   '() :type list)
  (crates  '() :type list)
  (storage '() :type list)
  (man (vector 0 0) :type (simple-vector 2))
  (max-x 0 :type integer)
  (max-y 0 :type integer))

(defvar *test-level-string*
  "#####
#.o@#
#####")

(defun thing-at (vector level)
  (cond
    ((wallp vector level)
     'wall)
    ((cratep vector level)
     'crate)
    (t 'empty)))

(flet ((member-equalp (thing things)
         (member thing things :test #'equalp)))
  (defun wallp (vector level)
    (member-equalp vector (level-walls level)))
  (defun cratep (vector level)
    (member-equalp vector (level-crates level)))
  (defun storagep (vector level)
    (member-equalp vector (level-storage level))))
(defun manp (vector level)
  (equalp (level-man level) vector))

(defun level-from-string (string)
  (declare (optimize (debug 3)))
  (let ((level (make-level)))
    (loop for char across string
          for x from 0 maximizing x into max-x
          with y = 0 maximizing y into max-y
          when (char= char #\Newline)
            do (incf y)
            and do (setf x -1)
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
                 (#\Space)))
          finally (setf (level-max-x level) max-x
                        (level-max-y level) max-y))
    level))

(defun print-level (level)
  (dotimes (y (1+ (level-max-y level)))
    (dotimes (x (1+ (level-max-x level)))
      (let ((here (vector x y)))
        (princ (cond
                 ((wallp here level)
                  #\#)
                 ((cratep here level)
                  (if (storagep here level)
                    #\*
                    #\o))
                 ((storagep here level)
                  (if (manp here level)
                    #\+
                    #\.))
                 ((manp here level)
                  #\@)
                 (t #\Space))))) 
    (princ #\Newline))) 

(defun move-from-char (char level)
  (case char
    (#\w (move level #(0 -1)))
    (#\a (move level #(-1 0)))
    (#\s (move level #(0 1)))
    (#\d (move level #(1 0)))))

(defun move (level delta)
  (let ((man (level-man level)))
    (case (thing-at (vector-+ man delta) level)
      (empty
        (progn (setf (level-man level)
                     (vector-+ man delta))
               level))
      (wall level)
      (crate
        (let ((next-square (vector-+ man (vector-+ delta
                                                   delta))))
          (case (thing-at next-square level)
            (empty
              (progn
                (setf (level-man level)
                      (vector-+ man delta))
                (remove (vector-+ man delta) 
                        (level-crates level) 
                        :test #'equalp) 
                (push next-square (level-crates level))
                level))
            (otherwise level)))))))

(defun vector-+ (v1 v2)
  (map 'vector #'+ v1 v2))
