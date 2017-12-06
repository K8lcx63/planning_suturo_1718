(in-package :planning-main-programm)

(defvar *x* 1.0)
(defvar *y* 0.0)
(defvar *z* 0.5)



(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (init-variables)
  (roslisp:with-ros-node ("planning_main")
    (print "move-Head")
    (planning-move::move-Head *x* *y* *z*)
    (print "call-Motion-Move-Arm")
    (planning-motion::call-Motion-Move-Arm)
    (print "call-vision-point --> ask Knowledge --> motion-move-to-point")
    (let ((point-for-motion
            (let ((point-center-of-object
                    (planning-vision::call-vision-point)))
                 (planning-knowledge::ask-knowledge point-center-of-object))))
         (planning-motion::call-Motion-Move-To-Point point-for-motion))))


(defun init-variables ()
  "initialize internal variables"
  (setf *x* 1.0)
  (setf *y* 0.0)
  (setf *z* 0.5))



