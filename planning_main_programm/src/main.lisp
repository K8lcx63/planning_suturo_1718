(in-package :planning-main-programm)

(defvar *trans-stamped*)
(defvar *xtrans*)
;the variables are static right now but we will change it.
(defvar *x* 1.0)
(defvar *y* 0.0)
(defvar *z* 0.5)
(defvar  *vec3d*)
(defvar *temp*)
(defvar *poke-point*)
(defvar *tf-point-stamped*)
(defvar *transform-listener*)


(defun main ()
  "our main function, we starting a ros node in the main function"
  (init-Vari)
  (roslisp:with-ros-node ("planning-test")
			 (planning-move::move-Head *x* *y* *z*)
			 (planning-motion::call-Motion-Move-Arm)
			 (setf *poke-point* (planning-vision::call-vision-point))
			 (setf *poke-point* (planning-knowledge::ask-knowledge *poke-point*))
			 ;(transform-Kinect-To-Robot)
			 (planning-motion::call-Motion-Move-To-Point *poke-point*)))


(defun init-Vari ()
  "we are initializing the variables to nil"
  (setf *xtrans* nil)
  (setf *x* 1.0)
  (setf *y* 0.0)
  (setf *z* 0.5)
  (setf *vec3d* nil)
  (setf *temp* nil)
  (setf *poke-point* nil)
  (setf *tf-point-stamped* nil)
  (setf *transform-listener* nil))



