(in-package :planning-motion)
(defvar *actionclient*)
(defvar *xtrans*)
(defvar *actiongoal*)
(defvar *motion-p*)

(defun call-Motion-Move-Arm ()
  "with this function we jus send a 3d-vector with the base_link frame to the actionsserver from motion"
  (setf *actionclient* 
	(actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction"))
  (loop until
	(actionlib:wait-for-server *actionclient*))
  (setf *xtrans* 
	(cl-tf:make-point-msg 
	 (cl-tf:make-point-stamped "base_link" 0 
				   (cl-transforms:make-3d-vector 5.0 3.0 1.2))))
  (setf *actiongoal* 
	(actionlib:make-action-goal *actionclient* point *xtrans* command 1))
  (actionlib:call-goal *actionclient* *actiongoal*))

		


(defun call-Motion-Move-To-Point (poke-point)
  "with this function we are sending the transformed point to the motion service and we are setting up the goal. command 3 is for the left arm, command 2 would be right arm and 1 is default posi"
  (setf *actionclient* 
	(actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction"))
  (loop until
	(actionlib:wait-for-server *actionclient*))

  
  (setf *motion-p* 
	(roslisp:with-fields (poke_position) poke-point
			     (print poke_position)))
  (setf *actiongoal* 
	(actionlib:make-action-goal *actionclient* point *motion-p* command 3))
  (actionlib:call-goal *actionclient* *actiongoal*))

