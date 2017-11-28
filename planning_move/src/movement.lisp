(in-package :planning-move)
(defvar *actionclient*)
(defvar *trans-stamped*)
(defvar *actiongoal*)

(defun move-Head (x y z)
  "with the moveHead function we can move the head without any other groups, we are using the frame head_traj_controller/point_head_action"
  (setf *actionclient* 
	(actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction"))
  (loop until
	(actionlib:wait-for-server *actionclient*))
  (setf *trans-stamped* 
	(cl-transforms-stamped:to-msg 
	 (cl-transforms-stamped:make-point-stamped "base_link" 0 
						   (cl-transforms:make-3d-vector x y z))))
  (setf *actiongoal* 
	(actionlib:make-action-goal *actionclient* target *trans-stamped*))
  (actionlib:call-goal *actionclient* *actiongoal*))

