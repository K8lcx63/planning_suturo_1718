(in-package :planning-move)

(defun move-Head (x y z)
  "Moving robot head via head_traj_controller/point_head_action. X Y Z are treated as coordinates."
  (let actionclient 
	(actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction")
  (loop until
	(actionlib:wait-for-server actionclient))
  (let point-to-look-at 
	(cl-transforms-stamped:to-msg 
	 (cl-transforms-stamped:make-point-stamped "base_link" 0 
						   (cl-transforms:make-3d-vector x y z)))
  (let actiongoal 
	(actionlib:make-action-goal actionclient target point-to-look-at)
  (actionlib:call-goal actionclient actiongoal)))))

