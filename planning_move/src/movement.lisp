(in-package :planning-move)

(defvar *VisionsMethodeSoon* T)
(defvar *beliefstateHead* 0)

(defvar *headMovementList* '((0 -1.5)(1 -1.0)(2 -0.5)(3 0.0)(4 0.5)(5 1.0)(6 1.5)(7 1.0)(8 0.5)(9 0.0)(10 -0.5)(11 -1.0)))


(defun move-Head (x y z)
  "Moving robot head via head_traj_controller/point_head_action. X Y Z are treated as coordinates."
  (let ((actionclient 
	(actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction")))
  (loop until
	(actionlib:wait-for-server actionclient))
  (let ((point-to-look-at 
	(cl-transforms-stamped:to-msg 
	 (cl-transforms-stamped:make-point-stamped "base_link" 0 
						   (cl-transforms:make-3d-vector x y z)))))
  (let ((actiongoal 
	(actionlib:make-action-goal actionclient target point-to-look-at)))
  (actionlib:call-goal actionclient actiongoal)))))

(defun move-Base-To-Point (x y z w)
  "Moving robot base via nav_pcontroller/move_base. X Y Z are treated as coordinates. W for Orientation."
  (let ((actionclient 
	(actionlib:make-action-client "nav_pcontroller/move_base" "move_base_msgs/MoveBaseAction")))
  (loop until
	(actionlib:wait-for-server actionclient))
  (let ((pose-to-drive-to 
	(cl-transforms-stamped:to-msg 
	 (cl-transforms-stamped:make-pose-stamped "base_link" 0 
                                            (cl-transforms:make-3d-vector x y z) (cl-tf:make-quaternion x y z w)))))
  (let ((actiongoal 
	(actionlib:make-action-goal actionclient target_pose pose-to-drive-to)))
  (actionlib:call-goal actionclient actiongoal)))))


(defun find-Object (x z objectString)
  "Looking around from 0-2 and 0-(-2) to find an object, restarting at current Point if reused"
  (block find-Object-Start 
    (loop for i from *beliefstateHead* to 24 do
      (let ((c (mod i 12)))
        (progn
          (move-head x (second (assoc c *headMovementList*)) z)
          (setf *beliefstateHead* c)
          (if (planning-knowledge::is-Object i)
              (return-from find-Object-Start)
              ))))
    (roslisp::ros-info "find-Object" "Couldnt find any Object in front of me")
    (return-from find-Object nil))
  (roslisp::ros-info "find-Object" "I see the Object. Head is in Position")
  (return-from find-Object T))
