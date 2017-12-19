(in-package :planning-move)

(defvar *VisionsMethodeSoon* T)
(defvar *test*)
(defvar *t* 1)


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


(defun find-Object (x z)
  "Looking around from 0-2 and 0-(-2) to find an object"
(let ((positions (make-array '(8) 
   :initial-contents '(0.0 0.5 1 1.5 2.0 -0.5 -1.5 -2.0))))
 (loop for i across positions do
   (progn
     (let ((y i))
       (move-Head x y z))
     (if (planning-move::askFor)
       (progn
         (roslisp::ros-info "Main" "I found an object, lets move on ")
         (return-from find-Object T))
       (roslisp::ros-info "Main" "I can't find any object, let me try it again")))))
(return-from find-Object nil))

(defun askFor ()
  "asking vision for the ice"
  (setf *test* (roslisp:call-service "/vision_main/objectPose" 'vision_msgs-srv:GetObjectInfo))
  (roslisp:with-fields (info) *test* (setf *test* info))
  (roslisp:with-fields (isstanding) *test* (setf *test* isstanding))
  (if (= *test* 2)
      (return-from askFor T)
      (return-from askFor nil)))
  
