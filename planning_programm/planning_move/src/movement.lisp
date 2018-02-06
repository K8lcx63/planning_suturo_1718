(in-package :planning-move)


(defvar *joint-states* 0)
(defvar *beliefstateHead* 0)
(defvar *action-client-base* nil)
(defvar *headMovementList* '((0 -0.8)(1 -0.6)(2 -0.3)(3 0.0)(4 0.3)(5 0.6)(6 0.8)(7 0.6)(8 0.3)(9 0.0)(10 -0.3)(11 -0.6)))

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



(defun find-Object (x z objectString)
  "Looking around from 0-2 and 0-(-2) to find an object, restarting at current Point if reused"
  (block find-Object-Start 
    (loop for i from *beliefstateHead* to 24 do
      (let ((c (mod i 12)))
        (progn
          (move-Head x (second (assoc c *headMovementList*)) z)
          (setf *beliefstateHead* c)
          (if (planning-knowledge::is-Object i)
              (return-from find-Object-Start)
              ))))
    (roslisp::ros-info "find-Object" "Couldnt find any Object in front of me")
    (return-from find-Object nil))
  (roslisp::ros-info "find-Object" "I see the Object. Head is in Position")
  (return-from find-Object T))


  
(defun move-Base-To-Point (x y z angle)
  "Moving robot base via nav_pcontroller/move_base. X Y Z are treated as coordinates. angle for Orientation."
  (roslisp:ros-info (move-Base-To-Point)
                    "before im moving ill make sure that my arms arent in the way!") 
  (planning-motion::call-Motion-Move-Arm-Homeposition)
  (get-action-client-base)
  (let ((pose-to-drive-to 
          (cl-transforms-stamped:to-msg 
           (cl-transforms-stamped:make-pose-stamped "map" 10
            (cl-transforms:make-3d-vector x y z)
            (cl-transforms:axis-angle->quaternion
             (cl-transforms:make-3d-vector 0 0 1)(/ (* angle pi) 180))))))
    (roslisp:ros-info (move-Base-To-Point)
                      "created quaternion, vector and pose-stamped") 
    (let ((actiongoal 
            (actionlib:make-action-goal *action-client-base* target_pose pose-to-drive-to)))
      (roslisp:ros-info (move-Base-To-Point)
                        "Sending actiongoal...") 
      (actionlib:call-goal *action-client-base* actiongoal))))

  
(defun move-Robo-Into-Homeposition ()
   "moving robot into homeposition with frame map"
(move-Base-To-Point 0.8 1 0 30))




(defun init-Robo-Moving ()
  "initialize all what's needed for the Robo to "
  (planning-motion::call-Motion-Move-Arm-Homeposition)
  (get-Action-Client-Base)
  (roslisp:ros-info (init-Robo-Moving)
                    "robo is starting to move...")
  (let ((positions-x (make-array '(4)  
                                 :initial-contents '(0.0 0.1 0.2 0.0)))) 
    (loop for i across positions-x do
        (move-Base-To-Point i 0 0 0)))
  (roslisp:ros-info (init-Robo-Moving)
                    "robo is ready..."))

   

(defun init-Action-Client-Base ()
  "creating move_base actionclient"
  (setf *action-client-base* 
        (actionlib:make-action-client
         "nav_pcontroller/move_base"
         "move_base_msgs/MoveBaseAction"))
  (roslisp:ros-info (init-Action-Client-Base)
                    "Waiting for move_base action server...")
  (loop until
        (actionlib:wait-for-server *action-client-base*))
  (roslisp:ros-info (init-Action-Client-Base) 
                    "move_base action client created."))


(defun get-Action-Client-Base ()
  (when (null *action-client-base*)
    (init-Action-Client-Base))
  *action-client-base*)



(defun save-joint-states (msg)
"Callback to save one sensor-msgs/JointState"
  (setf *joint-states* msg))

(defun get-joint-states ()
"gets exactly one sensor_msgs/JointState message"
  (progn
    (let
        ((subsc
           (roslisp:subscribe "/joint_states" "sensor_msgs/JointState" #'save-joint-states :max-queue-length 1)))
      (progn
        (sleep 1)
        (roslisp:unsubscribe subsc)))
    (return-from get-joint-states *joint-states*)))

(defun is-gripper-filled (side)
  "Checks if Gripper is filled after gripping something. Side is either left or right as string"
  (if
   (string= side "left")
   (progn
      (get-joint-states)
      (roslisp:with-fields
          ((Name (sensor_msgs-msg:Name))
           (Position (sensor_msgs-msg:Position))) planning-move::*joint-states* 
        (loop for a across Name 
              for b across Position do
                (if (string= a "l_gripper_joint")
                    (if (and
                         (>= b 0.004)
                         (<= b 0.08))(return-from is-gripper-filled T)(print b))))))
   (progn
      (get-joint-states)
      (roslisp:with-fields
          ((Name (sensor_msgs-msg:Name))
           (Position (sensor_msgs-msg:Position))) planning-move::*joint-states* 
        (loop for a across Name 
              for b across Position do
                (if (string= a "r_gripper_joint")
                    (if (and
                         (>= b 0.004)
                         (<= b 0.08))(return-from is-gripper-filled T)(print b))))))))

