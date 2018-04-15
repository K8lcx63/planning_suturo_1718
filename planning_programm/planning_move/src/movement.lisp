(in-package :planning-move)


(defvar *action-client-base* nil)


(defun move-Head (x y z)
  "Moving robot head via head_traj_controller/point_head_action. X Y Z are treated as coordinates."
  (let ((actionclient 
          (actionlib:make-action-client "head_traj_controller/point_head_action"
                                        "pr2_controllers_msgs/PointHeadAction")))
    (loop until
          (actionlib:wait-for-server actionclient))
    (let ((point-to-look-at 
            (cl-transforms-stamped:to-msg 
             (cl-transforms-stamped:make-point-stamped "base_link" 0 
                                                       (cl-transforms:make-3d-vector x y z)))))
      (let ((actiongoal 
              (actionlib:make-action-goal actionclient target point-to-look-at)))
        (actionlib:call-goal actionclient actiongoal)))))


(defun move-Torso (x)
  (let ((actionclient 
          (actionlib:make-action-client "torso_controller/position_joint_action" "pr2_controllers_msgs/SingleJointPositionAction")))
    (Loop until
          (actionlib:wait-for-server actionclient))
    (let ((actiongoal 
            (actionlib:make-action-goal actionclient
              :position x
              :min_duration 2.0
              :max_velocity 1.0)))
      (actionlib:call-goal actionclient actiongoal))))





(Defun move-Base-To-Point-Safe (x y z angle &optional (motion 1))
  (cram-language:wait-for
   (move-Base-To-Point 0.15 0.5 0 -90 motion))
  (if
   (and
    (> angle 90)
    (< angle 270))
   (move-Base-To-Point -0.29 1 0 180 motion)
   (move-Base-To-Point 0.75 0.8 0 0 motion)))

(defun move-Base (x y z angle &optional (motion 1))
   (roslisp:with-fields
      ((pr2-x
        (geometry_msgs-msg:x
         geometry_msgs-msg:position
         geometry_msgs-msg:pose
         geometry_msgs-msg:pose)))
      (cram-language:value planning-logic::*pr2-pose*)
    (if (and (< pr2-x 0) (> x 0))
        (move-base-to-point 0.15 0.5 0 -90)
        (if (and (> pr2-x 0) (< x 0))
             (move-base-to-point 0.15 0.5 0 -90)))))


(defmethod move-Base-To-Point (x y z angle &optional (motion 1))
  "Moving robot base via nav_pcontroller/move_base. X Y Z are treated as coordinates. angle for Orientation."
  (planning-motion::call-Motion-Move-Arm-Homeposition motion)
  (get-action-client-base)
  (let ((pose-to-drive-to 
          (cl-transforms-stamped:to-msg 
           (cl-transforms-stamped:make-pose-stamped "map" 10
                                                    (cl-transforms:make-3d-vector x y z)
                                                    (quaternion x y z angle)))))
    (let ((actiongoal 
            (actionlib:make-action-goal *action-client-base* target_pose pose-to-drive-to)))
      (actionlib:call-goal *action-client-base* actiongoal))))



(defmethod move-Base-To-Point :before (x y z angle &optional (motion 1))
  (roslisp:ros-info (move-Base-To-Point)
                    "robo is starting to move..."))
        
        

(defmethod move-Base-To-Point :after (x y z angle &optional (motion 1))
  (roslisp:ros-info (move-Base-To-Point)
                    "robo is done moving."))


(defgeneric quaternion (x y z angle))

(defmethod quaternion ((x Number) (y Number) (z Number) (angle Number)) 
  (cl-transforms:axis-angle->quaternion
   (cl-transforms:make-3d-vector 0 0 1)
   (/ (* angle pi) 180)))





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




       
