(in-package :planning-move)


(defvar *action-client-base* nil)



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


(defun move-Torso (x)
  "Moving robot torso via torso_controller/position_joint_action. 2 is for up and 0 for down. You'll need to interrupt by yourself in the simulation"
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

                                        ;Errorhandling is missing -V
(defmethod move-Base-To-Point (x y z angle  &optional (motion 1))
  "Moving robot base via nav_pcontroller/move_base. X Y Z are treated as coordinates. Angle for Orientation."
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



(defmethod move-Base-To-Point :before (x y z angle  &optional (motion 1))
  "before method for move-Base-To-Point"
  (roslisp:ros-info (move-Base-To-Point)
                    "robo is starting to move..."))



(defmethod move-Base-To-Point :after (x y z angle &optional motion)
  "after method for move-Base-To-Point"
  (roslisp:ros-info (move-Base-To-Point)
                    "robo is done moving."))


(defgeneric quaternion (x y z angle))

(defmethod quaternion ((x Number) (y Number) (z Number) (angle integer))
  "method for the orientation only income are Numbers and for angle integer"
  (cl-transforms:axis-angle->quaternion
   (cl-transforms:make-3d-vector 0 0 1)
   (/ (* angle pi) 180)))



(defmethod init-Action-Client-Base ()
  "creating move_base actionclient"
  (setf *action-client-base* 
        (actionlib:make-action-client
         "nav_pcontroller/move_base"
         "move_base_msgs/MoveBaseAction"))
  (loop until
        (actionlib:wait-for-server *action-client-base*)))
  
(defmethod init-Action-Client-Base :before ()
  "before method"
  (roslisp:ros-info (init-Action-Client-Base)
                    "Waiting for move_base action server..."))

(defmethod init-Action-Client-Base :after ()
  (roslisp:ros-info (init-Action-Client-Base) 
                    "move_base action client created."))

    

(defun get-Action-Client-Base ()
  (when (null *action-client-base*)
    (init-Action-Client-Base))
  *action-client-base*)





