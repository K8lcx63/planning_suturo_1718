(in-package :planning-nod-head)
(defvar *actionclient*)
(defvar *actionserver*)
(defvar *actiongoal*)
(defvar *trans-stamped*)
(defvar *xtrans*)
;the variables are static right now but we will change it.
(defvar *x* 1.0)
(defvar *y* 0.0)
(defvar *z* 0.5)
(defvar  *vec3d*)
(defvar *temp*)
(defvar *poke-point*)
(defvar *message-to-ask*)
(defvar *tf-point-stamped*)
(defvar *transform-listener*)
(defvar *motion-p*)

(defun main ()
  "our main function, we starting a ros node in the main function"
  (init-Vari)
  (roslisp:with-ros-node ("planning-test")
			 (move-Head)
			 (call-Motion-Move-Arm)
			 (call-Vision-Point)
			 (ask-knowledge)
			 (transform-Kinect-To-Robot)
			 (call-Motion-Move-To-Point)))


(defun init-Vari ()
  "we are initializing the variables to nil"
  (setf *actionclient* nil)
  (setf *actionserver* nil)
  (setf *actiongoal* nil)
  (setf *trans-stamped* nil)
  (setf *xtrans* nil)
  (setf *x* 1.0)
  (setf *y* 0.0)
  (setf *z* 0.5)
  (setf *vec3d* nil)
  (setf *temp* nil)
  (setf *poke-point* nil)
  (setf *message-to-ask* nil)
  (setf *tf-point-stamped* nil)
  (setf *transform-listener* nil)
  (setf *motion-p* nil))

(defun move-Head ()
  "with the moveHead function we can move the head without any other groups, we are using the frame head_traj_controller/point_head_action"
  (setf *actionclient* 
	(actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction"))
  (loop until
	(actionlib:wait-for-server *actionclient*))
  (setf *trans-stamped* 
	(cl-transforms-stamped:to-msg 
	 (cl-transforms-stamped:make-point-stamped "base_link" 0 
						   (cl-transforms:make-3d-vector *x* *y* *z*))))
  (setf *actiongoal* 
	(actionlib:make-action-goal *actionclient* target *trans-stamped*))
  (actionlib:call-goal *actionclient* *actiongoal*))



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



(defun call-Vision-Point ()
  "here we are asking the vision service for a point"
  (setf *poke-point* 
	(roslisp:call-service "/VisObjectInfo" 'object_detection-srv:VisObjectInfo)))
  
(defun ask-knowledge()	
  (roslisp:with-fields (object) *poke-point*
		       (setf *poke-point* 
			     (roslisp:call-service "/poke_service_node/calculate_poke_position" 'object_detection-srv:PokeObject
						   :detection object))))


(defun transform-Kinect-To-Robot ()
  "we are transforming the kinect frame to a odom-combined frame; vision is using kinect and motion odom-combined."
  (roslisp:with-fields (poke_position) *poke-point* 
		       (setf *temp* poke_position)) 
  (setf *vec3d* 
	(roslisp:with-fields(x) *temp* 
			    (roslisp:with-fields(y) *temp* 
						(roslisp:with-fields(z) *temp*
								    (cl-transforms:make-3d-vector x y z)))))
  (defparameter *tf-point-stamped* 
    (cl-tf:make-point-stamped "/head_mount_kinect_ir_optical_frame" 0.0 *vec3d*))
  (loop until
	(defparameter *transform-listener* 
	  (make-instance 'cl-tf:transform-listener)))
  (setf *poke-point* 
	(cl-tf:transform-point *transform-listener* :point *tf-point-stamped* :target-frame "odom_combined")))




(defun call-Motion-Move-To-Point ()
  "with this function we are sending the transformed point to the motion service and we are setting up the goal. command 3 is for the left arm, command 2 would be right arm and 1 is default posi"
  (setf *actionclient* 
	(actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction"))
  (loop until
	(actionlib:wait-for-server *actionclient*))

  
  (setf *motion-p* 
	(roslisp:with-fields (poke_position) *poke-point* 
			     (print poke_position)))
  (setf *actiongoal* 
	(actionlib:make-action-goal *actionclient* point *motion-p* command 3))
  (actionlib:call-goal *actionclient* *actiongoal*))





