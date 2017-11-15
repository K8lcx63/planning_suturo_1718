(in-package :planning-nod-head)
(defvar *actionclient*)
(defvar *actionserver*)
(defvar *actiongoal*)
(defvar *trans-stamped*)
(defvar *xtrans*)
(defvar *x* 1.0)
(defvar *y* 2.0)
(defvar *z* -1.2)


(defun main ()
  (roslisp:with-ros-node ("planning-test")
    ;KOPF BEWEGEN
    
    (setf *actionclient* (actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction"))
    (loop until
          (actionlib:wait-for-server *actionclient*))
    (setf *trans-stamped* (cl-transforms-stamped:to-msg (cl-transforms-stamped:make-point-stamped "base_link" 0 (cl-transforms:make-3d-vector *x* *y* *z*))))
    (setf *actiongoal* (actionlib:make-action-goal *actionclient* target *trans-stamped*))
    (actionlib:call-goal *actionclient* *actiongoal*)
    
    ;HÄNDE BEWEGEN
    
    (setf *actionclient* (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction"))
    (loop until
          (actionlib:wait-for-server *actionclient*))
    (setf *xtrans* (cl-transforms-stamped:to-msg (cl-transforms:make-3d-vector 5.0 3.0 1.2)))
    (setf *actiongoal* (actionlib:make-action-goal *actionclient* vector *xtrans* command 1))
    (actionlib:call-goal *actionclient* *actiongoal*)))

