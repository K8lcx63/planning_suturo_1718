(in-package :planning-nod-head)
(defvar *actionclient*)
(defvar *actionserver*)
(defvar *actiongoal*)
(defvar *trans-stamped*)
(defvar *xtrans*)
(defvar *x* 1.0)
(defvar *y* 0.0)
(defvar *z* 0.5)
(defvar  *vec3d*)
(defvar *test1*)
(defvar *poke-point*)
(defvar *message-to-ask*)
(defvar *tf-point-stamped*)
(defvar *transform-listener*)
(defvar *motion-p*)






(defun main (x y z)
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
    (setf *xtrans* (cl-tf:make-point-msg (cl-tf:make-point-stamped "base_link" 0 (cl-transforms:make-3d-vector 5.0 3.0 1.2))))
    (setf *actiongoal* (actionlib:make-action-goal *actionclient* point *xtrans* command 1))
    (actionlib:call-goal *actionclient* *actiongoal*)

                                        ;EIS-TEE
    (setf *poke-point* (roslisp:call-service "/VisObjectInfo" 'object_detection-srv:VisObjectInfo))

    (roslisp:with-fields ( object) *poke-point* (setf *poke-point* (roslisp:call-service "/poke_service_node/calculate_poke_position" 'object_detection-srv:PokeObject :detection object)))

                                        ;EIS TEE VISION ZU EIS TEE MOTION UMRECHNEN

    ;;  (roslisp:with-fields (poke_position) *poke-point* (setf *test1* poke_position)) 
    ;; (setf *vec3d* (roslisp:with-fields(x) *test1* (roslisp:with-fields(y) *test1* (roslisp:with-fields(z) *test1*(cl-transforms:make-3d-vector x y z)))))
    ;;  (defparameter *tf-point-stamped* (cl-tf:make-point-stamped "/head_mount_kinect_ir_optical_frame" 0.0 *vec3d*))
    ;; (loop until
    ;;   (defparameter *transform-listener* (make-instance 'cl-tf:transform-listener)))
    ;; (setf *poke-point* (cl-tf:transform-point
    ;;                     *transform-listener* :point *tf-point-stamped* :target-frame "/odom_combined"))
    
                                        ;HAENDE ZUM EISTEE
    (setf *actionclient* (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction"))
    (loop until
          (actionlib:wait-for-server *actionclient*))
    
    (setf *motion-p* (roslisp:with-fields (poke_position) *poke-point* (print poke_position)))
    (setf *actiongoal* (actionlib:make-action-goal *actionclient* point *motion-p* command 3))
    (actionlib:call-goal *actionclient* *actiongoal*)))

                                        ;3dve  (roslisp:with-fields(x) *test1* (roslisp:with-fields(y) *test1* (roslisp:with-fields(z) *test1*(cl-transforms:make-3d-vector x y z)))) 
