(in-package :planning-logic)

(defvar *joint-states* 0)
(defvar *pose* nil)
(defvar *pose-pr2* nil)
(defvar *perception-publisher*)

;;Fluents for gripper-filled
(defvar *gripper-right-state-fluent* (cram-language:make-fluent :name :gripper-right-state-fluent))
(defvar *gripper-left-state-fluent* (cram-language:make-fluent :name :gripper-left-state-fluent))


(defun transformation-Vision-Point (pose amount &optional (endFrame "/base_footprint")) 
  "transform a msgs with an optional Frame, default is base_footprint" 
  (roslisp:with-fields 
      ((startFrame 
        (STD_msgs-msg:frame_id geometry_msgs-msg:header)) 
       (x 
        (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose)) 
       (y 
        (geometry_msgs-msg:y geometry_msgs-msg:position  geometry_msgs-msg:pose)) 
       (z 
        (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose))) 
      (aref pose amount) 
    (let 
        ((transform-listener 
           (make-instance 'cl-tf:transform-listener)) 
         (tf-point-stamped 
           (cl-tf:make-point-stamped startFrame 0.0 
                                     (cl-transforms:make-3d-vector x y z)))) 
      (catch-Transformation transform-listener tf-point-stamped endFrame)))) 


(defun catch-Transformation (transform-listener tf-point-stamped endFrame)
  "transform-listener catch transformation (helpfunction)"
  (sleep 5.0)
  (cl-tf:transform-point transform-listener
                         :point tf-point-stamped
                         :target-frame endFrame))

;;muss überarbeitet werden sobald orientation hinzugefügt wurde
;; (defun let-Robo-Try-To-Poke (point-for-motion number-for-arm)
;;   "trying to Poke the object, first both arms will be used after that the robot will try different poses."
;;   (roslisp:ros-info (let-Robo-Try-To-Poke)
;;                     "trying to poke the object now...")
;;   (if (not(eq T (planning-motion::motion-To-Point point-for-motion number-for-arm)))
;;       (progn
;;         (if (/= 2 number-for-arm)
;;             (planning-motion::motion-To-Point point-for-motion 2)
;;             (planning-motion::motion-To-Point point-for-motion 3)))))

;; (defun try-To-Poke-Different-Location(point-for-motion number-for-arm)
;;   "trying to poke the object now again with different location.."
;;    (roslisp:ros-info (try-To-Poke-Different-Location)
;;                     "trying to poke the object now again with different location..")
;;   (let ((position (make-array '(4)  
;;                                  :initial-contents '(1.3 0.5 0 -0.3)))) 
;;     (loop for y across position do
;;       (planning-move::move-Base-To-Point 0.8 y 0 30)
;;       (let ((rotation (make-array '(3)  
;;                                   :initial-contents '(30 31 29)))) 
;;         (loop for r across rotation do
;;              (progn
;;                (planning-move::move-Base-To-Point 0.8 y 0 r)
;;                (if (eq T(let-Robo-Try-To-Poke point-for-motion number-for-arm))
;;                    (return-from try-To-Poke-Different-Location T))))))))
            


(defun should-Robo-Use-Left-Or-Right-Arm (pose amount &optional (endFrame "/base_footprint"))
  "decides if the left or right arm is chosen depends on which one is closer"
  (let ((pointTransformed(transformation-Vision-Point pose amount endFrame)))
    (roslisp:with-fields (y) pointTransFormed
      (progn
        (if (> y 0)
            (return-from should-Robo-Use-Left-Or-Right-Arm 3)
            (return-from should-Robo-Use-Left-Or-Right-Arm 2))))))

(defun disassemble-Vision-Call (visionclouds)
  "dissamble the whole vision-msg, setting new params onto param server"
  (roslisp:with-fields
      ((normal_features
        (vision_suturo_msgs-msg:normal_features))
       (color_features
        (vision_suturo_msgs-msg:color_features))
       (object_amount
        (vision_suturo_msgs-msg:object_amount))
       (object_poses
        (vision_suturo_msgs-msg:object_poses)))
      (vision_suturo_msgs-srv:clouds visionclouds)
    (roslisp:set-param "normal_features" normal_features)
    (roslisp:ros-info (disassemble-Vision-Call)
                      "param normal_features now exist")
    (roslisp:set-param "color_features" color_features)
    (roslisp:ros-info (disassemble-Vision-Call)
                      "param color_features now exist")    
    (roslisp:set-param "object_amount" object_amount)
    (roslisp:ros-info (disassemble-Vision-Call)
                      "param object_amount now exist")
    (setf *pose* object_poses))
  (let ((n (roslisp:get-param "object_amount")))
    (if (> n 0)
        (loop for amount from 1 to n do
            (if (= amount 1)
                (set-Params-Features 0 0 308 24 amount)
                (set-Params-Features
                 (* (- amount 1) 308)
                 (* (- amount 1) 24)
                 (* amount 308)
                 (* amount 24)amount)))))
  (Return-from disassemble-vision-call *pose*))

 (defun list-to-1d-array (list)
   "convert list to array"
  (make-array (length list)
              :initial-contents list))


(defun disassemble-Pose-Msg-To-Point-Stamped (pose-array amount)
  "making pose_stamped to point_stamped"
  (roslisp:with-fields
      ((header1
        (geometry_msgs-msg:header))
       (point1
        (geometry_msgs-msg:position
         geometry_msgs-msg:pose)))
      (aref pose-array amount)
    (return-from disassemble-Pose-Msg-To-Point-Stamped (roslisp:make-msg "geometry_msgs/pointstamped" (header) header1 (point) point1))))


(defun set-Params-Features (normal-s color-s normal-e color-e amount)
  "soon"
  (roslisp:set-param (concatenate 'string "normal_features"
                                  (write-to-string amount))
                     (subseq (roslisp:get-param "normal_features") normal-s normal-e))
  (roslisp:set-param (concatenate 'string "color_features"
                                  (write-to-string amount))
                     (subseq (roslisp:get-param "color_features") color-s color-e))
  (roslisp:set-param (concatenate 'string "features"
                                  (write-to-string amount))
                     (append
                      (roslisp:get-param (concatenate 'string "color_features"
                                                      (write-to-string amount)))
                      (roslisp:get-param (concatenate 'string "normal_features"
                                                      (write-to-string amount))))))









(defun save-Pr2-Pose (msg)
"Callback to save one geometry_msgs/PoseWithCovarianceStamped"
  (setf *pose-pr2* msg))

(defun get-Pr2-Pose ()
"gets exactly one geometry_msgs/PoseWithCovarianceStamped  message"
  (progn
    (let
        ((subsc
           (roslisp:subscribe "/robot_pose_ekf/odom_combined""geometry_msgs/PoseWithCovarianceStamped" #'save-Pr2-Pose :max-queue-length 1)))
      (progn
        (sleep 1)
        (roslisp:unsubscribe subsc)))
    (return-from get-Pr2-Pose *pose-pr2*)))



(defun transformation-Pr2-Pose (pose &optional (endFrame "/map"))
 "transform a msgs with an optional Frame, default is base_footprint"
 (roslisp:with-fields
     ((startFrame
       (STD_msgs-msg:frame_id geometry_msgs-msg:header))
      (x
       (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose geometry_msgs-msg:pose))
      (y
       (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose  geometry_msgs-msg:pose))
      (w
       (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
      (x2
       (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
      (y2
       (geometry_msgs-msg:y geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
      (z2
       (geometry_msgs-msg:z geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose)))
     pose
    (let
       ((transform-listener
          (make-instance 'cl-tf:transform-listener))
        (tf-point-stamped
          (cl-tf:make-pose-stamped startFrame 10.0
                                   (cl-transforms:make-3d-vector x y 0) (cl-transforms:make-quaternion x2 y2 z2 w))))
        (sleep 5.0) (cl-tf:transform-pose-stamped transform-listener :pose tf-point-stamped :target-frame endFrame))))


;; Methods for gripper filled controll sequence (Touch with caution)

(defun init-gripper-states ()
"subscribes to /joint_states and gives data to according handling method"
   (progn
     (roslisp:subscribe
    "/joint_states"
    "sensor_msgs/JointState"
    #'is-gripper-filled :max-queue-length 1)
     (return-from init-gripper-states())))

(defun is-gripper-filled (msg)
  "Callback for init-gripper-states, accepts sensor_msgs/JointState message, saves gripper state into fluents"
   (progn
     (cram-language:sleep 1)
      (roslisp:with-fields
          ((Name (sensor_msgs-msg:Name))
           (Position (sensor_msgs-msg:Position))) msg 
        (loop for a across Name 
              for b across Position do
                (if (string= a "l_gripper_joint")
                    (if (and
                         (>= b 0.0055)
                         (<= b 0.08))
                        (setf
                         (cram-language:value *gripper-right-state-fluent*) nil)
                        (setf
                         (cram-language:value *gripper-right-state-fluent*) T)))))
      (roslisp:with-fields
          ((Name (sensor_msgs-msg:Name))
           (Position (sensor_msgs-msg:Position))) msg 
        (loop for a across Name 
              for b across Position do
                (if (string= a "r_gripper_joint")
                    (if (and
                         (>= b 0.0055)
                         (<= b 0.08))
                        (setf
                         (cram-language:value *gripper-right-state-fluent*) nil)
                        (setf
                         (cram-language:value *gripper-right-state-fluent*) T)))))))


(defun vis-init ()
  (setf *perception-publisher*
       (roslisp:advertise "/beliefstate/perceive_action" "knowledge_msgs/PerceivedObject")))

;muss noch überarbeitet werden klappt so noch nicht
(defun publish-pose (label object_pose)
  (when *perception-publisher*
    (roslisp:publish *perception-publisher*
                      (roslisp:make-message "geometry_msgs/PoseStamped"
                                            label object_pose))))

(defun test-right-gripper ()
  (cram-language:top-level
    (cram-language:pursue
      (cram-language:wait-for *gripper-right-state-fluent*)
      (cram-language:unwind-protect
           (loop for i from 1 to 1000 do
             (progn
               (print i)
               (cram-language:sleep 1)
               ))
        (progn
          (format t "do shit"))))))
