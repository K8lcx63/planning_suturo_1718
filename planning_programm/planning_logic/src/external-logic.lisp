(in-package :planning-logic)

(defvar *joint-states* 0)
(defvar *pose* nil)

(defvar *perception-publisher*)
(defvar *text-publisher*)
(defvar *model-publisher*)
(defvar *pr2-pose* (cram-language:make-fluent :name :pr2-pose) nil)
(defvar *gripper-righ-state-fluent* (cram-language:make-fluent))
(defvar *gripper-left-state-fluent* (cram-language:make-fluent))

(defun init-logic ()
  (vis-init)
  (init-gripper-states)
  (init-pr2)
  (init-marker)
  (init-model-publisher)
  )

(defun square (x)
  (* x x)
  )

(defun disassemble-graspindividual-response (msg)
  (geometry_msgs-msg:y
   (Geometry_msgs-msg:position
    (geometry_msgs-msg:pose 
     (knowledge_msgs-srv:place_pose msg)))))

(defun transformation-Vision-Point (pose &optional (endFrame "/base_footprint")) 
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
      pose 
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

(defun transform-Pose (pose targetframe)
  "gets a Geometry_msgs/PoseStamped in frame x and outputs geometry_msgs/PoseStamped in targetframe"
  (let ((pose-transformable (cl-transforms-stamped:from-msg pose)))
    (let ((transform-listener (make-instance 'cl-tf:transform-listener)))
      (sleep 5.0)
      (cl-tf:to-msg 
       (cl-tf:transform-pose-stamped transform-listener
                                     :pose pose-transformable
                                     :target-frame targetframe))))
  )



(defun try-To-Grab-Different-Location(x y z)
  "trying to grab the object now again with different location.."
  (roslisp:ros-info (try-To-Poke-Different-Location)
                    "trying to grab the object now again with different location..")
  (let ((position (make-array '(3)  
                              :initial-contents '(1.123 1.5 0.57)))) 
    (loop for ya across position do
      (planning-move::move-Base-To-Point x ya z 180)
      (let ((rotation (make-array '(3)  
                                  :initial-contents '(0 -10 10))))
        (loop for r across rotation do
          (progn
            (cram-language:wait-for
             (planning-move::move-Base-To-Point x (+ y ya) z (+ r 180)))
            (if
             (eq 1
                 (planning-motion::call-motion-move-arm-homeposition))
             (print r)
             (return-from try-To-Grab-Different-Location "nooo"))))))))




(defun should-Robo-Use-Left-Or-Right-Arm (pose &optional (endFrame "/base_footprint"))
  "decides if the left or right arm is chosen depends on which one is closer"
  (let ((pointTransformed
          (transformation-Vision-Point pose endFrame)))
    (roslisp:with-fields (y) pointTransFormed
      (progn
        (if (> y 0)
            (return-from should-Robo-Use-Left-Or-Right-Arm 7)
            (return-from should-Robo-Use-Left-Or-Right-Arm 6))))))

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
        (vision_suturo_msgs-msg:object_information)))
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

;;fliegt raus sobald motion auf pose stamped ist.
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
                                                      (write-to-string amount)))))(print "made it"))

(defun init-pr2 ()
  "Subscribes to topics for a pr2 and binds callbacks."
  (roslisp:subscribe "/amcl_pose" "geometry_msgs/PoseWithCovarianceStamped" #'pose-cb))

(defun pose-cb (msg)
  "Callback for pose values. Called by the pose topic subscriber."
  (setf (cram-language:value *pr2-pose*) msg))


(Defun move-pr2 (x y z)
  ".."
  (planning-move::move-base-to-point-safe x y z
                                          (angle-from-pr2-pose-to-point x y z) 10))





(defun angle-From-Pr2-Pose-To-Point (x-goal y-goal z-goal)
  ""
  (init-pr2)
  (roslisp:with-fields
      ((x
        (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (y
        (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (z
        (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (xq
        (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (yq
        (geometry_msgs-msg:y geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (zq
        (geometry_msgs-msg:z geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose))
       (w
        (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose geometry_msgs-msg:pose)))
      (cram-language:value *pr2-pose*)
    (let
        ((angle-base-link
           (/
            (* 180
               (let
                   ((diff-pose
                      (cl-transforms:transform-point
                       (cl-tf::transform-inv
                        (cl-tf:make-transform
                         (cl-tf:make-3d-vector x y z)
                         (cl-tf:make-quaternion xq yq zq w)))
                       (cl-tf:make-3d-vector x-goal y-goal z-goal))))
                 (atan (cl-tf:y diff-pose)
                       (cl-tf:x diff-pose)))) pi))
                                        ;angle-base-link-stop
         (axis-angle
           (progn
             (/
              (* 180
                 (multiple-value-bind
                       (axis angle)
                     (cl-tf:quaternion->axis-angle
                      (cl-tf:make-quaternion xq yq zq w))
                   (nth 1(list axis angle)))) pi))))
      (print axis-angle) (print angle-base-link)
      (print (- angle-base-link axis-angle)))))



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
    (cram-language:sleep 0.01)
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
                       (cram-language:value *gripper-righ-state-fluent*) nil)
                      (setf
                       (cram-language:value *gripper-righ-state-fluent*) T)))))
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
                       (cram-language:value *gripper-left-state-fluent*) nil)
                      (setf
                       (cram-language:value *gripper-left-state-fluent*) T)))))))

(defun vis-init () 
  (setf *perception-publisher* 
        (roslisp:advertise "/beliefstate/perceive_action" "knowledge_msgs/PerceivedObject")))



(defun publish-pose (label object_pose)
  (when *perception-publisher*
    (roslisp:publish *perception-publisher*
                     (roslisp:make-message "knowledge_msgs/PerceivedObject"
                                           (object_label) label
                                           (object_pose) object_pose))))

(defun test-left-gripper ()
  (cram-language:top-level
    (cram-language:pursue
      (cram-language:wait-for *gripper-left-state-fluent*)
      (cram-language:unwind-protect
           (loop for i from 1 to 1000 do
             (progn
               (print i)
               (cram-language:sleep 0.1)
               ))
        (progn
          (read-char))))))





(defun init-Marker ()
  (setf *text-publisher* 
        (roslisp:advertise "/visualization_marker" "visualization_msgs/Marker")))

(defun init-Model-Publisher ()
  (setf *model-publisher*
        (roslisp:advertise "/gazebo/set_model_state" "gazebo_msgs/ModelState")))




(defun publish-Text (string)
  (roslisp:publish *text-publisher*
                   (roslisp:make-message "visualization_msgs/Marker" (frame_id header) "map"
                                         ns "planning_namespace"
                                         id 0
                                         type 9
                                         action 0
                                         pose (roslisp:make-msg "geometry_msgs/Pose"
                                                                (position)
                                                                (roslisp:make-msg "geometry_msgs/Point"
                                                                                  (x) 0
                                                                                  (y) 0
                                                                                  (z) 3)
                                                                (orientation)
                                                                (roslisp:make-msg "geometry_msgs/Quaternion"
                                                                                  (w) 1))
                                         (x scale) 0.2
                                         (y scale) 0.2
                                         (z scale) 0.2
                                         (r color) 0.5
                                         (g color) 0.8
                                         (b color) 1.0
                                         (a color) 1.0
                                         (text) string)))

(defun publish-Model-Pose (string)
  (roslisp:publish *model-publisher*
                   (roslisp:make-message "gazebo_msgs/ModelState"
                                         model_name string
                                         pose (roslisp:make-msg "geometry_msgs/Pose"
                                                                (position)
                                                                (roslisp:make-msg "geometry_msgs/Point"
                                                                                  (x) -0.8
                                                                                  (y) 1
                                                                                  (z) 0.9500)
                                                                (orientation)
                                                                (roslisp:make-msg "geometry_msgs/Quaternion"))
                                         twist (roslisp:make-msg "geometry_msgs/Twist"))))




(defun grab-Object-Right ()
  (sleep 5.0)
  (roslisp:with-fields (right_gripper)
      (cram-language:wait-for
       (planning-knowledge::empty-gripper))
    (print right_gripper)
    (roslisp:with-fields (object_label_1)
        (cram-language:wait-for
         (planning-knowledge::objects-to-pick))
      (print object_label_1)
      (if
       (>
        (length object_label_1) 0)
       (if
        (eq T right_gripper)
        (progn
          (planning-logic::publish-text "trying to grab now with right arm")
          (roslisp:with-fields (grasp_pose)
              (cram-language:wait-for
               (planning-knowledge::how-to-pick-objects object_label_1))
            (cram-language:wait-for
             (planning-motion::call-motion-move-arm-to-point grasp_pose object_label_1 6))
            (planning-motion::call-motion-move-arm-homeposition 11)))
        (planning-logic::publish-text "can't grab the an object with right"))
       (roslisp:with-fields (left_gripper)
           (planning-knowledge::empty-gripper)
         (if
          (eq T left_gripper)
          (progn
            (planning-logic::publish-text "trying to grab now with left arm")
            (roslisp:with-fields (grasp_pose)
                (cram-language:wait-for
                 (planning-knowledge::how-to-pick-objects object_label_1))
              (cram-language:wait-for
               (planning-motion::call-motion-move-arm-to-point grasp_pose object_label_1 7))
              (planning-motion::call-motion-move-arm-homeposition 12)))
          (planning-logic::publish-text "can't grab the object with left")))))))



(defun grab-Object-Left ()
  (sleep 5.0)
  (roslisp:with-fields (left_gripper)
      (cram-language:wait-for
       (planning-knowledge::empty-gripper))
    (print left_gripper)
    (roslisp:with-fields
        (object_label_1)
        (cram-language:wait-for
         (planning-knowledge::objects-to-pick))
      (print object_label_1)
      (if
       (>
        (length object_label_1) 0)
       (if
        (eq T left_gripper)
        (progn
          (planning-logic::publish-text "trying to grab now with right arm")
          (roslisp:with-fields (grasp_pose)
              (cram-language:wait-for
               (planning-knowledge::how-to-pick-objects object_label_1))
            (cram-language:wait-for
             (planning-motion::call-motion-move-arm-to-point grasp_pose object_label_1 7))
            (planning-motion::call-motion-move-arm-homeposition 12)))
        (planning-logic::publish-text "can't grab the an object with right"))
       (roslisp:with-fields (right_gripper)
           (planning-knowledge::empty-gripper)
         (if (eq T right_gripper)
             (progn
               (planning-logic::publish-text "trying to grab now with left arm")
               (roslisp:with-fields (grasp_pose)
                   (cram-language:wait-for
                    (planning-knowledge::how-to-pick-objects object_label_1))
                 (cram-language:wait-for
                  (planning-motion::call-motion-move-arm-to-point grasp_pose object_label_1 6))
                 (planning-motion::call-motion-move-arm-homeposition 11)))
             (planning-logic::publish-text "can't grab the object with left")))))))


(defun grab-Left-Or-Right ()
  (roslisp:with-fields (object_label_1)
      (planning-knowledge::objects-to-pick)
    (if
     (> (length object_label_1) 0)
     (roslisp:with-fields (grasp_pose)
         (planning-knowledge::how-to-pick-objects object_label_1)
       (if (=
            (should-robo-use-left-or-right-arm grasp_pose) 7)
           (grab-Object-Left)
           (grab-Object-Right))))))



(defun move-Base (x y z angle &optional (motion 1))
  (roslisp:with-fields
      ((pr2-x
        (geometry_msgs-msg:x
         geometry_msgs-msg:position
         geometry_msgs-msg:pose
         geometry_msgs-msg:pose)))
      (cram-language:value planning-logic:*pr2-pose*)
    (if (and (< pr2-x 0) (> x 0))
        (planning-move::move-base-to-point 0.15 0.5 0 -90)
        (if (and (> pr2-x 0) (< x 0))
            (planning-move::move-base-to-point 0.15 0.5 0 -90))))
  (planning-move::move-base-to-point x y z angle motion))



;;muss überarbeitet werden sobald orientation hinzugefügt wurde!
;; (defun let-Robo-Try-To-Poke (point-for-motion number-for-arm)
;;   "trying to Poke the object, first both arms will be used after that the robot will try different poses."
;;   (roslisp:ros-info (let-Robo-Try-To-Poke)
;;                     "trying to poke the object now...")
;;   (if (not(eq T (planning-motion::motion-To-Point point-for-motion number-for-arm)))
;;       (progn
;;         (if (/= 2 number-for-arm)
;;             (planning-motion::motion-To-Point point-for-motion 2)
;;             (planning-motion::motion-To-Point point-for-motion 3)))))




;; (cram-language:top-level
;;            (let ((loop-Finished nil))
;;              (cram-language:pursue
;;                (cram-language:wait-for planning-logic::*gripper-left-state-fluent*)
;;                (progn
;;                  (loop for i from 1 to 1000 do
;;                  (progn
;;                    (print i)
;;                    (cram-language:sleep 1.0)
;;                    )) 
;;                (setf loop-Finished T)))
;;              (unless loop-Finished 
;;                (cpl:fail "hello"))))
