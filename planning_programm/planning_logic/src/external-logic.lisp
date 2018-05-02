(in-package :planning-logic)

(defvar *joint-states* 0)
(defvar *pose* nil)
(defvar *perception-publisher*)
(defvar *text-publisher*)
(defvar *model-publisher*)
(defvar *pr2-pose* (cram-language:make-fluent :name :pr2-pose) nil)
(defvar *gripper-righ-state-fluent* (cram-language:make-fluent))
(defvar *gripper-left-state-fluent* (cram-language:make-fluent))
(defvar *l* nil)
(defvar *r* nil)

(defun init-logic ()
  (vis-init)
  (init-gripper-states)
  (init-pr2)
  (init-marker)
  (init-model-publisher))

(defun square (x)
  (* x x))

(defun disassemble-graspindividual-response (msg)
  (geometry_msgs-msg:y
   (Geometry_msgs-msg:position
    (geometry_msgs-msg:pose 
     (knowledge_msgs-srv:place_pose msg)))))

;@param: pose, amount und optional endFrame
;@return: a transformed point-stamped
(defun transformation-Vision-Point (pose &optional (endFrame "/base_footprint")) 
  "transform a poseStamped-msg with an optional Frame, default is base_footprint, to a pointStamped" 
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



(defun try-To-Grab-Different-Location(x y z w pose-array direction-key force label command)
  "trying to grab the object now, with different locations and orientations."
  (let ((position (make-array '(5)  
                              :initial-contents '(0 0.10 0.15 -0.10 -0.15)))) 
    (loop for ya across position do
      (planning-move:move-Base-To-Point x (+ ya y) z w)
      (let ((rotation (make-array '(5)  
                                  :initial-contents '(0 10 20 -10 -20))))
        (loop for r across rotation do
          (progn
            (cram-language:wait-for
             (planning-move:move-Base-To-Point x (+ y ya) z (+ w r)))
            (if
             (eq 1
                 (planning-motion:call-motion-move-arm-to-point pose-array direction-key label command force))
             (return-from try-To-Grab-Different-Location nil))))))))




(defun should-Robo-Use-Left-Or-Right-Arm (label)
  "decides if the left or right arm is chosen depends on which one is closer"
  (if (> (get-Information-About-Object label) 0)
      (return-from should-Robo-Use-Left-Or-Right-Arm 7)
      (return-from should-Robo-Use-Left-Or-Right-Arm 6)))


;; fiegt raus, wenn Vision weiter ist
;; Beschreibung: Extrahiert alle Informationen aus der
;; Visioncloud und speichert normal_features, color_features,
;; object_amount und object_pose auf dem Parameterserver.

;; @param: visionclouds
;; @return: object_pose
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



;; Beschreibung: Eine Hilfsfunktion für die Funktion disassemble-Vision-Call,
;; um aus normal_features und color_features konkateniert ein features-X zu
;; erstellen. (Knowledge ben ̈otigt ein Array in dem zuerst color_features
;; vorkommen und direkt im Anschluss normal_features)

;; @param: normal-s color-s normal-e color-e amount
;; @return: Nil
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




;; Beschreibung: Die Funktion arbeitet mit Hilfe des Packets Cram-Language um
;; einen Winkel zu berechnen, wie der Pr2 sich aus der aktuellen Position
;; in der Welt drehen muss, damit er einen gegebenen Punkt erreichen kann.
;; Da der Roboter sich in dem Frame "map" bewegt, muss der Winkel noch mit der
;; aktuellen Orientierung des Pr2 subtrahiert werden, so dass am Ende
;; ein Winkel resultiert der in dem Frame "map" funktioniert.

;; @param: x-goal y-goal z-goal
;; @return: angle
(defun angle-From-Pr2-Pose-To-Point (x-goal y-goal z-goal)
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


(defun grab-Object-Right (label)
  (sleep 5.0)
  (roslisp:with-fields (right_gripper)
      (cram-language:wait-for
       (planning-knowledge::empty-gripper))
       (if
        (eq T right_gripper)
        (progn
          (planning-logic::publish-text "trying to grab now with right arm")
          (roslisp:with-fields ((grasp_pose_array)
                                (direction_key)
                                (force))
              (cram-language:wait-for
               (planning-knowledge::how-to-pick-objects label))
            (cram-language:wait-for
             (planning-logic:try-to-grab-different-location 0.29 1 0 180 grasp_pose_array direction_key force label 6))
            (planning-motion::call-motion-move-arm-homeposition 11)))
        (planning-logic::publish-text "can't grab the an object with right"))))




(defun grab-Object-Left (label)
  (sleep 5.0)
  (roslisp:with-fields (left_gripper)
      (cram-language:wait-for
       (planning-knowledge::empty-gripper))
       (if
        (eq T left_gripper)
        (progn
          (planning-logic::publish-text "trying to grab now with left arm")
          (roslisp:with-fields ((grasp_pose_array)
                                (direction_key)
                                (force))
              (cram-language:wait-for
               (planning-knowledge::how-to-pick-objects label))
            (cram-language:wait-for
             (planning-logic:try-to-grab-different-location 0.29 1 0 180 grasp_pose_array direction_key force label 7))
            (planning-motion::call-motion-move-arm-homeposition 12)))
        (planning-logic::publish-text "can't grab the an object with left"))))


(defun grab-Left-Or-Right (label)
  (sleep 5.0)
  (roslisp:with-fields (object_label_1)
      (planning-knowledge::objects-to-pick)
    (if
     (> (length object_label_1) 0)
     (if
      (= (should-robo-use-left-or-right-arm label) 7)
      (grab-Object-Left label)
      (grab-Object-Right label)))))



(defun move-Base (x y z angle &optional (motion 1))
  (roslisp:with-fields
      ((pr2-x
        (geometry_msgs-msg:x
         geometry_msgs-msg:position
         geometry_msgs-msg:pose
         geometry_msgs-msg:pose)))
      (cram-language:value *pr2-pose*)
    (if (and (< pr2-x 0) (> x 0))
        (planning-move::move-base-to-point 0.15 0.5 0 -90)
        (if (and (> pr2-x 0) (< x 0))
            (planning-move::move-base-to-point 0.15 0.5 0 -90))))
  (planning-move::move-base-to-point x y z angle motion))




(defun transformation-Pose-Stamped (pose &optional (endFrame "/base_footprint")) 
  "transform a msgs with an optional Frame, default is base_footprint" 
  (roslisp:with-fields 
      ((startFrame 
        (STD_msgs-msg:frame_id geometry_msgs-msg:header)) 
       (x 
        (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose)) 
       (y 
        (geometry_msgs-msg:y geometry_msgs-msg:position  geometry_msgs-msg:pose)) 
       (z 
        (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose))
       (w
        (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (xo
        (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (yo
        (geometry_msgs-msg:x geometry_msgs-msg:orientation geometry_msgs-msg:pose))
       (zo
        (geometry_msgs-msg:z geometry_msgs-msg:orientation geometry_msgs-msg:pose))) 
      pose 
    (let 
        ((transform-listener 
           (make-instance 'cl-tf:transform-listener)) 
         (tf-pose-stamped 
           (cl-tf:make-pose-stamped startFrame 0.0 
                                    (cl-transforms:make-3d-vector x y z)
                                    (cl-transforms:make-quaternion xo yo zo w)))) 
      (catch-Transformation-Pose-Stamped transform-listener tf-pose-stamped endFrame))))

(defun transformation-XYZ (x y z startFrame &optional (endFrame "/base_footprint"))  
    (let 
        ((transform-listener 
           (make-instance 'cl-tf:transform-listener)) 
         (tf-pose-stamped
           (cl-tf:make-pose-stamped startFrame 0.0 
                                    (cl-transforms:make-3d-vector x y z)
                                    (cl-transforms:make-quaternion 0 0 0 1)))) 
      (catch-Transformation-Pose-Stamped transform-listener tf-pose-stamped endFrame)))

(defun catch-Transformation-Pose-Stamped (transform-listener tf-pose-stamped endFrame)
  "transform-listener catch transformation (helpfunction)"
  (sleep 5.0)
           (cl-tf:transform-pose-stamped transform-listener
                                         :pose tf-pose-stamped
                                         :target-frame endFrame))


(defun distance (xa ya xb yb)
  "calctulating the distance between 2 2d-vectors"
  (sqrt
   (+
    (expt
     (- xa xb) 2)
    (expt
     (- ya yb) 2))))



(defun save-Object (label object_pose)
  (roslisp:with-fields (origin)
      (planning-logic:transformation-pose-stamped object_pose "/map")
     (roslisp:set-param label (roslisp:with-fields (x y z) origin (list x y z)))))

(defun get-Information-About-Object (label)
  (roslisp:with-fields (origin)
      (transformation-XYZ
       (nth 0 (roslisp:get-param label))
       (nth 1 (roslisp:get-param label))
       (nth 2 (roslisp:get-param label))
       "/map")
    (roslisp:with-fields (y) origin
      (return-from get-Information-About-Object y))))

(defun how-Many-Gripper ()
           (roslisp:with-fields
               ((x knowledge_msgs-srv:left_gripper)
                (y knowledge_msgs-srv:right_gripper right_gripper))
               (planning-knowledge:empty-gripper)
             (if (eq x T) (setf *l* 0) (setf *l* 1)) (if (eq y T) (setf *r* 0) (setf *r* 1)))) 
