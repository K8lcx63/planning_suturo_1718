(in-package :planning-logic)

(defvar *joint-states* 0)
(defvar *perception-publisher*)
(defvar *text-publisher*)
(defvar *model-publisher*)
(defvar *pr2-pose* (cram-language:make-fluent :name :pr2-pose) nil)
(defvar *gripper-righ-state-fluent* (cram-language:make-fluent))
(defvar *gripper-left-state-fluent* (cram-language:make-fluent))
(defvar *l* nil)
(defvar *r* nil)
(defvar *x* nil)
(defvar *y* nil)
(defvar *angle* nil)
(defvar *counter* nil)
(defvar *first-gripper* nil)
(defvar *second-gripper* nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;INIT;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-logic ()
  (vis-init)
  (init-gripper-states)
  (init-pr2)
  (init-marker)
  (init-model-publisher))



(defun init-pr2 ()
  "Subscribes to topics for a pr2 and binds callbacks."
  (roslisp:subscribe "/amcl_pose" "geometry_msgs/PoseWithCovarianceStamped" #'pose-cb))


(defun init-gripper-states ()
  "subscribes to /joint_states and gives data to according handling method"
  (progn
    (roslisp:subscribe
     "/joint_states"
     "sensor_msgs/JointState"
     #'is-gripper-filled :max-queue-length 1)
    (return-from init-gripper-states())))


(defun init-Marker ()
  (setf *text-publisher* 
        (roslisp:advertise "/visualization_marker" "visualization_msgs/Marker")))

(defun init-Model-Publisher ()
  (setf *model-publisher*
        (roslisp:advertise "/gazebo/set_model_state" "gazebo_msgs/ModelState")))


(defun vis-init () 
  (setf *perception-publisher* 
        (roslisp:advertise "/beliefstate/perceive_action" "knowledge_msgs/PerceivedObject")))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;PUBLISH;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun publish-Text-Pose (string pose)
  (roslisp:publish *text-publisher*
                   (roslisp:make-message "visualization_msgs/Marker" (frame_id header) "map"
                                         ns "planning_namespace"
                                         id 0
                                         type 9
                                         action 0
                                         pose pose
                                         (x scale) 0.2
                                         (y scale) 0.2
                                         (z scale) 0.2
                                         (r color) 0.5
                                         (g color) 0.8
                                         (b color) 1.0
                                         (a color) 1.0
                                         (text) string)))

(defun publish-sphere (pose)
  (roslisp:publish *text-publisher*
                   (roslisp:make-message "visualization_msgs/Marker" (frame_id header) "base_link"
                                         ns "planning_namespace"
                                         id 101010
                                         type 0
                                         action 0
                                         pose pose
                                         (x scale) 0.2
                                         (y scale) 0.2
                                         (z scale) 0.2
                                         (r color) 0.5
                                         (g color) 0.8
                                         (b color) 1.0
                                         (a color) 1.0)))


(defun publish-pose (label object_pose)
  (when *perception-publisher*
    (roslisp:publish *perception-publisher*
                     (roslisp:make-message "knowledge_msgs/perceivedobject"
                                           (object_label) label
                                           (object_pose) object_pose))))




(defun publish-Pose-JaMilch ()
  (let ((object_pose_stamped 
          (roslisp:make-msg "geometry_msgs/posestamped"
                            header (roslisp:make-msg "std_msgs/header"
                                                     (frame_id) "/map")
                            pose (roslisp:make-msg "geometry_msgs/pose"
                                                   (position) (roslisp:make-msg "geometry_msgs/point"
                                                                                (x) -0.9
                                                                                (y) 1.5
                                                                                (z) 0.932383)
                                                   (orientation) (roslisp:make-msg "geometry_msgs/quaternion"
                                                                                   (x) 1
                                                                                   (y) 0
                                                                                   (z) 0
                                                                                   (w) 0)))))
    (when *perception-publisher*
      (roslisp:publish *perception-publisher*
                       (roslisp:make-message "knowledge_msgs/PerceivedObject"
                                             (object_label) "JaMilch"
                                             (object_pose) object_pose_stamped)))
    
    (planning-logic::save-object "JaMilch" object_pose_stamped)
    (roslisp:set-param "counter"
                       (+ 1 (roslisp:get-param "counter")))))







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
                                                                                  (x) 0
                                                                                  (y) 0
                                                                                  (z) 0
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;TRANSFORMATIONS;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;OBJECT;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun save-Object (label object_pose)
  (roslisp:with-fields (origin)
      (transformation-pose-stamped object_pose "/map")
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


(defun percieve-Objetcs ()
  (print "geht er hier rein percieve-Objects")
  (roslisp:with-fields ((labels
                            (vision_suturo_msgs-msg:labels
                                vision_suturo_msgs-srv:clouds)))
      (cram-language:wait-for (planning-vision:call-vision-object-clouds))
    (loop for i from 1 to (array-total-size labels)
          do
             (sleep 5.0)
             (print "array-total-size:")             (print (array-total-size labels)) (print labels) 
             (let ((name
                     (aref labels (- i 1))))
               (roslisp:with-fields (object_pose)
                   (planning-vision:call-vision-object-pose name (- i 1))
                 (print object_pose)
                 (planning-logic:publish-pose name object_pose)
                 (planning-logic::save-object name object_pose)))
             (roslisp:set-param "counter"
                                (+ 1 (roslisp:get-param "counter")))
          (print (roslisp:get-param "counter")))))

(defun percieve-Objects-And-Search (label)
  (print "percieve objects an search:")
  (roslisp:with-fields ((labels
                            (vision_suturo_msgs-msg:labels
                                vision_suturo_msgs-srv:clouds)))
      (planning-vision:call-vision-object-clouds)
    (print "hier rechnet er mit vision")
    (loop for i from 1 to (array-total-size labels)
          do
             (let ((name
                     (aref labels (- i 1))))
               (if (eq label name)
                   (print "hier ist eq label")
                   (progn
                     (roslisp:with-fields (object_pose)
                         (planning-vision:call-vision-object-pose name (- i 1))
                       (planning-logic:publish-pose name object_pose)
                       (planning-logic::save-object name object_pose))
                     (return-from percieve-objects-and-search T)))))
    (return-from  percieve-objects-and-search nil)))


(defun make-Object-Pose-For-Handshake (label)
  (roslisp:make-msg "geometry_msgs/PoseStamped"
                    (header) (roslisp:make-msg "std_msgs/header"
                                               (frame_id) "/map")
                    (pose) (roslisp:make-message "geometry_msgs/Pose" 
                                                 (position) (roslisp:make-msg "geometry_msgs/Point"
                                                                              (x) (nth 0 (roslisp:get-param label))
                                                                              (y) (nth 1 (roslisp:get-param label))
                                                                              (z) (nth 2 (roslisp:get-param label)))
                                                 (orientation) (roslisp:make-msg "geometry_msgs/Quaternion"
                                                                                 (w) 1))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;GRAB;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun how-Many-Gripper ()
  (roslisp:with-fields
      ((x knowledge_msgs-srv:left_gripper)
       (y knowledge_msgs-srv:right_gripper right_gripper))
      (planning-knowledge:empty-gripper)
    (if (eq x T) (setf *l* 0) (setf *l* 1)) (if (eq y T) (setf *r* 0) (setf *r* 1)))) 



(defun grab-Or-Place-Object (label x y angle arm-first arm-first-homeposi grab-string &optional arm-second arm-second-homeposi)
  (sleep 5.0)
  (print "er geht in die FUNKTION GRAB-OR_PLACE_OBJECT REIN")
  (block start-Grab
    (print "block start-grab")
;which gripper
    (if (= arm-first 6)
       (roslisp:with-fields (right_gripper)
           (cram-language:wait-for
            (planning-knowledge:empty-gripper))
         (setf *first-gripper* right_gripper))
       (roslisp:with-fields (left_gripper)
           (cram-language:wait-for
            (planning-knowledge:empty-gripper))
         (setf *first-gripper* left_gripper)))
;trying to grab first arm
    (if (eq T *first-gripper*) 
            (progn
               (print "erster arm:")
               (print arm-first)
 
              (if (eq (try-to-grab-or-place-different-location x y 0 angle label arm-first) T)
                  (progn
                    (planning-motion::call-motion-move-arm-homeposition arm-first-homeposi)
                    (return-from grab-or-place-object T)))))
                       ;do we want to grab?
    (if (string= grab-string "grab")
                      ;grab with second arm
        (progn
          (print "grab with second arm")
          (sleep 5.0)
                
                    ;which gripper is empty
          (if (= arm-second 6)
              (roslisp:with-fields (right_gripper)
                  (cram-language:wait-for
                   (planning-knowledge:empty-gripper))
                (setf *second-gripper* right_gripper))
              (roslisp:with-fields (left_gripper)
                  (cram-language:wait-for
                   (planning-knowledge:empty-gripper))
                (setf *second-gripper* left_gripper)))
              
          (if(eq T *second-gripper*)
             (progn
               (print "second arm:")
               (print arm-second)
               (if (eq (try-to-grab-or-place-different-location x y 0 angle label arm-second) T)
                   (progn
                     (planning-motion:call-motion-move-arm-homeposition arm-second-homeposi)
                             (return-from grab-or-place-object T)))))
          (print "counter bei grab:")
          (print *counter*)))

  (return-from grab-or-place-object nil))) 



(defun trying-To-Grab (label x y angle arm-first arm-first-homeposi grab-string arm-second arm-second-homeposi)
  (if (eq nil 
          (grab-or-place-object label x y angle arm-first arm-first-homeposi grab-string arm-second arm-second-homeposi))
      (if (and (string= grab-string "grab")(= *counter* 0))
          (progn
            (setf *counter* 1)
            (calculate-object-and-pr2-distance label)
            (sleep 5.0)
            (if (eq T
                    (grab-or-place-object label *x* *y* *angle* arm-first arm-first-homeposi grab-string arm-second arm-second-homeposi))
                (return-from trying-To-Grab T) 
                                        ; since here interaction with human
                (roslisp:with-fields
                    ((left_gripper (knowledge_msgs-srv:left_gripper))
                     (right_gripper(knowledge_msgs-srv:right_gripper)))
                    (cram-language:wait-for
                     (planning-knowledge:empty-gripper))
                  (roslisp:with-fields (force)
                      (cram-language:wait-for
                       (planning-knowledge:how-to-pick-objects label))
                    (if (and (eq T left_gripper)(= arm-first 7))
                        (planning-interaction:ask-human-to-move-object (make-object-pose-for-handshake label) label force 3)
                        (if (eq T right_gripper)
                            (planning-interaction:ask-human-to-move-object (make-object-pose-for-handshake label) label force 2))))))))))
                                        ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>HIER NOCH EIN ELSE FÜR STRING GRAB WENN NICHT ABSTELLBAR  HUMAN INTERACTION


(defun grab-Left-Or-Right (x y angle label)
  (sleep 5.0)
  (setf *counter* 0)
  (roslisp:with-fields (object_label_1)
      (planning-knowledge::objects-to-pick)
    (if
     (> (length object_label_1) 0)
     (if
      (= (should-robo-use-left-or-right-arm label) 7)
      (trying-To-Grab label x y angle 7 12 "grab" 6 11)
      (trying-To-Grab label x y angle 6 11 "grab" 7 12)))))

(defun place-Object ())




(defun try-To-Grab-Or-Place-Different-Location(x y z w label command)
  (roslisp:with-fields ((grasp_pose_array knowledge_msgs-srv:grasp_pose_array)
                        (force knowledge_msgs-srv:force))
      (cram-language:wait-for
       (planning-knowledge::how-to-pick-objects label))
    (planning-move:move-base-to-point x y z w 10)
    (let ((position (make-array '(1)  
                                :initial-contents '(0))))
      (loop for ya across position do
        (print "print ya")
        (print ya)
        (let ((rotation (make-array '(1)  
                                    :initial-contents '(0))))

              (planning-motion:call-motion-move-arm-homeposition 10)
              (loop for r across rotation do
              (print "ab hier versucht er zu rotatieren atm auf 0 gesetzt")
              (cram-language:wait-for
               (planning-move:move-Base-To-Point 0 ya 0 r 10 "/base_link"))
              (if
               (eq 1(planning-motion:call-motion-move-arm-to-point grasp_pose_array label command force))
               (return-from try-to-grab-or-place-different-location T)
               ;; (if
               ;;  (eq (percieve-Objects-And-Search label) nil)
               ;;  (return-from try-to-grab-or-place-different-location nil)
                
               )))))))                       

(defun should-Robo-Use-Left-Or-Right-Arm (label)
  "decides if the left or right arm is chosen depends on which one is closer"
  (if (> (get-Information-About-Object label) 0)
      (return-from should-Robo-Use-Left-Or-Right-Arm 7)
      (return-from should-Robo-Use-Left-Or-Right-Arm 6)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;OTHERTR;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-Object-And-Pr2-Distance (label)
  (let ((punkt1
          (distance -1 0
                    (nth 0 (roslisp:get-param label))
                    (nth 1 (roslisp:get-param label))))
        (punkt2
          (distance -1.7 0.20
                    (nth 0 (roslisp:get-param label))
                    (nth 1 (roslisp:get-param label)))))
    (print "print punkt1:") (print punkt1) (print "print punkt 2:") (print punkt2)
    (if (< punkt1 punkt2)
        (set-first-posi)
        (set-second-posi))))



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



(defun disassemble-graspindividual-response (msg)
  (geometry_msgs-msg:y
   (Geometry_msgs-msg:position
    (geometry_msgs-msg:pose 
     (knowledge_msgs-srv:place_pose msg)))))

(defun distance (xa ya xb yb)
  "calctulating the distance between 2 2d-vectors"
  (sqrt
   (+
    (expt
     (- xa xb) 2)
    (expt
     (- ya yb) 2))))




(defun square (x)
  (* x x))


(defun pose-cb (msg)
  "Callback for pose values. Called by the pose topic subscriber."
  (setf (cram-language:value *pr2-pose*) msg))

(defun set-First-Posi ()
  (setf *x* -1)
  (setf *Y* 0)
  (setf *angle* 90)
   (print "x y z angle set-first-posi")(print *x*) (print *y*) (print *angle*))
(defun set-Second-Posi ()
  (setf *x* -1.7)
  (setf *y* 0.20)
  (setf *angle* 30)
  (print "x y z angle set-secoind-posi")(print *x*) (print *y*) (print *angle*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;STILL USABLE;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
