(in-package :planning-logic)

(defvar *pose* nil)



(defun transformation-Vision-Point (point-center-of-object &optional (endFrame "/base_footprint"))
  "transform a msgs with an optional Frame, default is base_footprint"
  
  (roslisp:with-fields ((x
                         (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                        (y
                         (geometry_msgs-msg:y geometry_msgs-msg:point))
                        (z
                         (geometry_msgs-msg:z geometry_msgs-msg:point))
                        (startFrame
                         (STD_msgs-msg:frame_id geometry_msgs-msg:header)))
      (object_detection-msg:position
       (object_detection-srv:object point-center-of-object))
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


(defun let-Robo-Try-To-Poke (point-for-motion number-for-arm)
  "trying to Poke the object, first both arms will be used after that the robot will try different poses."
  (roslisp:ros-info (let-Robo-Try-To-Poke)
                    "trying to poke the object now...")
  (if (not(eq T (planning-motion::motion-To-Point point-for-motion number-for-arm)))
      (progn
        (if (/= 2 number-for-arm)
            (planning-motion::motion-To-Point point-for-motion 2)
            (planning-motion::motion-To-Point point-for-motion 3)))))

(defun try-To-Poke-Different-Location(point-for-motion number-for-arm)
  "trying to poke the object now again with different location.."
   (roslisp:ros-info (try-To-Poke-Different-Location)
                    "trying to poke the object now again with different location..")
  (let ((position (make-array '(4)  
                                 :initial-contents '(1.3 0.5 0 -0.3)))) 
    (loop for y across position do
      (planning-move::move-Base-To-Point 0.8 y 0 30)
      (let ((rotation (make-array '(3)  
                                  :initial-contents '(30 31 29)))) 
        (loop for r across rotation do
             (progn
               (planning-move::move-Base-To-Point 0.8 y 0 r)
               (if (eq T(let-Robo-Try-To-Poke point-for-motion number-for-arm))
                   (return-from try-To-Poke-Different-Location T))))))))
            


(defun should-Robo-Use-Left-Or-Right-Arm (visionPoint)
  "decides if the left or right arm is chosen depends on which one is closer"
  (let ((pointTransformed(transformation-Vision-Point visionPoint)))
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
                (set-Params-Features 0 0 308 768 amount)
                (set-Params-Features
                 (* (- amount 1) 308)
                 (* (- amount 1) 768)
                 (* amount 308)
                 (* amount 768)amount)))))
  (return-from disassemble-vision-call *pose*))

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
