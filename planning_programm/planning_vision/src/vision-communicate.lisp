(in-package :planning-vision)

(defvar not-a-number)

(defun call-Vision-Object-Clouds ()
  "calling Visions Service for the objectclouds"
  (roslisp:call-service
   "/vision_suturo/objects_information"
   'vision_suturo_msgs-srv:objects)) 


(defun call-Vision-Object-pose (label index)
(roslisp:call-service "vision_suturo/objects_poses"
                      'vision_suturo_msgs-srv:poses
                      :labels label
                      :index index))







