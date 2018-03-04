(in-package :planning-vision)

(defvar not-a-number)




(defun check-Points-Is-Equal (msg-one msg-two delta)
  "Compares two points with delta."
  (roslisp::ros-info "check-points-is-equal" "Starting to check if point of object is still valid")
  (roslisp:with-fields ((x1 (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                        (y1 (geometry_msgs-msg:y geometry_msgs-msg:point))
                        (z1 (geometry_msgs-msg:z geometry_msgs-msg:point)))
      (object_detection-msg:position (object_detection-srv:object msg-one))
     (roslisp:with-fields ((x2 (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                           (y2 (geometry_msgs-msg:y geometry_msgs-msg:point))
                           (z2 (geometry_msgs-msg:z geometry_msgs-msg:point)))
                        (object_detection-msg:position (object_detection-srv:object msg-two))
       (let (
             (dx (abs (- x1 x2)))
             (dy (abs (- y1 y2)))
             (dz (abs (- z1 z2))))
         (if (and
              (<= dx delta)
              (<= dy delta)
              (<= dz delta))
             (print t)
             (print nil))))))
    

  

(defun call-Vision-Object-Clouds ()
  "calling Visions Service for the objectclouds"
  (roslisp:call-service
   "/vision_suturo/objects_information"
   'vision_suturo_msgs-srv:objects)) ;denkemal dass ist ihr neuer call
    



