(in-package :planning-vision)

(defun call-vision-point ()
  "Call vision service, to look for point. Returns ObjectDetection object"
  (roslisp:call-service "/vision_main/objectPoint" 'object_detection-srv:VisObjectInfo))

(defun call-vision-pose ()
  "Call vision service, to look for point. Returns ObjectDetection object"
  (roslisp:call-service "/vision_main/objectPose" 'vision_msgs-srv:GetObjectInfo))

(defun check-points-is-equal (msg-one msg-two delta)
  "Compares two points with delta."
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
             (dz (abs (- z1 z2)))
             )
         (if (and
              (<= dx delta)
              (<= dy delta)
              (<= dz delta)
              )
             (print t)
             (print nil)
             )
         )
       )
    )
  )
    
  
  
  




