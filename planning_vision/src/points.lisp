(in-package :planning-vision)

(defun call-vision-point ()
  "Call vision service, to look for point. Returns ObjectDetection object"
  (roslisp:call-service "/vision_main/objectPoint" 'object_detection-srv:VisObjectInfo))

(defun call-vision-pose ()
  "Call vision service, to look for point. Returns ObjectDetection object"
  (roslisp:call-service "/vision_main/objectPose" 'vision_msgs-srv:GetObjectInfo))

(defun check-points-is-equal (msg-one msg-two delta)
  "Maybe the ugliest function ever. Compares two points with delta."
  (roslisp:with-fields (object) msg-one
    (roslisp:with-fields (position) object
      (roslisp:with-fields (point) position
        (roslisp:with-fields (x y z) point
          (let ((x1 x) (y1 y) (z1 z))
            (roslisp:with-fields (object) msg-two
               (roslisp:with-fields (position) object
                 (roslisp:with-fields (point) position
                   (roslisp:with-fields (x y z) point
                     (let ((x2 x) (y2 y) (z2 z))
                       (let ((dx (abs (- x1 x2))) (dy (abs (- y1 y2))) (dz (abs (- z1 z2))))
                         (if (and (<= dx delta) (<= dy delta) (<= dz delta)) (print t)(print nil))
                         ))))))))))))
  




