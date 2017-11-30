(in-package :planning-vision)

(defun call-vision-point (point-center-of-object)
  "Call vision service, to look for point. Returns ObjectDetection object"
  (setf point-center-of-object
        (roslisp:call-service "/vision_main/visObjectInfo" 'object_detection-srv:VisObjectInfo)))
  




