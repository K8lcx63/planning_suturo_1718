(in-package :planning-vision)

(defun call-vision-point ()
  "Call vision service, to look for point. Returns ObjectDetection object"
	(roslisp:call-service "/VisObjectInfo" 'object_detection-srv:VisObjectInfo))
  




