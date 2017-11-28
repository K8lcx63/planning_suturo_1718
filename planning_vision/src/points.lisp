(in-package :planning-vision)

(defun call-vision-point ()
  "here we are asking the vision service for a point"
	(roslisp:call-service "/VisObjectInfo" 'object_detection-srv:VisObjectInfo))
  




