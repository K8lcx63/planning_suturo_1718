(in-package :planning-vision)

(defvar not-a-number)

(defun call-vision-point ()
  "Call vision service, to look for point. Returns ObjectDetection object"
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")))
      (let ((response
              (roslisp:call-service
               "/vision_main/objectPoint"
               'object_detection-srv:visobjectinfo)))
        ;;Debug to find NaN message from vision
        (setf not-a-number response)
        (print not-a-number)
        (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                              (y (geometry_msgs-msg:y geometry_msgs-msg:point))
                              (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
            (object_detection-msg:position (object_detection-srv:object response))
          ;;If Vision returns a NaN and it is of type String this will recover from the error
          (if (or (stringp x)
                  (stringp y)
                  (stringp z))
              (cpl:fail "One or more of the coordinates returned by the service /vision_main/objectPoint is of type String
                        which is likely the not a number error")
              (if (or (sb-ext:float-nan-p x)
                      (sb-ext:float-nan-p y)
                      (sb-ext:float-nan-p z))
                  (cpl:fail "One or more of the coordinates returned by the service /vision_main/objectPoint is  not a number")
                  (roslisp:with-fields (object_detection-msg:error) (object_detection-srv:object response)
                    (if (or (string= "Cloud empty. " object_detection-msg:error)
                            (string= "Cloud was empty after filtering. " object_detection-msg:error)
                            (string= "No plane found. " object_detection-msg:error)
                            (string= "Final extracted cluster was empty. " object_detection-msg:error))
                        (cpl:fail "service call failed")
                        (progn
                          (format t "service call successful")
                          (return-from call-vision-point
                            (roslisp:call-service "/vision_main/objectPoint" 'object_detection-srv:VisObjectInfo))))))))))))


(defun call-vision-pose ()
  "Call vision service, to look for pose. Returns ObjectDetection object"
  (roslisp:call-service "/vision_main/objectPose" 'vision_msgs-srv:GetObjectInfo))

(defun check-points-is-equal (msg-one msg-two delta)
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
    
  
(defun askFor ()
  "asking vision for the ice"
  (let ((object-Info (roslisp:call-service "/vision_main/objectPose" 'vision_msgs-srv:GetObjectInfo)))
    (roslisp:with-fields (info) object-Info (setf object-Info info))
    (roslisp:with-fields (isstanding) object-Info (setf object-Info isstanding))
    (if (= object-Info 2)
        (return-from askFor T))))
  
  




