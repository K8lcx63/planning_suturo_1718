(in-package :planning-old)




;:NICHT MEHR AKTUELL ENTSCHEIDET MOTION SELBER
;; (defun ask-Knowledge-For-Poke-Point(point-center-of-object)
;;   "Calling knowledge-service to look for a point to poke for specific object"
;;   (cpl:with-retry-counters ((retry-counter 10))
;;     (cpl:with-failure-handling
;;         ((cpl:simple-plan-failure (error-object)
;;            (format t "An error happened: ~a~%" error-object)
;;            (cpl:do-retry retry-counter
;;              (format t "Now retrying~%")
;;              (roslisp:ros-info "Knowledge" "Now retrying")
;;              (cpl:retry))
;;            (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
;;            (roslisp:ros-error "Knowledge" "Reached maximum amount of retries. Now propagating the failure up.")))    
;;       (roslisp:with-fields (object) point-center-of-object
;;         (setf point-center-of-object
;;               (roslisp:call-service "/poke_position_service/calculate_poke_position" 'object_detection-srv:PokeObject :detection object))
;;         (roslisp:with-fields (error_message) point-center-of-object
;;           (if (or (string= error_message "Failed to call service 'calculate_poke_position'. Transformation failed!")
;;                   (string= error_message "Failed to call service 'calculate_poke_position'. Prolog found no solution!"))
;;               (progn
;;                 (roslisp:ros-warn "Knowledge" "Service call failed.")
;;                 (cpl:fail 'planning-error::knowledge-error :message (format nil "knowledge service failed with: ~a" error_message)))
;;               (print point-center-of-object)))))))  





;;NICHT MEHR AKTUELL!    
;; (defun call-Vision-Point ()
;;   "Call vision service, to look for point. Returns ObjectDetection object"
;;   (cpl:with-retry-counters ((retry-counter 10))
;;     (cpl:with-failure-handling
;;         ((cpl:simple-plan-failure (error-object)
;;            (format t "An error happened: ~a~%" error-object)
;;            (cpl:do-retry retry-counter
;;              (format t "Now retrying~%")
;;              (cpl:retry))
;;            (format t "Reached maximum amount of retries. Now propagating the failure up.~%")))
;;       (let ((response
;;               (roslisp:call-service
;;                "/vision_main/objectPoint"
;;                'object_detection-srv:visobjectinfo)))
;;         ;;Debug to find NaN message from vision
;;         (setf not-a-number response)
;;         (print not-a-number)
;;         (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)) 
;;                               (y (geometry_msgs-msg:y geometry_msgs-msg:point))
;;                               (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
;;             (object_detection-msg:position (object_detection-srv:object response))
;;           ;;If Vision returns a NaN and it is of type String this will recover from the error
;;           (if (or (stringp x)
;;                   (stringp y)
;;                   (stringp z))
;;               (cpl:fail 'planning-error::vision-error :message "One or more of the coordinates returned by the service /vision_main/objectPoint is of type String
;;                         which is likely the not a number error")
;;               (if (or (sb-ext:float-nan-p x)
;;                       (sb-ext:float-nan-p y)
;;                       (sb-ext:float-nan-p z))
;;                   (cpl:fail 'planning-error::vision-error :message "One or more of the coordinates returned by the service /vision_main/objectPoint is not a number")
;;                   (roslisp:with-fields (object_detection-msg:error) (object_detection-srv:object response)
;;                     (if (or (string= "Cloud empty. " object_detection-msg:error)
;;                             (string= "Cloud was empty after filtering. " object_detection-msg:error)
;;                             (string= "No plane found. " object_detection-msg:error)
;;                             (string= "Final extracted cluster was empty. " object_detection-msg:error))
;;                         (cpl:fail 'planning-error::vision-error :message object_detection-msg:error)
;;                         (progn
;;                           (format t "service call successful")
;;                           (return-from call-vision-point
;;                             (roslisp:call-service "/vision_main/objectPoint" 'object_detection-srv:VisObjectInfo))))))))))))



;Hier müsste die logik noch einmal angepasst werden
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
