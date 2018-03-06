(in-package :planning-knowledge)
 
(defvar *percieved-topic* nil)

(defun what-object(features)
  "Takes set of features to knowledge, and asks for identification"
  (roslisp:call-service "/svm_classifier/classify_service" 'knowledge_msgs-srv:Classify :features features))

(defun is-object(objectString features)
  "Takes features and string and asks knowledge if features are the object described in string"
  (roslisp:with-fields (label) (what-object features) (if (string= label objectString)(return-from is-object T)(return-from is-object nil))))

(defun init-Perceive-Topic ()
(setf *percieved-topic*
       (roslisp:advertise "/beliefstate/perceive_action" "knowledge_msgs/PerceivedObject")))

;Hier müssen wir nochmal rüber gehen stimmt noch nicht ganz und konnte nicht getestet werden.
(defun publish-pose (pose)
  (when *percieved-topic*
     (roslisp:publish *marker-publisher*
                      (roslisp:make-message "knowledge_msgs/PercievedObject"
                                            ;object-label?
                                            (roslisp:make-message "geometry_msgs/PoseStamped"
                                            (std_msgs-msg:stamp header)
                                            (roslisp:ros-time)
                                            (std_msgs-msg:frame_id header)
                                            (typecase pose
                                              (cl-tf:pose-stamped (cl-tf:frame-id pose)))
                                            (x position pose) (cl-transforms:x point)
                                            (y position pose) (cl-transforms:y point)
                                            (z position pose) (cl-transforms:z point)
                                            (x orientation pose) (cl-transforms:x quaternion)
                                            (y orientation pose) (cl-transforms:y quaternion)
                                            (z orientation pose) (cl-transforms:z quaternion)
                                            (w orientation pose) (cl-transforms:w quaternion))))))












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
