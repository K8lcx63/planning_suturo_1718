(in-package :planning-knowledge)
 
(defvar *percieved-topic* nil)

(defun what-object(features)
  "Takes set of features to knowledge, and asks for identification"
  (roslisp:call-service "/svm_classifier/classify_service" 'knowledge_msgs-srv:Classify :features features))

(defun is-object(objectString features)
  "Takes features and string and asks knowledge if features are the object described in string"
  (roslisp:with-fields (label) (what-object features) (if (string= label objectString)(return-from is-object T)(return-from is-object nil))))



(defun ask-Knowledge-How-To-Grab (object)
   (roslisp:call-service "/knowledge_grasp/knowledge_grasp" 'knowledge_msgs-srv:GraspIndividual :object_label object))

(defun ask-knowledge-where-belongs (object)
  (roslisp:call-service "/storage_place_service/storage_place" 'knowledge_msgs-srv:StoragePlace :object_label object))

;(defun place-object (gripper frame-id x-coordinate y-coordinate)
;  (roslisp:call-service "/place_object/place" 'knowledge_msgs-srv:PlaceObject :gripper gripper :frame_id frame-id :x_coordinate x-coordinate :y_coordinate y-coordinate))

(defun objects-To-Pick ()
  (roslisp:call-service "/beliefstate/objects_to_pick" 'knowledge_msgs-srv:ObjectsToPick))

(Defun how-To-Pick-Objects (object) 
    (roslisp:call-service "/knowledge_grasp/knowledge_grasp" 'knowledge_msgs-srv:GraspIndividual :object_label object))

(Defun empty-Gripper () 
    (roslisp:call-service "/beliefstate/gripper_empty" 'knowledge_msgs-srv:EmptyGripper))





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

