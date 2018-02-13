(in-package :planning-objects)

(defvar pose-message)
(defvar *marker-publisher* nil)
(defvar *marker-id* 0)

(defvar *last-middle-point-x-1* 9.0)
(defvar *last-middle-point-x-2* 9.0)
(defvar *last-middle-point-x-3* 9.0)
(defvar *last-middle-point-x-4* 9.0)

(defun calculate-landing-zone (object)
  (let ((landing-zone-message
          (planning-knowledge::ask-knowledge-where-belongs object)))
    (roslisp:with-fields ((width (storage_place_width))
                          (height (storage_place_height))
                          (position (storage_place_position)))
        landing-zone-message
      (setf width (- width 0.1))
      (setf height (- height 0.1))
      ;;(shrink-landing-zone)
      (split-landing-zone position width height))))

;(defun shrink-landing-zone
;  (setf *width* (- *width* 0.1))
;  (setf *height* (- *height* 0.1)))

(defun split-landing-zone (position width height)
  (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp:ros-error "Objects" "Out of storage space!")))
  (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point))
                        (y (geometry_msgs-msg:y geometry_msgs-msg:point))
                        (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
      position
    (incf *marker-id*)
    (let ((last-middle-point-current))
      (case y
        (1.13063d0 (setf last-middle-point-current *last-middle-point-x-1*))
        (0.75563d0 (setf last-middle-point-current *last-middle-point-x-2*))
        (0.38063d0 (setf last-middle-point-current *last-middle-point-x-3*))
        (0.00563d0 (setf last-middle-point-current *last-middle-point-x-4*)))
      (if (< last-middle-point-current 1.40494)
          (cpl:fail 'planning-error::objects-error :message "Out of storage space!"))
      (if (= last-middle-point-current 9.0)
          (setf x (+ x (/ height 2)))
          (setf x last-middle-point-current))
      (let* ((random-height (+ 0.10 (random 0.06)))
             (middle-point-landing-zone-x (- x (/ random-height 2.0)))
             (middle-point-landing-zone-pose (cl-tf:make-pose-stamped
                                              "/map"
                                              0.0
                                              (cl-transforms:make-3d-vector middle-point-landing-zone-x y z)
                                              (cl-transforms:make-quaternion 0 0 0 1))))
        
        (setf last-middle-point-current (- middle-point-landing-zone-x (/ random-height 2.0)))
        
        (case y
          (1.13063d0 (setf *last-middle-point-x-1* last-middle-point-current))
          (0.75563d0 (setf *last-middle-point-x-2* last-middle-point-current))
          (0.38063d0 (setf *last-middle-point-x-3* last-middle-point-current))
          (0.00563d0 (setf *last-middle-point-x-4* last-middle-point-current)))
        (vis-init)
        (publish-pose middle-point-landing-zone-pose *marker-id* random-height width))))))
    



;; Dependencies: visualization_msgs - visualization_msg-msg
;;               std_msgs - std_msgs-msg
;;               cl_transforms - cl-transforms
;;               cl_tf - cl-tf

;(setf pose-message (roslisp:make-message "geometry_msgs/Pose" 
;  (position) (roslisp:make-msg "geometry_msgs/Point" (x) 3)
;  (orientation) (roslisp:make-msg "geometry_msgs/Quaternion" (w) 1)))

(defun vis-init ()
  (setf *marker-publisher*
        (roslisp:advertise "~location_marker" "visualization_msgs/Marker")))

(defun publish-pose (pose id height width)
  (let ((point (cl-transforms:origin pose))
        (rot (cl-transforms:orientation pose))
        (current-index 0)
        (red (random 1.0))
        (green (random 1.0))
        (blue (random 1.0)))
    (when *marker-publisher*
      (roslisp:publish *marker-publisher*
                       (roslisp:make-message "visualization_msgs/Marker"
                                             (std_msgs-msg:stamp header) 
                                             (roslisp:ros-time)
                                             (std_msgs-msg:frame_id header)
                                             (typecase pose
                                               (cl-tf:pose-stamped (cl-tf:frame-id pose))
                                               (t "map"))
                                             ns "goal_locations"
                                             id (or id (incf current-index))
                                             type (roslisp:symbol-code
                                                   'visualization_msgs-msg:<marker> :cube)
                                             action (roslisp:symbol-code
                                                     'visualization_msgs-msg:<marker> :add)
                                             (x position pose) (cl-transforms:x point)
                                             (y position pose) (cl-transforms:y point)
                                             (z position pose) (cl-transforms:z point)
                                             (x orientation pose) (cl-transforms:x rot)
                                             (y orientation pose) (cl-transforms:y rot)
                                             (z orientation pose) (cl-transforms:z rot)
                                             (w orientation pose) (cl-transforms:w rot)
                                             (x scale) height
                                             (y scale) width
                                             (z scale) 0.02
                                             (r color) red
                                             (g color) green
                                             (b color) blue
                                             (a color) 1.0)))))

;; Example usage:
;; (vis-init)
;; (publish-pose (cl-tf:make-identity-pose))





  
;;    (landing-zone-small (roslisp:modify-message-copy landing-zone



    
;;    (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)) 
;;                          (y (geometry_msgs-msg:y geometry_msgs-msg:point))
;;                          (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
;;        (knowledge_msgs-srv:storage_place_position
;;         (height (storage_place_height))
;;         (width (storage_place_width))
;;         (knowledge_msgs-srv:storageplace response))
;;      (= height (- height 0.1))
;;      (= width (- height 0.1))
      

