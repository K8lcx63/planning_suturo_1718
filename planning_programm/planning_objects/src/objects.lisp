(in-package :planning-objects)

(defvar pose-message)
(defvar *marker-publisher* nil)
(defvar *marker-id* 0)

(defvar *last-y-border-y-1* 9.0)
(defvar *last-y-border-y-2* 9.0)
(defvar *last-y-border-y-3* 9.0)
(defvar *last-y-border-y-4* 9.0)

(defun calculate-landing-zone (object)
  (let ((landing-zone-message
          (planning-knowledge::ask-knowledge-where-belongs object)))
    (roslisp:with-fields ((width (storage_place_width))
                          (height (storage_place_height))
                          (position (storage_place_position)))
        landing-zone-message
      (width (- width 0.1))
      (height (- height 0.2))
      (split-landing-zone position width height))))


(defun fill-landing-zone-horizontally (position width height)
  (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point))
                        (y (geometry_msgs-msg:y geometry_msgs-msg:point))
                        (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
      position
    (let ((last-y-border)
          (width-split (/ width 2.0))
          (height-split (/ height 2.0))
      (if (>= y (- 1.13063 width-split))
          (setf last-y-border *last-y-border-y-1*)
          (if ((>= y (- 0.75563 width-split)))
               (setf last-y-border *last-y-border-y-2*)
               (if ((>= y (- 0.38063 width-split)))
                    (setf last-y-border *last-y-border-y-3*)
                    (if ((>= y (- 0.00563 width-split)))
                         (setf last-y-border *last-y-border-y-4*)))))
      (if (< last-y-border (- y (- width-split 0.08)))
          (cpl:fail 'planning-error::objects-error :message "Out of storage space!"))
      (if (= last-y-border 9.0)
          (setf y (+ y width-split))
          (setf y (last-y-border)))
      (let* ((random-height (random height-split))
             (x (- x random-height))
             (y (
             
           
           
           
(defun visualize-landing-zone pose)

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
      

