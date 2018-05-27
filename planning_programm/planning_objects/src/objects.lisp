(in-package :planning-objects)


;; ############################
;; ###   GLOBAL VARIABLES   ###
;; ############################


;; Publisher for publishing markers where objects will be placed
(defvar *marker-publisher* nil)

;; ID for unique markers
(defvar *marker-id* 10)

;; Y-coordinate of the last placed object in storage place number
(defvar *last-y-border-y-1* 9.0)
(defvar *last-y-border-y-2* 9.0)
(defvar *last-y-border-y-3* 9.0)
(defvar *last-y-border-y-4* 9.0)

;; Last placed object label
(defvar *current-object-label*)

;; Object label of object 1 or 2 in landing zone 1 to 4
(defvar *object-label-1-lz-1* nil)
(defvar *object-label-2-lz-1* nil)
(defvar *object-label-1-lz-2* nil)
(defvar *object-label-2-lz-2* nil)
(defvar *object-label-1-lz-3* nil)
(defvar *object-label-2-lz-3* nil)
(defvar *object-label-1-lz-4* nil)
(defvar *object-label-2-lz-4* nil)

(defvar *current-storage-place-number*)

;; Message which tells the receiver of the landing zone if the storage place has space for another object
(defvar *storage-place-capacity* "storage-place-empty")


;; ##########################
;; ###   CORE FUNCTIONS   ###
;; ##########################


;; Asks Knowledge where the object belongs and crops the landing zone. The object must be spelled correctly or an error will be thrown by the function check-storage-place-spelling.
;; fill-landing-zone-horizontally will be used to give the object its coordinates within the landing zone.
;;
;; @input  string object                             - object lable
;; @input  knowledge_msgs/gripper gripper            - number of the gripper which holds the object, 1 for left 2 for right
;; @output list (geometry_msgs/PoseStamped , string) - list of a pose where the object should be placed and a string that says "storage-place-empty" or "storage-place-full". "storage-place-empty" means that there is more space for one or more objects.

(defun calculate-landing-zone (object gripper)
  (cpl:with-failure-handling
      (((or cpl:simple-plan-failure planning-error::objects-error) (error-object)
         (format t "An error happened: ~a~%" error-object)))

    (if (not (check-storage-place-spelling object))
        (cpl:fail 'planning-error::objects-error :message "Misspelled or non existent object-lable!"))
    
    (setf *current-object-label* object)
    (let ((landing-zone-message
            (planning-knowledge::ask-knowledge-where-belongs object)))
      (roslisp:with-fields ((width (storage_place_width))
                            (height (storage_place_height))
                            (position (storage_place_position)))
          landing-zone-message
        (setf width (- width 0.03))
        (setf height (- height 0.3))
        (let ((middle-point-landing-zone-pose
                (cl-tf:to-msg (fill-landing-zone-horizontally position width height)))
              (gripper-msg (roslisp:make-message "knowledge_msgs/gripper" :gripper gripper)))
          (roslisp:with-fields ((exact-landing-x
                                 (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose))
                                (exact-landing-y
                                 (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose))) middle-point-landing-zone-pose
            (let ((landing-pose-message
                    (planning-knowledge::place-object gripper-msg "/map" exact-landing-x exact-landing-y)))
              (return-from calculate-landing-zone (list landing-pose-message *storage-place-capacity*)))))))))


;; Checks if the object-label is one of the following
;; "HelaCurryKetchup", "TomatoSauceOroDiParma", "PringlesPaprika", "PringlesSalt", "JaMilch", "KoellnMuesliKnusperHonigNuss", "KellogsToppasMini", "CupEcoOrange", "EdekaRedBowl" and "SiggBottle".
;; If it is one of them and correctly spelled with capitalization this function returns true.
;; If it does not match it will return false.

;; @input string object-label - name of the object
;; @output bool               - true if the name of the object matches one in the list, otherwise false

(defun check-storage-place-spelling (object-label)
  (loop for x in '("HelaCurryKetchup" "TomatoSauceOroDiParma" "PringlesPaprika" "PringlesSalt" "JaMilch" "KoellnMuesliKnusperHonigNuss" "KellogsToppasMini" "CupEcoOrange" "EdekaRedBowl" "SiggBottle")
        do (if (string= object-label x)
               (return t))))


;; Places one more object in the landing zone. Throws an error if the storage place already contains two or more objects.
;;
;; @input geometry_msgs/PoseStamped position - position of the new object
;; @input float32 width                      - width of the landing zone, 1.0 is one meter
;; @input float32 length                     - length of the landing zone, 1.0 is one meter

(defun fill-landing-zone-horizontally (position width height)
  (cpl:with-failure-handling
      (((or cpl:simple-plan-failure planning-error::objects-error) (error-object)
         (format t "An error happened: ~a~%" error-object)))
    
    (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point))
                          (y (geometry_msgs-msg:y geometry_msgs-msg:point))
                          (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
        position
      (let ((last-y-border)
            (current-y-border)
            (width-split (/ width 2.0))
            (height-split (/ height 2.0)))

        (cond ((>= y (- 1.13063 width-split))
               (progn
                 (setf last-y-border *last-y-border-y-1*)
                 (setf *current-storage-place-number* 1)))
              ((>= y (- 0.75563 width-split))
               (progn
                 (setf last-y-border *last-y-border-y-2*)
                 (setf *current-storage-place-number* 2)))
              ((>= y (- 0.38063 width-split))
               (progn
                 (setf last-y-border *last-y-border-y-3*)
                 (setf *current-storage-place-number* 3)))
              ((>= y (- 0.00563 width-split))
               (progn
                 (setf last-y-border *last-y-border-y-4*)
                 (setf *current-storage-place-number* 4))))

        ; + 0.2 to tell when there are 2 objects in one storage place.
        (if (<= last-y-border (+ (- y width-split) 0.2))
            (setf *storage-place-capacity* "storage-place-full")
            (setf *storage-place-capacity* "storage-place-empty"))

        (if (<= last-y-border (- y width-split))
            (cpl:fail 'planning-error::objects-error :message "Out of storage space!"))
        
        (cond ((= *current-storage-place-number* 1)
               (if (not *object-label-1-lz-1*)
                   (setf *object-label-1-lz-1* *current-object-label*)
                   (setf *object-label-2-lz-1* *current-object-label*)))
              ((= *current-storage-place-number* 2)
               (if (not *object-label-1-lz-2*)
                   (setf *object-label-1-lz-2* *current-object-label*)
                   (setf *object-label-2-lz-2* *current-object-label*)))
              ((= *current-storage-place-number* 3)
               (if (not *object-label-1-lz-3*)
                   (setf *object-label-1-lz-3* *current-object-label*)
                   (setf *object-label-2-lz-3* *current-object-label*)))
              ((= *current-storage-place-number* 4)
               (if (not *object-label-1-lz-4*)
                   (setf *object-label-1-lz-4* *current-object-label*)
                   (setf *object-label-2-lz-4* *current-object-label*))))
        
        (if (= last-y-border 9.0)
            (setf last-y-border (+ y width-split)))
        (let* ((random-height (random height-split))
               (landing-zone-width (/ width 2.0))
               (current-middle-point-x (- x random-height))
               (current-middle-point-y (- (- last-y-border (/ landing-zone-width 2.0)) 0.05))
               (landing-zone-pose (cl-tf:make-pose-stamped
                                   "/map"
                                   0.0
                                   (cl-transforms:make-3d-vector current-middle-point-x current-middle-point-y z)
                                   (cl-transforms:make-quaternion 0 0 0 1))))
          
          (setf current-y-border (- last-y-border landing-zone-width))
          
          (cond ((>= y (- 1.13063 width-split))
                 (setf *last-y-border-y-1* current-y-border))
                ((>= y (- 0.75563 width-split))
                 (setf *last-y-border-y-2* current-y-border))
                ((>= y (- 0.38063 width-split))
                 (setf *last-y-border-y-3* current-y-border))
                ((>= y (- 0.00563 width-split))
                 (setf *last-y-border-y-4* current-y-border)))
          (return-from fill-landing-zone-horizontally landing-zone-pose))))))


;; Pushes both objects of the last filled storage place away to free up space for new objects.
;; In between pushes the robot will move into the home position to avoid collisions.
;; If the storage place is not full an error will be thrown.

(defun push-object ()
  (cpl:with-failure-handling
      (((or cpl:simple-plan-failure planning-error::objects-error) (error-object)
         (format t "An error happened: ~a~%" error-object)))

    (let ((push-pose)
          (gripper)
          (empty-gripper-msg (planning-knowledge::empty-gripper)))

      ;finds out which gripper is free and writes it in "gripper". 5 left 4 right
      (roslisp:with-fields ((left-gripper (left_gripper))
                            (right-gripper (right_gripper)))
          empty-gripper-msg
        (if left-gripper
            (setf gripper 5))
        (if right-gripper
            (setf gripper 4)))

      ;open gripper
      (planning-motion::toggle-gripper 20 gripper 0.08)

      (case *current-storage-place-number*
        (1
         (if (or (not *object-label-1-lz-1*) (not *object-label-2-lz-1*))
             (cpl:fail 'planning-error::objects-error :message "The storage place is not full!"))
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-1-lz-1*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-1-lz-1* gripper)
         (planning-motion::call-motion-move-arm-homeposition)
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-2-lz-1*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-2-lz-1* gripper)
         (setf *last-y-border-y-1* 9.0))
        (2
         (if (or (not *object-label-1-lz-2*) (not *object-label-2-lz-2*))
             (cpl:fail 'planning-error::objects-error :message "The storage place is not full!"))
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-1-lz-2*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-1-lz-2* gripper)
         (planning-motion::call-motion-move-arm-homeposition)
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-2-lz-2*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-2-lz-2* gripper)
         (setf *last-y-border-y-2* 9.0))
        (3
         (if (or (not *object-label-1-lz-3*) (not *object-label-2-lz-3*))
             (cpl:fail 'planning-error::objects-error :message "The storage place is not full!"))
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-1-lz-3*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-1-lz-3* gripper)
         (planning-motion::call-motion-move-arm-homeposition)
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-2-lz-3*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-2-lz-3* gripper)
         (setf *last-y-border-y-3* 9.0))
        (4
         (if (or (not *object-label-1-lz-4*) (not *object-label-2-lz-4*))
             (cpl:fail 'planning-error::objects-error :message "The storage place is not full!"))
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-1-lz-4*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-1-lz-4* gripper)
         (planning-motion::call-motion-move-arm-homeposition)
         (roslisp:with-fields (push_pose) (planning-knowledge::push-object *object-label-2-lz-4*) (setf push-pose push_pose))
         (planning-motion::call-motion-move-arm-to-point push-pose *object-label-2-lz-4* gripper)
         (setf *last-y-border-y-4* 9.0)))
      (setf *storage-place-capacity* "storage-place-empty"))))


;; #########################
;; ###   VISUALIZATION   ###
;; #########################


;; Works like calculate-landing-zone. Additionally a marker will be published at the position where the object should be placed.
;;
;; @input  string object                             - object lable
;; @input  knowledge_msgs/gripper gripper            - gripper number, 1 for left 2 for right
;; @output list (geometry_msgs/PoseStamped , string) - list of a pose where the object should be placed and a string that says "storage-place-empty" or "storage-place-full". "storage-place-empty" means that there is more space for one or more objects.

(defun calculate-landing-zone-visualized (object gripper)
  (let ((landing-zone-pose (calculate-landing-zone object gripper)))
    (visualize-landing-zone landing-zone-pose)
    (incf *marker-id*)
    (return-from calculate-landing-zone-visualized landing-zone-pose)))

;; Main function of visualiation which calls sub functions to publish markers of the landing zone.
;;
;; @input geometry_msgs/PoseStamped landing-zone-pose - pose where the marker should be spawned

(defun visualize-landing-zone (landing-zone-pose)
  (vis-init)
  (publish-pose landing-zone-pose *marker-id* 0.1 0.1))


;; Starts the marker publisher

(defun vis-init ()
  (setf *marker-publisher*
        (roslisp:advertise "~location_marker" "visualization_msgs/Marker")))

;; Publishes a marker with a random color at the given position.
;;
;; @input geometry_msgs/PoseStamped pose - pose where the marker should be placed
;; @input int id                         - unique id for the marker, to replace a marker just reuse its id
;; @input float32 height                 - height of the marker 1.0 is one meter
;; @input float32 length                 - length of the marker 1.0 is one meter         

(defun publish-pose (pose id height width)
  ;; Publiziert eine Pose als Marker
  (setf pose (nth 0 pose))
  (roslisp:with-fields ((place-pose (knowledge_msgs-srv:place_pose))) pose
    (setf pose (cl-tf:from-msg place-pose)))
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
                                             (z scale) 0.1
                                             (r color) red
                                             (g color) green
                                             (b color) blue
                                             (a color) 1.0)))))


;; #####################
;; ###   DEBUGGING   ###
;; #####################


;; Clears all landing zones of objects. This is for testing purposes.

(defun clear-all-landing-zones ()
  (setf *last-y-border-y-1* 9.0)
  (setf *last-y-border-y-2* 9.0)
  (setf *last-y-border-y-3* 9.0)
  (setf *last-y-border-y-4* 9.0)

  (setf *object-label-1-lz-1* nil)
  (setf *object-label-2-lz-1* nil)
  (setf *object-label-1-lz-2* nil)
  (setf *object-label-2-lz-2* nil)
  (setf *object-label-1-lz-3* nil)
  (setf *object-label-2-lz-3* nil)
  (setf *object-label-1-lz-4* nil)
  (setf *object-label-2-lz-4* nil)

  (setf *storage-place-capacity* "storage-place-empty"))
