(in-package :planning-logic)
(defvar *pont*)

(defun transformation (point-center-of-object &optional (endFrame "/base_footprint"))
  "transform a msgs with an optional Frame, default is base_footprint"
  
  (roslisp:with-fields ((x
                         (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                        (y
                         (geometry_msgs-msg:y geometry_msgs-msg:point))
                        (z
                         (geometry_msgs-msg:z geometry_msgs-msg:point))
                        (startFrame
                         (STD_msgs-msg:frame_id geometry_msgs-msg:header)))
      (object_detection-msg:position
       (object_detection-srv:object point-center-of-object))
    (let
        ((transform-listener
           (make-instance 'cl-tf:transform-listener))
         (tf-point-stamped
           (cl-tf:make-point-stamped startFrame 0.0
                                     (cl-transforms:make-3d-vector x y z))))
      (catch-Transform transform-listener tf-point-stamped endFrame))))



(defun catch-Transform (transform-listener tf-point-stamped endFrame)
  "transform-listener catch transformation (helpfunction)"
  (sleep 5.0)
  (cl-tf:transform-point transform-listener
                         :point tf-point-stamped
                         :target-frame endFrame))


(defun can-Robo-Poke ()
  "mit mathe herrausfinden warum nicht gepoked werden kann oder ob er"
  (setf *pont* (planning-vision::call-vision-point))
  (let ((pointTransformed(planning-logic::transformation *pont*)))
    (roslisp:with-fields (y) pointTransFormed
      (progn
        (if (> y 0)
            (planning-motion::
             call-Motion-Move-To-Point *pont* 3)
            (planning-motion::
             call-Motion-Move-To-Point *pont* 2))))))
                                                     
        
  
