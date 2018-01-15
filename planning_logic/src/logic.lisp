(in-package :planning-logic)
(defvar *pont*)
(defvar *transform-listener*)
(defvar *tf-point-stamped*)


(defun transformation (&optional (endFrame "base_footprint"))
  "transformiert zu dem gegeben frame, deualt base_footprint"

  (setf *pont* (planning-vision::call-vision-point))
 (roslisp:with-fields ((x
                        (geometry_msgs-msg:x geometry_msgs-msg:point)) 
                       (y
                        (geometry_msgs-msg:y geometry_msgs-msg:point))
                       (z
                        (geometry_msgs-msg:z geometry_msgs-msg:point))
                       (startFrame
                        (STD_msgs-msg:frame_id geometry_msgs-msg:header)))
     (object_detection-msg:position
      (object_detection-srv:object *pont*))
   (let
       ((transform-listener
          (make-instance 'cl-tf:transform-listener))
        (tf-point-stamped
          (cl-tf:make-point-stamped startFrame 0.0
                                    (cl-transforms:make-3d-vector x y z))))
     (cl-tf:transform-point transform-listener :point tf-point-stamped :target-frame endFrame))))

(defvar *poke-point*)
(defvar *transform-listener*)
(defvar *vec3d*)
(defvar *tf-point-stamped*)

(defun dummytf (&optional (endFrame "base_footprint"))
  "transformiert zu dem gegeben frame, deualt base_footprint"
 (setf *poke-point* (planning-vision::call-vision-point))
  (setf *transform-listener* (make-instance 'cl-tf:transform-listener))
  (loop until (sleep 20))

  
   (roslisp:with-fields (object) *poke-point* (setf *poke-point* object))
   (roslisp:with-fields (position) *poke-point* (setf *poke-point* position))
(roslisp:with-fields (point) *poke-point* (setf *poke-point* point))
   (setf *vec3d* (roslisp:with-fields(x) *poke-point* (roslisp:with-fields(y) *poke-point* (roslisp:with-fields(z) *poke-point*(cl-transforms:make-3d-vector x y z)))))
  (setf *tf-point-stamped* (cl-tf:make-point-stamped "/head_mount_kinect_ir_optical_frame" 0.0 *vec3d*))
(cl-tf:transform-point *transform-listener* :point *tf-point-stamped* :target-frame endFrame))
