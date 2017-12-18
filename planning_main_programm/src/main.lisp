(in-package :planning-main-programm)

(defvar *point-center-of-object*)
(defvar *x* 1.0)
(defvar *y* 0.0)
(defvar *z* 0.5)



(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (init-variables)
  (roslisp:with-ros-node ("planning_main")
    (roslisp::ros-info "Main" "Robotlife seems hard, but lets do this")
    (planning-move::move-Head *x* *y* *z*)
    (roslisp::ros-info "Main" "Moving into homeposition")
    (planning-motion::call-Motion-Move-Arm)
    (let ((counter 10))
      (block check-for-valid-point 
        (loop while (>= counter 0)
              do (progn
                   (let ((point-for-motion
                            (let ((point-center-of-object
                                    (progn
                                      (roslisp::ros-info "Main" "Calling Vision for center point of object")
                                      (planning-vision::call-vision-point))))
                              (progn
                                (setf *point-center-of-object* point-center-of-object)
                                (roslisp:ros-info "Main" "Asking knowledge where to poke")
                                (planning-knowledge::ask-knowledge point-center-of-object)))))
                      (if (planning-vision::check-points-is-equal (planning-vision::call-vision-point) *point-center-of-object* 0.2)
                          (progn
                            (roslisp::ros-info "Main" "I think object position is still valid")
                            (planning-motion::call-Motion-Move-To-Point point-for-motion)
                            (return-from check-for-valid-point))
                          )
                         (roslisp::ros-info "Main" "Object position doesnt seem to be valid anymore, i will check again")
                    )
                   )
              )
        )
      )
    )
  )
  


(defun init-variables ()
  "initialize internal variables"
  (setf *x* 1.0)
  (setf *y* 0.0)
  (setf *z* 0.5))



