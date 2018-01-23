(in-package :planning-main)

(defvar *point-center-of-object* nil)

(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (roslisp:with-ros-node ("planning_main")
    (roslisp::ros-info "Main" "Robotlife seems hard, but lets do this")
    (planning-motion::call-Motion-Move-Arm)
    (if (planning-move::find-Object 1.0 0.5 "tolles Object")
        (progn
          (block check-for-valid-point 
            (loop for counter from 10 downto 1
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
                               (roslisp::ros-info "Main" "Object should be touched now")
                               (return-from check-for-valid-point))
                             (progn
                               (roslisp::ros-info "Main" "I suppose, that the Object moved. Will check if its still there.")
                               (if (planning-move::find-Object 1.0 0.5 "tolles Object")()(progn
                                                                                           (roslisp::ros-info "Main" "I cannot find the Object anymore.")
                                                                                           (return-from check-for-valid-point))))
                             )
                         (roslisp::ros-info "Main" "Object seems to be in my line of sight now, will look for exact position again now.")))))))
    (return-from main  (roslisp::ros-info "Main" "Sorry i couldnt find any object :c"))))



