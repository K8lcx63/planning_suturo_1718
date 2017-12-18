(in-package :planning-main-programm)


(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (init-variables)
  (roslisp:with-ros-node ("planning_main")
    (if (planning-move::find-Object 1.0 0.5)
        (progn
          (planning-motion::call-Motion-Move-Arm)
          (let ((point-for-motion
                  (let ((point-center-of-object
                          (planning-vision::call-vision-point)))
                    (planning-knowledge::ask-knowledge point-center-of-object))))
            (planning-motion::call-Motion-Move-To-Point point-for-motion)))
        (return-from main  (roslisp::ros-info "Main" "Sorry i couldnt find any object :c")))))


(defun init-variables ()
  "initialize internal variables"
  (setf *x* 1.0)
  (setf *z* 0.5))

