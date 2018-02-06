(in-package :planning-main)

(defvar *number-for-arm* nil)
(defvar *point-center-of-object* nil)

(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (roslisp:with-ros-node ("planning_main")
    (roslisp:ros-info (main)
                      "Robotlife seems hard, but lets do this")
    (planning-move::init-Robo-Moving)
    (loop for counter from 5 downto 1
          do (progn
               (if (planning-move::find-Object 1.0 0.5 objectString)
                   (progn
                                        ; (block check-for-valid-point 
                                        ;  (loop for counter from 10 downto 1
                                        ;       do (progn
                     (let ((point-for-motion
                             (progn
                               (let ((point-center-of-object
                                       (progn
                                         (roslisp:ros-info (main)
                                                           "getting point-ceter-of-object from vision")
                                         (setf *point-center-of-object*(planning-vision::call-Vision-Point)))))
                                 (roslisp:ros-info (main)
                                                   "asking knowledge where to poke")
                                 (setf *number-for-arm*(planning-logic::should-Robo-Use-Left-Or-Right-Arm *point-center-of-object*))
                                 (planning-knowledge::ask-Knowledge-For-Poke-Point point-center-of-object)))))                     
                                        ; (if (planning-vision::check-Points-Is-Equal
                                        ; (planning-vision::call-Vision-Point) *point-center-of-object* 0.2)
                                        ; (progn
                                        ;  (roslisp:ros-info (main)
                                        ;                   "i think point is still valid..")
                       (planning-logic::try-To-Poke-Different-Location point-for-motion *number-for-arm*)
                       (return-from main (roslisp:ros-info (main)
                              "object should be touched now"))))
                                        ; (return-from check-for-valid-point))))))
                                        ;       (progn
                                        ;        (roslisp:ros-info (main)
                                        ;                         "I suppse, that the Object moved. Will check if its still there.")
                                        ;      (if (planning-move::find-Object 1.0 0.5 )
                                        ;         ()
                                        ;        (progn
                                        ;         (roslisp::ros-info (main)
                                        ;                           "I cannot find the Object anymore.")
                                        ;       (return-from check-for-valid-point)))))
                                        ;  (roslisp::ros-info (main)
                                        ;                   "Object seems to be in my line of sight now, will look for exact position again now."))
                   
                   (roslisp::ros-info (main)
                                      "Sorry i couldnt find any object, but i'll try again! "))))
    (return-from main (roslisp:ros-info (main)
                                        "I really cant find any object im sry"))))
