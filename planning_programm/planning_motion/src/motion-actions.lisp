(in-package :planning-motion)

(defun motion-Home ()
 "gibt die funktion mit true or false aus diese Dient erstmal als basic konstrukt und wird später noch genauer bearbeitet wenn die Funktioin drum herrum entstanden ist."
(let ((result
        (planning-motion::call-Motion-Move-Arm-Homeposition)))
  (roslisp:with-fields(successful) result (print successful))))

(defun motion-To-Point (point-center-Of-object &optional (x 3))
 "gibt die funktion mit true or false aus diese Dient erstmal als basic konstrukt und wird später noch genauer bearbeitet wenn die Funktioin drum herrum entstanden ist.Optional kann man von dem linken zum rechten Arm wechseln 3=links 2=rechts"
(let ((result
        (planning-motion::call-Motion-Move-Arm-To-Point point-center-of-object x)))
  (roslisp:with-fields(successful) result (return-from motion-To-Point successful))))
        
(defun call-Motion-Move-Arm-Homeposition()
  "Moves robot-arms into home position"
   (roslisp::ros-info "Motion" "getting into home position")
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
           ;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")))
      (let ((status-message
              (let ((actionclient 
                      (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((xtrans
                        (cl-transforms-stamped:to-msg
                         (cl-tf:make-point-stamped "base_link" 0 
                                                   (cl-transforms:make-3d-vector 5.0 3.0 1.2)))))
                  (let ((actiongoal 
                          (actionlib:make-action-goal actionclient point_stamped xtrans command 1)))
                    (actionlib:call-goal actionclient actiongoal))))))
        (roslisp:with-fields (motion_msgs-msg:status (motion_msgs-msg:movingcommandresult status))
            status-message
              (case motion_msgs-msg:status
                (0 (roslisp::ros-info "Motion" "Successfully moved into home position."))
                (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
                 (cpl:fail 'planning-error::motion-error :message "Goal is out of range."))
                (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
                 (cpl:fail 'planning-error::motion-error :message "Path to goal is obstructed."))
                (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
                 (cpl:fail 'planning-error::motion-error :message "Unmanageable error occured in motion!"))))))))

		


(defun call-Motion-Move-Arm-To-Point (point-center-of-object &optional (x 3))
  "Moves choosen robot-arm (optional parameter) to the point-center-of-object (default right arm 3=right, 2=left)"
  (roslisp::ros-info "Motion" "moving arm to point")
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
           ;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")))
      (let ((status-message
              (let ((actionclient 
                      (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((actiongoal
                        (actionlib:make-action-goal actionclient point_stamped point-center-of-object command x)))
                  (actionlib:call-goal actionclient actiongoal)))))
             (roslisp:with-fields (motion_msgs-msg:status (motion_msgs-msg:movingcommandresult status))
            status-message
              (case motion_msgs-msg:status
                (0 (roslisp::ros-info "Motion" "Successfully moved into position."))
                (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
                 (cpl:fail 'planning-error::motion-error :message "Goal is out of range."))
                (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
                 (cpl:fail 'planning-error::motion-error :message "Path to goal is obstructed."))
                (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
                 (cpl:fail 'planning-error::motion-error :message "Unmanageable error occured in motion!"))))))))

