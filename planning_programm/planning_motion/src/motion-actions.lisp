(in-package :planning-motion)
(defvar status-message nil)

(defun call-Motion-Move-Arm-Homeposition (&optional (x 1))
  "Moves robot-arms into home position"
  (roslisp::ros-info "Motion" "getting into home position")
  (cpl:with-retry-counters ((retry-counter 2))
    (cpl:with-failure-handling
        (((or cpl:simple-plan-failure planning-error::motion-error) (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
                                        ;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))

           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")
         (return-from call-Motion-Move-Arm-Homeposition status-message)))

      
      (setf status-message
              (let ((actionclient
                      (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((xtrans
                        (cl-transforms-stamped:to-msg
                         (cl-tf:make-pose-stamped "base_link" 0
                                                  (cl-tf:make-point-stamped "base_link" 0
                                                                            (cl-transforms:make-3d-vector 5.0 3.0 1.2))
                                                  (cl-tf:make-quaternion 1 1 1 1)))))
                  (let ((actiongoal
                          (actionlib:make-action-goal actionclient goal_pose xtrans command x)))
                     (actionlib:call-goal actionclient actiongoal)))))
           (roslisp:with-fields (motion_msgs-msg:status (motion_msgs-msg:movingcommandresult) status)
            status-message
          (case motion_msgs-msg:status
            (0 (roslisp::ros-info "Motion" "Successfully moved into home position."))
            (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
             (cpl:fail 'planning-error::motion-error :message "Goal is out of range."))
            (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
             (cpl:fail 'planning-error::motion-error :message "Path to goal is obstructed."))
            (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
             (cpl:fail 'planning-error::motion-error :message "Unmanageable error occured in motion!")))))))
    













(Defun call-Motion-Move-Arm-To-Point (point-center-of-object label &optional (x 3))
  "Moves choosen robot-arm (optional parameter) to the point-center-of-object (default right arm 3=right, 2=left)"
  (roslisp::ros-info "Motion" "moving arm to point")
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        (((or cpl:simple-plan-failure planning-error::motion-error)(error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
           ;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")
      (return-from call-Motion-Move-Arm-To-Point status-message)))
      (setf status-message
              (let ((actionclient 
                      (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((actiongoal
                        (actionlib:make-action-goal actionclient goal_pose point-center-of-object command x grasped_object_label label)))
                  (actionlib:call-goal actionclient actiongoal))))
             (roslisp:with-fields (motion_msgs-msg:status (motion_msgs-msg:movingcommandresult status))
            status-message
              (case motion_msgs-msg:status
                (0 (roslisp::ros-info "Motion" "Successfully moved into position."))
                (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
                 (cpl:fail 'planning-error::motion-error :message "Goal is out of range."))
                (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
                 (cpl:fail 'planning-error::motion-error :message "Path to goal is obstructed."))
                (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
                 (cpl:fail 'planning-error::motion-error :message "Unmanageable error occured in motion!")))))))

(defun toggle-gripper (effort &optional (gripper 1) (position 0.008))
  "Uses sound_play service to let the pr2 say a string out loud"
  (let ((actionclient (actionlib:make-action-client "/gripper" "motion_msgs/GripperAction")))
    (let
        ((actiongoal
      (actionlib:make-action-goal actionclient position position force effort gripper gripper)))
      (actionlib:wait-for-server actionclient 5.0)
      (actionlib:call-goal actionclient actiongoal))))

