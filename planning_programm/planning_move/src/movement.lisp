(in-package :planning-move)

(defvar *beliefstateHead* 0)
(defvar *headMovementList* '((0 -0.8)(1 -0.6)(2 -0.3)(3 0.0)(4 0.3)(5 0.6)(6 0.8)(7 0.6)(8 0.3)(9 0.0)(10 -0.3)(11 -0.6)))

(defun move-Head (x y z)
  "Moving robot head via head_traj_controller/point_head_action. X Y Z are treated as coordinates."
  (roslisp::ros-info "Motion" "Moving head")
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
           ;;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")))
      (let ((status-message
              (let ((actionclient 
                      (actionlib:make-action-client "head_traj_controller/point_head_action" "pr2_controllers_msgs/PointHeadAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((point-to-look-at 
                        (cl-transforms-stamped:to-msg 
                         (cl-transforms-stamped:make-point-stamped "base_link" 0 
                                                                   (cl-transforms:make-3d-vector x y z)))))
                  (let ((actiongoal 
                          (actionlib:make-action-goal actionclient target point-to-look-at)))
                    (actionlib:call-goal actionclient actiongoal))))))
        (roslisp:with-fields (status (motion_msgs-msg:movingcommandresult motion_msgs-msg:status)) status-message
          (case status
            (0 (roslisp::ros-info "Motion" "Successfully moved head."))
            (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
             (cpl:fail))
            (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
             (cpl:fail))
            (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
             (cpl:fail))))))))

(defun move-Base-To-Point (x y z w)
  "Moving robot base via nav_pcontroller/move_base. X Y Z are treated as coordinates. W for Orientation."
  (roslisp::ros-info "Motion" "Moving base to point.")
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (roslisp::ros-info "Motion" "Trying to solve error.")
           ;;Here is the place for solutions to the problem!
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (roslisp::ros-info "Motion" "Now retrying ...")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")
           (roslisp::ros-error "Motion" "Reached maximum amount of retries. Now propagating the failure up.~%")))
      (let ((status-message
              (let ((actionclient 
                      (actionlib:make-action-client "nav_pcontroller/move_base" "move_base_msgs/MoveBaseAction")))
                (loop until
                      (actionlib:wait-for-server actionclient))
                (let ((pose-to-drive-to 
                        (cl-transforms-stamped:to-msg 
                         (cl-transforms-stamped:make-pose-stamped "base_link" 0 
                                                                  (cl-transforms:make-3d-vector x y z) (cl-tf:make-quaternion x y z w)))))
                  (let ((actiongoal 
                          (actionlib:make-action-goal actionclient target_pose pose-to-drive-to)))
                    (actionlib:call-goal actionclient actiongoal))))))
        (roslisp:with-fields (status (motion_msgs-msg:movingcommandresult motion_msgs-msg:status)) status-message
          (case status
            (0 (roslisp::ros-info "Motion" "Successfully moved base to point."))
            (1 (roslisp::ros-warn "Motion" "Goal is out of range.")
             (cpl:fail))
            (2 (roslisp::ros-warn "Motion" "Path to goal is obstructed.")
             (cpl:fail))
            (3 (roslisp::ros-error "Motion" "Unmanageble error occured in motion!")
             (cpl:fail))))))))


(defun find-Object (x z objectString)
  "Looking around from 0-2 and 0-(-2) to find an object, restarting at current Point if reused"
  (block find-Object-Start 
    (loop for i from *beliefstateHead* to 24 do
      (let ((c (mod i 12)))
        (progn
          (move-Head x (second (assoc c *headMovementList*)) z)
          (setf *beliefstateHead* c)
          (if (planning-knowledge::is-Object i)
              (return-from find-Object-Start)
              ))))
    (roslisp::ros-info "find-Object" "Couldnt find any Object in front of me")
    (return-from find-Object nil))
  (roslisp::ros-info "find-Object" "I see the Object. Head is in Position")
  (return-from find-Object T))


  

  
