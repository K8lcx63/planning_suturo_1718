(in-package :planning-motion)

(defun call-Motion-Move-Arm ()
  "Moves robot-arms into home position"
  (let actionclient 
    (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")
    (loop until
          (actionlib:wait-for-server actionclient))
    (let xtrans
      ((cl-tf:make-point-msg 
        (cl-tf:make-point-stamped "base_link" 0 
                                  (cl-transforms:make-3d-vector 5.0 3.0 1.2))))
      (let actiongoal 
        (actionlib:make-action-goal actionclient point xtrans command 1)
        (actionlib:call-goal actionclient actiongoal)))))

		


(defun call-Motion-Move-To-Point (poke-point)
  "Moves right arm to poke-point"
  (let actionclient 
    (actionlib:make-action-client "/moving" "motion_msgs/MovingCommandAction")
    (loop until
          (actionlib:wait-for-server actionclient))
  (let point-to-poke 
    (roslisp:with-fields (poke_position) poke-point
      (print poke_position))
  (let actiongoal 
    (actionlib:make-action-goal actionclient point point-to-poke command 3)
    (actionlib:call-goal actionclient actiongoal)))))

