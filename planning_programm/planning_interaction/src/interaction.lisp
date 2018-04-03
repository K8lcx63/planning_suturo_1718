(in-package :planning-interaction)

;; say (message)
;;
;; Utilizes sound_play package to play phoenetic string 
;;  
;; @input   string message - the message to speak out loud 
;; @output  sound_play-msg:SoundRequestResult - information about played sound

(defun say (message)
  "Uses sound_play service to let the pr2 say a string out loud"
  (let
      ((actionclient
         (actionlib:make-action-client "/sound_play" "sound_play/SoundRequestAction")))
    (let
        ((actiongoal
      (actionlib:make-action-goal actionclient sound_request 
        (roslisp:make-msg "sound_play/SoundRequest"
                          :sound -3
                          :command 1
                          :arg message
                          :arg2 ""))))
      (actionlib:wait-for-server actionclient)
      (actionlib:call-goal actionclient actiongoal))))



;; get-pointing-pose (pose)
;;
;; Points at a pose the pr2 cant reach. Uses static x-value to ensure that theres a kinematic solution
;; to Point at this position.
;;  
;; @input   geometry_msgs/PoseStamped pose  - unreachable Pose to Point at
;; @output  geometry_msgs/PoseStamped       - reachable Pose to Point at


(defun get-pointing-pose (pose)
  "Calculates pointing pose for given pose. Will be used to let the Pr2 use his Gripper to point at this pos"
  (let ((pose-in-baselink (planning-logic::transform-pose pose "base_link")))
                          (roslisp:modify-message-copy pose-in-baselink
                               (geometry_msgs-msg:x
                                geometry_msgs-msg:position
                                geometry_msgs-msg:pose) 2)))




;; ask-human-to-move-robot(pose label moving-command)
;;
;; Interacts with Human if Object is not reachable. Drives into Homeposition
;; Points at Object. Says String and then waits for Human to move Object and
;; shake his hand
;;
;; +++ UNDER CONSTRUCTION +++
;;  
;; @input   geometry_msgs/PoseStamped pose - pose of unreachable object
;; @input   string label                   - label of object as String (e.g. "Ja Milch")
;; @input   int moving-command             - 2 for left, 3 for right arm. Default is 3
;; @output  undefined


(defun ask-human-to-move-object (pose label &optional (moving-command 3))
  (planning-motion::call-motion-move-arm-homeposition)
  (let ((pose-to-point
          (get-pointing-pose pose)))
    (planning-motion::call-motion-move-arm-to-point pose-to-point "" moving-command)) 
  (say (concatenate 'string "I cannot grasp this object over there. Can you please move the " label  " and shake my Hand?")))

(defun drive-to-human ()
  (say "Driving to my Human now")
  )


