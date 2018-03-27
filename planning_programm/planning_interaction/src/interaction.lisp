(in-package :planning-interaction)

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
      (actionlib:call-goal actionclient actiongoal)
      ))
  )
