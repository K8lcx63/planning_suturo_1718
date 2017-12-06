(in-package :planning-knowledge)



(defun ask-knowledge(point-center-of-object)
  "Calling knowledge-service to look for a point to poke for specific object"	
  (roslisp:with-fields (object) point-center-of-object
    (setf point-center-of-object
          (roslisp:call-service "/poke_position_service/calculate_poke_position" 'object_detection-srv:PokeObject :detection object))))

