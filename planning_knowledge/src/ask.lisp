(in-package :planning-knowledge)



(defun ask-knowledge(point-center-of-object)
  "Calling knowledge-service to look for a point to poke for specific object"	
  (setf point-center-of-object (roslisp:call-service "/poke_service_node/calculate_poke_position" 'object_detection-srv:PokeObject :detection point-center-of-object)))



