(in-package :planning-knowledge)



(defun ask-knowledge(poke-point)	
  (roslisp:with-fields (object) poke-point
			     (roslisp:call-service "/poke_service_node/calculate_poke_position" 'object_detection-srv:PokeObject
						   :detection object)))
		



