(in-package :planning-knowledge)



(defun ask-knowledge(poke-point)
"Calling knowledge-service to look for a point to poke for specific object"	
  (roslisp:with-fields (object) poke-point
			     (roslisp:call-service "/poke_service_node/calculate_poke_position" 'object_detection-srv:PokeObject
						   :detection object)))
		



