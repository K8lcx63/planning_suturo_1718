(in-package :planning-knowledge)



(defun ask-Knowledge-For-Poke-Point(point-center-of-object)
  "Calling knowledge-service to look for a point to poke for specific object"
  (cpl:with-retry-counters ((retry-counter 10))
    (cpl:with-failure-handling
        ((cpl:simple-plan-failure (error-object)
           (format t "An error happened: ~a~%" error-object)
           (cpl:do-retry retry-counter
             (format t "Now retrying~%")
             (cpl:retry))
           (format t "Reached maximum amount of retries. Now propagating the failure up.~%")))    
      (roslisp:with-fields (object) point-center-of-object
        (setf point-center-of-object
              (roslisp:call-service "/poke_position_service/calculate_poke_position" 'object_detection-srv:PokeObject :detection object))
        (roslisp:with-fields (error_message) point-center-of-object
          (if (or (string= error_message "Failed to call service 'calculate_poke_position'. Transformation failed!")
                  (string= error_message "Failed to call service 'calculate_poke_position'. Prolog found no solution!"))
              (cpl:fail "Service call failed.")
              (print point-center-of-object)))))))   


(defun is-Object(objectString)
  (if (= objectString 22)(return-from is-Object T)(return-from is-Object nil))
  )
