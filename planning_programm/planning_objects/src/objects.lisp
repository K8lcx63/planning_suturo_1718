(in-package :planning-objects)

(defun calculate-landing-zone (object)
  (let ((landing-zone
          (planning-knowledge::ask-knowledge-where-belongs object)))
;;    (landing-zone-small (roslisp:modify-message-copy landing-zone
))



    
;;    (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)) 
;;                          (y (geometry_msgs-msg:y geometry_msgs-msg:point))
;;                          (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
;;        (knowledge_msgs-srv:storage_place_position
;;         (height (storage_place_height))
;;         (width (storage_place_width))
;;         (knowledge_msgs-srv:storageplace response))
;;      (= height (- height 0.1))
;;      (= width (- height 0.1))
      

