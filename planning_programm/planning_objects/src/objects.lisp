(in-package :planning-objects)

(defvar *width*)
(defvar *height*)

(defun calculate-landing-zone (object)
  (let ((landing-zone
          (planning-knowledge::ask-knowledge-where-belongs object)))
    (roslisp:with-fields ((*width*(storage_place_width))
                          (*height* (storage_place_height))
                          (position (storage_place_position)))
        landing-zone
      (setf *width* (- *width* 0.1))
      (setf *height* (- *height* 0.1))
      ;;(shrink-landing-zone)
      (split-landing-zone position))))

;(defun shrink-landing-zone
;  (setf *width* (- *width* 0.1))
;  (setf *height* (- *height* 0.1)))

(defun split-landing-zone (position)
  (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)))
      position
  ;; check data structure if this position has already been split
  (let ((offset (/ *height* 3.0)))
  (x (- x offset))
  ;; modify message position and safe in data structure?
  (x (+ x offset))
  ;; modify message position and safe in data structure?
  )))
  
  

  
;;    (landing-zone-small (roslisp:modify-message-copy landing-zone



    
;;    (roslisp:with-fields ((x (geometry_msgs-msg:x geometry_msgs-msg:point)) 
;;                          (y (geometry_msgs-msg:y geometry_msgs-msg:point))
;;                          (z (geometry_msgs-msg:z geometry_msgs-msg:point)))
;;        (knowledge_msgs-srv:storage_place_position
;;         (height (storage_place_height))
;;         (width (storage_place_width))
;;         (knowledge_msgs-srv:storageplace response))
;;      (= height (- height 0.1))
;;      (= width (- height 0.1))
      

