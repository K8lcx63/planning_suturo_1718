(in-package :planning-main)

(defvar *pose-array* nil)
(defvar *my-array* (make-array '(5) 
                                 :initial-contents
                                 '("features1" "features2" "features3" "features4" "features5")))
(defvar *head-array-tmp* (make-array '(5)
                                     :initial-contents
                                     '(1 -1 2 -2 0)))

(defun main (objectString)
  "Main function - Executing and planning robot behaviour on the top level"
  (roslisp:with-ros-node ("planning_main")
    (roslisp:ros-info (main)
                      "Robotlife seems hard, but lets do this")
    (planning-move::move-Robo-Into-Homeposition)
    (planning-move::move-Head 2.6 0 0)
    (loop for counter from 5 downto 1
          do (progn
               (setf *pose-array* (planning-logic::disassemble-Vision-Call
                (planning-vision::call-Vision-Object-Clouds)))
             (let
                 ((n
                    (roslisp:get-param "object_amount")))
               (if (> n 0)
                   (loop for amount from 1 to n do
                     ;sehen wir das gewünschte object
                     (if (planning-knowledge::is-Object objectString
                                             (planning-logic::list-To-1d-Array
                                              (roslisp:get-param
                                               (aref *my-array* (- amount 1)))))
                          ;anstupsen
                         (progn
                           (planning-motion::call-Motion-Move-Arm-To-Point
                            (planning-logic::disassemble-pose-msg-to-point-stamped *pose-array*
                                                                                   (- amount 1)))
                              (planning-motion::call-Motion-Move-Arm-Homeposition)
                              (return-from main
                                (roslisp:ros-info (main)
                                                  "object should be touched")))
                         ;es ist nicht das objekt --> kopf bewegen
                         (planning-move::move-Head 2.6 0 (aref *head-array-tmp* counter))))
                   ;ich sehe kein objekt --> kopf bewegen
                   (planning-move::move-Head 2.6 0 (aref *head-array-tmp* counter))))))
    (return-from main (roslisp:ros-info (main)
                                        "sry i can't find an object.."))))
                     
               








             
