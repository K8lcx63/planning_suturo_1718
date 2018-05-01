(in-package :planning-main)

(defvar *beliefstateHead* 0)
(defvar *headMovementList* '((0 0)(1 -0.6)(2 -0.3)(3 0.0)(4 0.3)(5 0.6)(6 0.8)(7 0.6)(8 0.3)(9 0.0)(10 -0.3)(11 -0.6)))
(defvar *pose-array* nil)
(defvar *my-array* (make-array '(10) 
                                 :initial-contents
                                 '("features1" "features2" "features3" "features4" "features5" "features6" "features7" "features8" "features9" "features10")))

;MAINFUNKTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;>>LOOP<<

;homeposi
;kopf-bewegen

;[FUNKTION OBJEKT SUCHEN] 
;{
;alle object Suchen --> Wenn alle Objekte gefunden nach einander an knowledgen geben (input feature vektoren output string z.B "Ja_Milch")
;weitergabe an vision von Objekten String+Nummer im array, sodass pose kommt --> publish an knowledge String + Pose
;}

;[CALL]
;Frage knowlege welche objekte soll ich nehmen?
;[CALL]
;Wie greifen?

;[TRY TO GRAP]
;{
;Motion-->GREIF!
;Ja--> Greif
;Nein-->Versuch anders hinzufahren -->
;}

;Kritischer Abschnit--------------------!!!! Wenn was runterfällt PUBLISH GRIPPER
;[ABSTELLEN]
;{
;Hauke wo soll das Objekt hin? -->Hinfahren (funktion dafür muss noch kommen)
;Abstellen an Motion weiter geben
;Kritischer Abschnitt Ende--------------!!!!
;}

;zurück zur homeposi /loop anfang wieder
;MAINENDE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
    (roslisp:ros-info (main)
                      "Robotlife seems hard, but lets do this")
  ;(roslisp:with-ros-node ("planning_main")
  (planning-logic::init-logic)
  ;(planning-move::move-Robo-Into-Homeposition)
  (planning-logic::publish-text "Searching for objects")
  (if (eq T (find-Object-Dummy 1.2 0))
      (let
          ((n
             (roslisp:get-param "object_amount")))
        (print (roslisp:get-param "object_amount"))
        (planning-logic::publish-text "I found at least 1 Object -> starting to classify")
        (loop for amount from 1 to n do
          (print "amount:")(print amount)
                                        ;KNOWLEDGE WELCHES OBJEKT IST DAS?>>>>
          (let
              ((name
                 (planning-knowledge::what-object
                      (planning-logic::list-To-1d-Array
                       (roslisp:get-param
                        (aref *my-array* (- amount 1)))))))
            (print name)
                                        ;WEITERGABE AN VISION>>>>>>
            (planning-logic::publish-text "Calling vision to get the pose")
            (let
                ((object_pose_msg
                   (roslisp:with-fields (label) name
                     (cram-language:wait-for (planning-vision::call-vision-object-pose label (- amount 1))))))
              (roslisp:with-fields
                  (object_pose) object_pose_msg
                (progn
                  (planning-logic::publish-text "Publishing object-pose")
                  (print name)
                  (roslisp:with-fields (label) name 
                    (cram-language:wait-for (planning-logic::publish-pose label object_pose))
                    (planning-logic::save-object label object_pose)))))))
        (loop for i from 1 to 2 do
          (roslisp:with-fields (object_label_1)
              (planning-knowledge:objects-to-pick)
          (planning-logic::grab-left-or-right object_label_1)))
        (planning-motion::call-motion-move-arm-homeposition 10)
        ;hier muss noch eine logik rein die entscheidet wie viele gripper grade benutzt werden sollen r und l
        (planning-interaction:check-gripper "errormsgs" 'planning-logic:move-pr2 '(0.75 1 0 0) 0 0)
              )))

       

(defun find-Object (x z)
  "Looking around to find /beliefstate/gripper_empty an object, restarting at current Point if reused" 
  (block find-Object-Start
    (loop for i from *beliefstateHead* to 24 do
      (let ((c (mod i 12)))
        (progn
          (planning-move::move-Head x(second (assoc c *headMovementList*)) z)
          (setf *beliefstateHead* c)
          (setf *pose-array*
                (planning-logic::disassemble-vision-call
                 (planning-vision::call-vision-object-clouds)))
          (if (> (roslisp:get-param "object_amount") 0)
              (return-from find-Object-Start)))))
    (roslisp::ros-info "find-Object" "Couldnt find any Object in front of me")
    (return-from find-Object nil))
  (roslisp::ros-info "find-Object" "I see the Object. Head is in Position")
  (return-from find-Object T))
                     
               
(defun find-Object-Dummy (x z)
  ".."
  (planning-move::move-Head x 0 z)
  (sleep 5.0)
  (setf *pose-array*
        (planning-logic::disassemble-vision-call(planning-vision::call-vision-object-clouds)))
  (return-from find-Object-Dummy T))




