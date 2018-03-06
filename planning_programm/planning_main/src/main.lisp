(in-package :planning-main)

(defvar *beliefstateHead* 0)
(defvar *headMovementList* '((0 -0.8)(1 -0.6)(2 -0.3)(3 0.0)(4 0.3)(5 0.6)(6 0.8)(7 0.6)(8 0.3)(9 0.0)(10 -0.3)(11 -0.6)))
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
;Kritischer Abschnit--------------------!!!! Wenn was runterfällt PUBLISH GRIPPER
;{
;Motion-->GREIF!
;Ja--> Greif
;Nein-->Versuch anders hinzufahren
;}

;[ABSTELLEN]
;{
;Hauke wo soll das Objekt hin? -->Hinfahren (funktion dafür muss noch kommen)
;Abstellen an Motion weiter geben
;Kritischer Abschnitt Ende--------------!!!!
;}

;zurück zur homeposi /loop anfang wieder
;MAINENDE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~







(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"
  (roslisp:with-ros-node ("planning_main")
    (roslisp:ros-info (main)
                      "Robotlife seems hard, but lets do this")

    (planning-move::move-Robo-Into-Homeposition)
    (if (eq T (find-Object 1.6 0))
             (let
                 ((n
                    (roslisp:get-param "object_amount")))                               
               (loop for amount from 1 to n do 
                 ;KNOWLEDGE WELCHES OBJEKT IST DAS?>>>>>
                 (let
                     ((name
                        (planning-knowledge::what-object
                         (planning-logic::list-To-1d-Array
                          (roslisp:get-param
                           (aref *my-array* (- amount 1)))))))
                   ;WEITERGABE AN VISION>>>>>>
                   (let
                       ((object_pose
                          (planning-vision::call-vision-object-pose name amount)))
                     (planning-logic::publish-pose name object_pose))
                     ;what should i grep?>>>>>>

                   (let
                       ((list-of-objects
                          (multiple-value-bind (o1 o2)
                                  (planning-knowledge::objects-to-pick)
                                (list o1 o2))))
                     (loop for object from 0 to 1 do
                      ;let's grab >>>>>>>>>>>>>>>>
                       (if (>
                            (length
                               (nth object list-of-objects)) 0)
                                        ;GREIFE DAS OBJECT1 = linker Arm
                                        ;(planning-Motion::pickpickpick)
                                        ;zusammen mit how-To-Pick
                                        ;KEIN OBJEKT ZUM GREIFEN
                            (print "pech")))
                                        ;FAHREN >>>>>>>>>>>>>>>>>>
                                   (multiple-value-bind
                                         (left_gripper right_gripper)
                                                           (planning-knowledge::objects-to-pick)
                                     (if (eq T (or left_gripper right_gripper))
                                         (cram-language:wait-for
                                          (planning-move::move-base-to-point-safe 0.78 0.8 0 0)))))))))))
                            ;landin zone --> y daten 
                   
                 
       

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
                     
               








             
