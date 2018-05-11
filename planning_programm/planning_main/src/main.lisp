(in-package :planning-main)


(defvar *headMovementList* '((0 0)(1 -0.6)(2 -0.3)(3 0.0)(4 0.3)(5 0.6)))
(defvar *y* nil)

(defun init ()
  (planning-logic:init-logic)
  (planning-interaction:init-interaction))
  

(defun main ()
  "Main function - Executing and planning robot behaviour on the top level"  
                                        ;(roslisp:with-ros-node ("planning_main")
  (init)
  ;;homeposition of the robo, he stand infront of the kitchen_island
  (planning-move:move-base-to-point -0.29 1 0 180)
  (block find-Objects-Start
    (roslisp:set-param "counter" 0)
    (loop for i from 0 to 0 do 
      (planning-move:move-Head 1.2 (second (assoc i *headMovementList*)) 0)

      ;;perciving objects
      (sleep 5.0)
      (planning-logic::percieve-objetcs))

    (if (> (roslisp:get-param "counter") 0)
        (progn
          ;;grab object
          (loop for i from 1 to 2 do
            (roslisp:with-fields (object_label_1)
                (planning-knowledge:objects-to-pick)
              (planning-logic:grab-left-or-right -0.29 1 180 object_label_1))) 
          (planning-motion:call-motion-move-arm-homeposition 10)
          (planning-logic::how-many-gripper)

          
          ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>WELCHER GRIPPER + OBJEKT?! +LOOP

          (loop for i from 1 to 2 do
            (roslisp:with-fields (object_label)
                (planning-knowledge::get-object-attached-to-gripper i)
              (block drive-And-Place
                ;;were should pr2 drive
                 (if
                  (> (length object_label) 0)
                  (let ((calculate-landing-zone
                          (planning-objects::calculate-landing-zone object_label i)))
                    (setf *y*
                          (planning-logic:disassemble-graspindividual-response (nth 0 calculate-landing-zone)))
                    ;;driving to point
                    ;; (cram-language:top-level
                    ;; (planning-interaction:check-gripper "errormsgs" 'planning-logic:move-base '(0.75 *y* 0 0 10 "/map" nil)
                    ;;                                     planning-logic::*r*
                    ;;                                     planning-logic::*l*))
                    ;;placing Object
                    (planning-logic:move-base 0.75 *y* 0 0)
                    (roslisp:with-fields (place_pose)
                        (nth 0 calculate-landing-zone)
                      (if (= i 2)
                          (planning-motion:call-motion-move-arm-to-point place_pose object_label 8)
                          (planning-motion:call-motion-move-arm-to-point place_pose objecT_label 9)))
                    (if
                     (String= "storage-place-full" (nth 1 calculate-landing-zone))
                     (planning-objects::push-object)))))))

          ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>LOOP ABSTELLEN STOP!!
          
          (planning-logic:move-base -0.29 1 0 180)
          (roslisp:delete-param "counter")
          (return-from find-Objects-Start))
        (return-from main "cant find any object"))))




(defun main-demo ()
  "Main function - Executing and planning robot behaviour on the top level"  
                                        ;(roslisp:with-ros-node ("planning_main")
  (init)
  ;;homeposition of the robo, he stand infront of the kitchen_island
  (planning-move:move-base-to-point -0.29 1.4 0 180)
  (block find-Objects-Start
    (roslisp:set-param "counter" 0)
    (loop for i from 0 to 0 do
      (planning-move:move-Head 1.4 (second (assoc i *headMovementList*)) 0)
      (print "publish jamilch")
      ;;perciving objects !! the object "JaMilch" needs to be placed on the right spot
      (sleep 5.0)
      (planning-logic::publish-pose-jamilch))
    
    (if (> (roslisp:get-param "counter") 0)
        (progn
          ;;grab object
          (loop for i from 1 to 2 do
            (print "trying to grab:")
            (roslisp:with-fields (object_label_1)
                (planning-knowledge:objects-to-pick)
              (print object_label_1)
              (planning-logic:grab-left-or-right -0.29 1.4 180 object_label_1))) 
          (planning-motion:call-motion-move-arm-homeposition 10)
          (planning-logic::how-many-gripper)

          
        ;;   ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>WELCHER GRIPPER + OBJEKT?! +LOOP

        ;;   (loop for i from 1 to 2 do
        ;;     (roslisp:with-fields (object_label)
        ;;         (planning-knowledge::get-object-attached-to-gripper i)
        ;;       (block drive-And-Place
        ;;         ;;were should pr2 drive
        ;;          (if
        ;;           (> (length object_label) 0)
        ;;           (let ((calculate-landing-zone
        ;;                   (planning-objects::calculate-landing-zone object_label i)))
        ;;             (setf *y*
        ;;                   (planning-logic:disassemble-graspindividual-response calculate-landing-zone))
        ;;             ;;driving to point
        ;;             (planning-interaction:check-gripper "errormsgs" 'planning-logic:move-base '(0.75 *y* 0 0)
        ;;                                                 planning-logic::*r*
        ;;                                                 planning-logic::*l*)
        ;;             ;;placing Object 
        ;;             (roslisp:with-fields (place_pose)
        ;;                 (nth 0 calculate-landing-zone)
        ;;               (if (= i 2)
        ;;                   (planning-motion:call-motion-move-arm-to-point place_pose object_label 8)
        ;;                   (planning-motion:call-motion-move-arm-to-point place_pose objecT_label 9)))
        ;;             (if
        ;;              (String= "storage-place-full" (nth 1 calculate-landing-zone))
        ;;              (planning-objects::push-object)))))))

        ;;   ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>LOOP ABSTELLEN STOP!!
          
        ;;   (planning-logic:move-base -0.29 1 0 180)
        ;;   (roslisp:delete-param "counter")
        ;;   (return-from find-Objects-Start))
        ;; (return-from main-demo "cant find any object"))))
))))
