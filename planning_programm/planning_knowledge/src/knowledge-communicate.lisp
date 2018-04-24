(in-package :planning-knowledge)
 
(defvar *percieved-topic* nil)

(defun what-object(features)
  "Takes set of features to knowledge, and asks for identification"
  (roslisp:call-service "/svm_classifier/classify_service" 'knowledge_msgs-srv:Classify :features features))

(defun is-object(objectString features)
  "Takes features and string and asks knowledge if features are the object described in string"
  (roslisp:with-fields (label) (what-object features) (if (string= label objectString)(return-from is-object T)(return-from is-object nil))))



(defun ask-Knowledge-How-To-Grab (object)
   (roslisp:call-service "/knowledge_grasp/knowledge_grasp" 'knowledge_msgs-srv:GraspIndividual :object_label object))

(defun ask-knowledge-where-belongs (object)
  (roslisp:call-service "/storage_place_service/storage_place" 'knowledge_msgs-srv:StoragePlace :object_label object))

(defun place-object (gripper frame-id x-coordinate y-coordinate)
  (roslisp:call-service "/place_object/place" 'knowledge_msgs-srv:PlaceObject :gripper gripper :frame_id frame-id :x_coordinate x-coordinate :y_coordinate y-coordinate))

(defun objects-To-Pick ()
  (cram-language:wait-for (roslisp:call-service "/beliefstate/objects_to_pick" 'knowledge_msgs-srv:ObjectsToPick)))

(Defun how-To-Pick-Objects (object) 
    (roslisp:call-service "/knowledge_grasp/knowledge_grasp" 'knowledge_msgs-srv:GraspIndividual :object_label object))

(Defun empty-Gripper () 
    (roslisp:call-service "/beliefstate/gripper_empty" 'knowledge_msgs-srv:EmptyGripper))

(defun push-Object (object-label)
  (roslisp:call-service "/push_object/push" 'knowledge_msgs-srv:PushObject :object_label object-label))

