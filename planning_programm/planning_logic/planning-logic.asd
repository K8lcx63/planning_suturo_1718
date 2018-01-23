(defsystem planning-logic

  :author "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :maintainer "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :license "BSD"

  :depends-on (roslisp
               cl-tf
               actionlib
               geometry_msgs-msg
               pr2_controllers_msgs-msg
	       object_detection-srv
	       motion_msgs-msg
	       planning-motion
	       planning-vision
	       planning-knowledge
	       planning-move)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "external-logic" :depends-on ("package"))))))
