(defsystem planning-knowledge

  :author "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :maintainer "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :license "BSD"

  :depends-on (roslisp
               cl-tf
               actionlib
               geometry_msgs-msg
               pr2_controllers_msgs-msg
	       object_detection-srv
	       motion_msgs-msg)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "knowledge-communicate" :depends-on ("package"))))))
