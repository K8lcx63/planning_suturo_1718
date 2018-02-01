(defsystem planning-objects

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
	       cram-language
	       planning-error
	       knowledge_msgs-srv)


  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "objects" :depends-on ("package"))))))
