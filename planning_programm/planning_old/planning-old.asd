(defsystem planning-old

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
	       sensor_msgs-msg
	       cram-language
	       move_base_msgs-msg)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "old-functions" :depends-on ("package"))))))
