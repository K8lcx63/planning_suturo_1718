(defsystem planning-vision

  :author "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :maintainer "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :license "BSD"

  :depends-on (roslisp
               cl-tf
               actionlib
               geometry_msgs-msg
               pr2_controllers_msgs-msg
	       object_detection-srv
	       vision_msgs-srv
	       motion_msgs-msg
	       cram-language)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "vision-communicate" :depends-on ("package"))))))
