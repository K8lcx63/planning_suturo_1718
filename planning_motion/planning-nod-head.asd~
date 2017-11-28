(defsystem planning-nod-head

  :author "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :maintainer "Kevin Störmer, Vanessa Hassouna, Hauke Tietjen"
  :license "BSD"

  :depends-on (roslisp
               cl-tf
               actionlib
               geometry_msgs-msg
               pr2_controllers_msgs-msg
	       motion_msgs-msg)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "ptu" :depends-on ("package"))))))
