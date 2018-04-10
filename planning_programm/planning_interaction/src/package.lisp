(in-package :cl-user)


(defpackage planning-interaction
  (:use #:common-lisp)
  (:export
   :init-interaction
   :say
   :decide-gripper
   :ask-human-to-move-object))

