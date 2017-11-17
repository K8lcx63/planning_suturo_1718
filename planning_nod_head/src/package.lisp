(in-package :cl-user)


(defpackage planning-nod-head
  (:use #:common-lisp)	
  (:export #:look-in-front-of-me
	   #:change-move
	   #:*actionclient*
	   #:*trans-stamped*
	   #:*actiongoal*))

