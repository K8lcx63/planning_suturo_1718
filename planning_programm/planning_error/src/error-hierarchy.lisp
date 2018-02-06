(in-package :planning-error)

(define-condition custom-error (error) ((message :initarg :message :initform "" :reader message))
  (:documentation "any error"))

(define-condition vision-error (custom-error) ()
  (:report (lambda (condition stream)
             (format stream "vision error: ~A~%"
                     (message condition))))
  (:documentation "vision error"))

(define-condition motion-error (custom-error) ()
  (:report (lambda (condition stream)
             (format stream "motion error: ~A~%"
                     (message condition))))
  (:documentation "motion error"))

(define-condition move-error (custom-error) ()
  (:report (lambda (condition stream)
             (format stream "move error: ~A~%"
                     (message condition))))
  (:documentation "move error"))

(define-condition knowledge-error (custom-error) ()
  (:report (lambda (condition stream)
             (format stream "knowledge error: ~A~%"
                     (message condition))))
  (:documentation "knowledge error"))
