(in-package :planning-error)

(define-condition custom-error (error) ((message :initarg :message :initform "" :reader message))
  (:documentation "any error"))

(define-condition vision-error (custom-error) ()
  (:report (lambda (condition stream)
             (format stream "vision error: ~A~%"
                     (message condition))))
  (:documentation "vision-error"))

(define-condition motion-error (error) ((message :initarg :message :documentation "~A")))

(define-condition move-error (error) ((message :initarg :message :documentation "~A")))

(define-condition knowledge-error (error) ((message :initarg :message :documentation "~A")))
