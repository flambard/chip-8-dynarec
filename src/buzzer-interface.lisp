(in-package :chip-8-buzzer)

;;;
;;; Interface
;;;

(defgeneric beep (buzzer seconds)
  (:documentation "Make some noise for SECONDS."))

(defgeneric silence (buzzer)
  (:documentation "Shut up."))


;;;
;;; Default methods
;;;

(defmethod beep ((buzzer t) seconds)
  (declare (ignore buzzer seconds))
  nil)

(defmethod silence ((buzzer t))
  (declare (ignore buzzer))
  nil)
