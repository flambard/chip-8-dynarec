(in-package :chip-8-buzzer)

;;;
;;; Interface
;;;

(defgeneric beep (buzzer seconds)
  (:documentation "Make some noise for SECONDS."))


;;;
;;; Default methods
;;;

(defmethod beep ((buzzer t) seconds)
  (declare (ignore buzzer seconds))
  nil)
