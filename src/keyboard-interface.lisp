(in-package :chip-8-keyboard)

;;;
;;; Interface
;;;

(defgeneric key-pressed? (keyboard key)
  (:documentation "Return true if KEY is currently pressed."))

(defgeneric wait-for-keypress (keyboard)
  (:documentation "Wait until a key has been pressed and return it."))


;;;
;;; Default methods
;;;

(defmethod key-pressed? ((keyboard t) key)
  (declare (ignore keyboard key))
  nil)

(defmethod wait-for-keypress ((keyboard t))
  (declare (ignore keyboard))
  0)
