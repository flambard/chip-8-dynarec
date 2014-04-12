(in-package :chip-8-display)

;;;
;;; Interface
;;;

(defgeneric clear (display)
  (:documentation "Clear the display."))

(defgeneric draw (display x y sprite)
  (:documentation "Draw SPRITE at location (X, Y)."))


;;;
;;; Default methods
;;;

(defmethod clear ((display t))
  (declare (ignore display))
  (format t "CLEAR DISPLAY~%"))

(defmethod draw ((display t) x y sprite)
  (declare (ignore display))
  (format t "AT (~a, ~a) DRAW:~%" x y)
  (loop
     for byte across sprite
     do (format t "  ~8,'0B~%" byte)))
