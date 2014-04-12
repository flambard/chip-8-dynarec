(defpackage #:chip-8-dynarec
  (:documentation "CHIP-8-to-Lisp recompiler.")
  (:use #:cl)
  (:export

   #:compile-program

   ))

(defpackage #:chip-8-buzzer
  (:documentation "CHIP-8 buzzer interface.")
  (:use #:cl)
  (:export

   #:beep
   #:silence

   ))

(defpackage #:chip-8-display
  (:documentation "CHIP-8 display interface.")
  (:use #:cl)
  (:export

   #:clear
   #:draw

   ))

(defpackage #:chip-8-keyboard
  (:documentation "CHIP-8 keyboard interface.")
  (:use #:cl)
  (:export

   #:key-pressed?
   #:wait-for-keypress

   ))

(defpackage #:chip-8-program
  (:documentation "Package of currently compiled and loaded CHIP-8 programs.")
  (:export

   ;; Programs will be exported when they are compiled.

   ))
