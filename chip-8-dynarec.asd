(defpackage :chip-8-dynarec-system
  (:use :cl))

(in-package :chip-8-dynarec-system)

(asdf:defsystem :chip-8-dynarec
  :description "Dynamic CHIP-8 recompiler."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.0.1"
  :license "MIT License"
  :depends-on (:trivial-timers)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "buzzer-interface"
                    :depends-on ("packages"))
             (:file "display-interface"
                    :depends-on ("packages"))
             (:file "keyboard-interface"
                    :depends-on ("packages"))
             (:file "digits"
                    :depends-on ("packages"))
             (:file "ram"
                    :depends-on ("packages"))
             (:file "rom"
                    :depends-on ("packages"))
             (:file "generator"
                    :depends-on ("packages"
                                 "buzzer-interface"
                                 "display-interface"
                                 "keyboard-interface"
                                 "digits"
                                 "ram"
                                 "rom"))
             (:file "compiler"
                    :depends-on ("packages"
                                 "generator"))
             ))))
