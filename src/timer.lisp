(in-package :chip-8-dynarec)

(defun make-timer (fn)
  (trivial-timers:make-timer fn))

(defun schedule-timer (timer seconds)
  (trivial-timers:schedule-timer timer seconds))
