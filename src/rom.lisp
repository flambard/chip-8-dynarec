(in-package :chip-8-dynarec)

(defun read-rom-file (filename)
  (coerce (with-open-file (f filename :element-type '(unsigned-byte 8))
            (loop
               for op = (read-byte f nil)
               while op
               collect op))
          'vector))

(defun print-ops (ops)
  (loop
     for op in ops
     for addr upfrom #x200 by 2
     do (format t "~3,'0X: ~4,'0X~%" addr op)))
