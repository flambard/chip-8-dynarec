(in-package :chip-8-dynarec)

(defconstant +ram-upper-limit+ #xFFF)
(defconstant +ram-lower-limit+ #x200)


(defun make-ram ()
  (let ((ram (make-array (1+ +ram-upper-limit+)
                         :element-type '(unsigned-byte 8)
                         :adjustable nil
                         :initial-element 0)))
    (write-bytes ram 0 +digit-sprites+ #.(* 5 16))
    ram))

(defun load-rom (ram code)
  (write-bytes ram +ram-lower-limit+ code (length code)))

(defun read-bytes (ram position n)
  (subseq ram position (+ position n)))

(defun write-bytes (ram position values n)
  (replace ram values
           :start1 position
           :end1 (+ position n)
           :start2 0
           :end2 n))

