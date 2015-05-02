(in-package :chip-8-dynarec)

(defun read-rom (code)
  (multiple-value-bind (main subs-addr)
      (parse-routine chip-8-ram:+lower-limit+ code)
    (values main (mapcar #'(lambda (addr)
                             (cons (make-subroutine-symbol addr)
                                   (read-routine addr code)))
                         subs-addr))))

(defun read-routine (start-address rom)
  (loop
     with addr = start-address
     with jumps = nil
     with subroutines = nil
     for op = (translate-instruction (read-instruction rom (- addr 512)))
     collect addr into code
     collect (replace-pseudo-instruction addr op) into code
     do (case (first op)
          (:skip-next-if
           (pushnew (+ addr 4) jumps)
           (incf addr 2))
          (go
           (pushnew (second op) jumps)
           (setf jumps
                 (sort (remove-if #'(lambda (a) (member a code)) jumps) #'<))
           (if jumps
               (setf addr (pop jumps))
               (return (values code subroutines))))
          (:call
           (pushnew (second op) subroutines)
           (incf addr 2))
          (return
           (return (values code subroutines)))
          (otherwise
           (incf addr 2)) )) )


(defun read-instruction (code position)
  (let ((instruction 0))
    (setf (ldb (byte 8 8) instruction) (aref code position))
    (setf (ldb (byte 8 0) instruction) (aref code (1+ position)))
    instruction))

(defun replace-pseudo-instruction (address op)
  (case (first op)
    (:skip-next-if `(if ,(second op) (go ,(+ address 4))))
    (:call         `(,(make-subroutine-symbol (second op))))
    (otherwise     op))) ;; Return non-pseudo instruction as-is

(defun make-subroutine-symbol (n)
  (intern (format nil "SUBROUTINE-~D" n) :chip-8-dynarec))
