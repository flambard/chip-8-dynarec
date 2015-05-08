(in-package :chip-8-dynarec)

(defun compile-program (filename)
  (let* ((rom (read-rom-file filename))
         (name (string-upcase (file-namestring filename)))
         (name-symbol (intern name :chip-8-program))
         (program (make-program rom)))
    (compile name-symbol program)
    (setf (get name-symbol :code) program)
    (export name-symbol :chip-8-program)
    name-symbol))

(defun make-program (code)
  (multiple-value-bind (main-routine subroutines) (read-rom code)
    (put-in-context code main-routine subroutines)))

(defun put-in-context (code main-routine subroutines)
  `(lambda (display keyboard buzzer)
     (declare (ignorable display keyboard buzzer))
     (let ((v0 0) (v1 0) (v2 0) (v3 0) (v4 0) (v5 0) (v6 0) (v7 0)
           (v8 0) (v9 0) (vA 0) (vB 0) (vC 0) (vD 0) (vE 0) (vF 0)
           (I  0) (DT 0)
           (memory (chip-8-ram:make-ram)))
       (declare (type (unsigned-byte 16) I)
                (type (unsigned-byte 8) DT
                      v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF)
                (ignorable I DT
                           v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF))
       (let ((zero-dt-timer (make-timer #'(lambda () (setf DT 0)))))
         (declare (ignorable zero-dt-timer))
         (chip-8-ram:load-rom memory ,code)
         (labels ,(loop for (subroutine-symbol . code) in subroutines collect
                       `(,subroutine-symbol ()
                              (block nil
                                (tagbody ,@code))))
           (tagbody ,@main-routine)) ))) )
