(in-package :chip-8-dynarec)

(defun compile-program (filename)
  (let* ((rom (read-rom-file filename))
         (name (string-upcase (file-namestring filename)))
         (name-symbol (intern name :chip-8-program)))
    (compile name-symbol (make-program rom))
    (export name-symbol :chip-8-program)
    name-symbol))
