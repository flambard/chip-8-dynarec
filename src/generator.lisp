(in-package :chip-8-dynarec)

;;;
;;; MAKE-PROGRAM
;;;
;;
;; This function takes a ROM vector and generates the corresponding Lisp
;; program in the form of a single function.
;;

(defun make-program (code)
  (multiple-value-bind (main-routine subroutines) (parse-rom code)
    `(lambda (display keyboard buzzer)
       (declare (ignorable display keyboard buzzer))
       (let ((v0 0) (v1 0) (v2 0) (v3 0) (v4 0) (v5 0) (v6 0) (v7 0)
             (v8 0) (v9 0) (vA 0) (vB 0) (vC 0) (vD 0) (vE 0) (vF 0)
             (I  0) (DT 0)
             (memory (make-ram)))
         (declare (type (unsigned-byte 16) I)
                  (type (unsigned-byte 8) DT
                        v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF)
                  (ignorable I DT
                             v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF))
         (let ((zero-dt-timer (trivial-timers:make-timer
                               #'(lambda () (setf DT 0)))))
           (declare (ignorable zero-dt-timer))
           (load-rom memory ,code)
           (labels ,(mapcar #'(lambda (sub)
                                `(,(make-subroutine-symbol (first sub)) ()
                                   (block nil
                                     (tagbody ,@sub))))
                            subroutines)
             (tagbody ,@main-routine)) ))) ))


(defun parse-rom (code)
  (multiple-value-bind (main subs-addr) (parse-routine +ram-lower-limit+ code)
    (values main (mapcar #'(lambda (addr)
                             (parse-routine addr code))
                         subs-addr))))


(defun parse-routine (start-address rom)
  (loop
     with addr = start-address
     with jumps = nil
     with subroutines = nil
     for instruction = (op->code (read-instruction rom (- addr 512)))
     collect addr into code
     collect (replace-pseudo-instruction addr instruction) into code
     do (case (first instruction)
          (:skip-next-if
           (pushnew (+ addr 4) jumps)
           (incf addr 2))
          (go
           (pushnew (second instruction) jumps)
           (setf jumps
                 (sort (remove-if #'(lambda (a) (member a code)) jumps) #'<))
           (if jumps
               (setf addr (pop jumps))
               (return (values code subroutines))))
          (:call
           (pushnew (second instruction) subroutines)
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

(defun replace-pseudo-instruction (address instruction)
  (case (first instruction)
    (:skip-next-if
     `(if ,(second instruction) (go ,(+ address 4))))
    (:call
     `(,(make-subroutine-symbol (second instruction))))
    (otherwise
     ;; Return non-pseudo instructions as-is
     instruction)))


;;;
;;; OP->CODE
;;;
;;
;; This function translates a single ROM instruction to the corresponding
;; Lisp code.
;;

(defun op->code (op)
  (case (ldb (byte 4 12) op)
    (#x0
     (case (ldb (byte 8 0) op)
       (#xE0
        ;; CLS
        ;; Clear the display
        '(chip-8-display:clear display))
       (#xEE
        ;; RET
        ;; Return from a subroutine
        '(return))
       (otherwise
        ;; SYS addr
        ;; Jump to machine code routine (ignored)
        (let ((n (ldb (byte 12 0) op)))
          `(sys ,n)))
       ))

    (#x1
     ;; JP addr
     ;; Jump to location n
     (let ((n (ldb (byte 12 0) op)))
       `(go ,n))
     )

    (#x2
     ;; CALL addr
     ;; Call subroutine at n
     (let ((n (ldb (byte 12 0) op)))
       `(:call ,n))
     )

    (#x3
     ;; SE Vx, byte
     ;; Skip next instruction if Vx = k
     (let ((x (ldb (byte 4 8) op))
           (k (ldb (byte 8 0) op)))
       `(:skip-next-if (= ,(make-register-symbol x) ,k)))
     )

    (#x4
     ;; SNE Vx, byte
     ;; Skip next instruction if Vx != k
     (let ((x (ldb (byte 4 8) op))
           (k (ldb (byte 8 0) op)))
       `(:skip-next-if (/= ,(make-register-symbol x) ,k)))
     )

    (#x5
     ;; SE Vx, Vy
     ;; Skip next instruction if Vx = Vy
     (let ((x (ldb (byte 4 8) op))
           (y (ldb (byte 4 4) op)))
       `(:skip-next-if (= ,(make-register-symbol x) ,(make-register-symbol y))))
     )

    (#x6
     ;; LD Vx, byte
     ;; Set Vx = k
     (let ((x (ldb (byte 4 8) op))
           (k (ldb (byte 8 0) op)))
       `(setf ,(make-register-symbol x) ,k))
     )

    (#x7
     ;; ADD Vx, byte
     ;; Set Vx = Vx + k
     (let ((x (ldb (byte 4 8) op))
           (k (ldb (byte 8 0) op)))
       `(incf ,(make-register-symbol x) ,k))
     )

    (#x8
     (case (ldb (byte 4 0) op) ;; ecase
       (#x0
        ;; LD Vx, Vy
        ;; Set Vx = Vy
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          `(setf ,(make-register-symbol x) ,(make-register-symbol y)))
        )
       (#x1
        ;; OR Vx, Vy
        ;; Set Vx = Vx OR Vy
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          `(setf ,(make-register-symbol x)
                 (logior ,(make-register-symbol x) ,(make-register-symbol y))))
        )
       (#x2
        ;; AND Vx, Vy
        ;; Set Vx = Vx AND Vy
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          `(setf ,(make-register-symbol x)
                 (logand ,(make-register-symbol x) ,(make-register-symbol y))))
        )
       (#x3
        ;; XOR Vx, Vy
        ;; Set Vx = Vx XOR Vy
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          `(setf ,(make-register-symbol x)
                 (logxor ,(make-register-symbol x) ,(make-register-symbol y))))
        )
       (#x4
        ;; ADD Vx, Vy
        ;; Set Vx = Vx + Vy, set VF = carry
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          ;; TODO: Carry!
          `(incf ,(make-register-symbol x) ,(make-register-symbol y)))
        )
       (#x5
        ;; SUB Vx, Vy
        ;; Set Vx = Vx - Vy, set VF = NOT borrow
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          ;; TODO: Borrow!
          `(decf ,(make-register-symbol x) ,(make-register-symbol y)))
        )
       (#x6
        ;; SHR Vx {, Vy}
        ;; Set Vx = Vx SHR 1
        (let ((x (ldb (byte 4 8) op)))
          `(setf ,(make-register-symbol x) (ash ,(make-register-symbol x) -1)))
        )
       (#x7
        ;; SUBN Vx, Vy
        ;; Set Vx = Vy - Vx, set VF = NOT borrow
        (let ((x (ldb (byte 4 8) op))
              (y (ldb (byte 4 4) op)))
          `(setf ,(make-register-symbol x)
                 (- ,(make-register-symbol y) ,(make-register-symbol x))))
        )
       (#xE
        ;; SHL Vx {, Vy}
        ;; Set Vx = Vx SHL 1
        (let ((x (ldb (byte 4 8) op)))
          `(setf ,(make-register-symbol x) (ash ,(make-register-symbol x) 1)))
        )))

    (#x9
     ;; SNE Vx, Vy
     ;; Skip next instruction if Vx != Vy
     (let ((x (ldb (byte 4 8) op))
           (y (ldb (byte 4 4) op)))
       `(:skip-next-if (/= ,(make-register-symbol x)
                           ,(make-register-symbol y))))
     )

    (#xA
     ;; LD I, addr
     ;; Set I = n
     (let ((n (ldb (byte 12 0) op)))
       `(setf i ,n))
     )

    (#xB
     ;; JP V0, addr
     ;; Jump to location n + V0
     (let ((n (ldb (byte 12 0) op)))
       (make-go-offset-case n))
     )

    (#xC
     ;; RND Vx, byte
     ;; Set Vx = random byte AND k
     (let ((x (ldb (byte 4 8) op))
           (k (ldb (byte 8 0) op)))
       `(setf ,(make-register-symbol x) (logand ,k (random 256))))
     )

    (#xD
     ;; DRW Vx, Vy, nibble
     ;; Display n-byte sprite starting at memory location I at (Vx, Vy),
     ;; set VF = collision
     (let ((x (ldb (byte 4 8) op))
           (y (ldb (byte 4 4) op))
           (n (ldb (byte 4 0) op)))
       `(chip-8-display:draw display
                             ,(make-register-symbol x)
                             ,(make-register-symbol y)
                             (read-bytes memory i ,n)))
     )

    (#xE
     (case (ldb (byte 8 0) op) ;; ecase
       (#x9E
        ;; SKP Vx
        ;; Skip next instruction if key with the value of Vx is pressed
        (let ((x (ldb (byte 4 8) op)))
          `(:skip-next-if
            (chip-8-keyboard:key-pressed? keyboard ,(make-register-symbol x))))
        )
       (#xA1
        ;; SKNP Vx
        ;; Skip next instruction if key with the value of Vx is not pressed
        (let ((x (ldb (byte 4 8) op)))
          `(:skip-next-if
            (not (chip-8-keyboard:key-pressed?
                  keyboard ,(make-register-symbol x)))))
        )))

    (#xF
     (case (ldb (byte 8 0) op) ;; ecase
       (#x07
        ;; LD Vx, DT
        ;; Set Vx = delay timer value
        (let ((x (ldb (byte 4 8) op)))
          `(setf ,(make-register-symbol x) dt))
        )
       (#x0A
        ;; LD Vx, K
        ;; Wait for a key press, store the value of the key in Vx
        (let ((x (ldb (byte 4 8) op)))
          `(let ((key (chip-8-keyboard:wait-for-keypress keyboard)))
             (setf ,(make-register-symbol x) key)))
        )
       (#x15
        ;; LD DT, Vx
        ;; Set delay timer = Vx
        (let ((x (ldb (byte 4 8) op)))
          `(progn
             (setf dt ,(make-register-symbol x))
             (trivial-timers:schedule-timer zero-dt-timer (/ dt 60))))
        )
       (#x18
        ;; LD ST, Vx
        ;; Set sound timer = Vx
        (let ((x (ldb (byte 4 8) op)))
          ;; Replace with call to sound-making interface
          ;; NOTE: There are no instructions for reading the sound timer which
          ;; means that we don't have to implement an actual timer that
          ;; decrements the DT register at 60 Hz. Just calculate for how long it
          ;; should beep and call the buzzer.
          `(chip-8-buzzer:beep buzzer (/ ,(make-register-symbol x) 60)))
        )
       (#x1E
        ;; ADD I, Vx
        ;; Set I = I + Vx
        (let ((x (ldb (byte 4 8) op)))
          `(incf i ,(make-register-symbol x)))
        )
       (#x29
        ;; LD F, Vx
        ;; Set I = location of sprite for digit Vx
        (let ((x (ldb (byte 4 8) op)))
          `(setf i ,(get-digit-sprite-address x)))
        )
       (#x33
        ;; LD B, Vx
        ;; Store BCD representation of Vx in memory locations I, I+1, and I+2
        (let* ((x (ldb (byte 4 8) op))
               (x-sym (make-register-symbol x)))
          `(multiple-value-bind (100s 10s-and-1s) (truncate ,x-sym 100)
             (multiple-value-bind (10s 1s) (truncate 10s-and-1s 10)
               (write-bytes memory i (vector 100s 10s 1s) 3))))
        )
       (#x55
        ;; LD [I], Vx
        ;; Store registers V0 through Vx in memory starting at location I
        (let ((x (ldb (byte 4 8) op)))
          `(write-bytes memory i
                        (vector ,@(loop
                                     for v upfrom 0 to x
                                     collect (make-register-symbol v)))
                        ,(1+ x)))
        )
       (#x65
        ;; LD Vx, [I]
        ;; Read registers V0 through Vx from memory starting at location I
        (let ((x (ldb (byte 4 8) op)))
          `(let ((bytes (read-bytes memory i ,(1+ x))))
             (setf ,@(loop
                        for v upfrom 0 to x
                        append `(,(make-register-symbol v) (aref bytes ,v))))))
        )))))



(defun make-register-symbol (n)
  (intern (format nil "V~X" n) :chip-8-dynarec))

(defun make-subroutine-symbol (n)
  (intern (format nil "SUBROUTINE-~D" n) :chip-8-dynarec))

(defun make-go-offset-case (n)
  `(ecase (+ ,n v0)
     ,@(loop
          for address upfrom n to (+ n 255) by 2
          collect `(,address (go ,address)))))
