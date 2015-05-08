(in-package :chip-8-dynarec)


(defun translate-instruction (instruction)
  (op-code (ldb (byte 4 12) instruction)
           (ldb (byte 4  8) instruction)
           (ldb (byte 4  4) instruction)
           (ldb (byte 4  0) instruction)))


(defgeneric op-code (nibble0 nibble1 nibble2 nibble3)
  (:documentation "Translates a single ROM instruction to Lisp code"))


(defmethod op-code ((n0 (eql #x0)) (n1 (eql #x0)) (n2 (eql #xE)) (n3 (eql #x0)))
  ;; 00E0: CLS
  ;; Clear the display
  '(chip-8-display:clear display))

(defmethod op-code ((n0 (eql #x0)) (n1 (eql #x0)) (n2 (eql #xE)) (n3 (eql #xE)))
  ;; 00EE: RET
  ;; Return from a subroutine
  '(return))

(defmethod op-code ((n0 (eql #x0)) n1 n2 n3)
  ;; 0nnn: SYS addr
  ;; Jump to machine code routine (ignored)
  (let ((n (combine-3-nibbles n1 n2 n3)))
    `(sys ,n)))

(defmethod op-code ((n0 (eql #x1)) n1 n2 n3)
  ;; 1nnn: JP addr
  ;; Jump to location n
  (let ((n (combine-3-nibbles n1 n2 n3)))
    `(go ,n)))

(defmethod op-code ((n0 (eql #x2)) n1 n2 n3)
  ;; 2nnn: CALL addr
  ;; Call subroutine at n
  (let ((n (combine-3-nibbles n1 n2 n3)))
    `(:call ,n)))

(defmethod op-code ((n0 (eql #x3)) x n2 n3)
  ;; 3xnn: SE Vx, byte
  ;; Skip next instruction if Vx = k
  (let ((k (combine-2-nibbles n2 n3)))
    `(:skip-next-if (= ,(make-register-symbol x) ,k))))

(defmethod op-code ((n0 (eql #x4)) x n2 n3)
  ;; 4xnn: SNE Vx, byte
  ;; Skip next instruction if Vx != k
  (let ((k (combine-2-nibbles n2 n3)))
    `(:skip-next-if (/= ,(make-register-symbol x) ,k))))

(defmethod op-code ((n0 (eql #x5)) x y (n3 (eql #x0)))
  ;; 5xy0: SE Vx, Vy
  ;; Skip next instruction if Vx = Vy
  `(:skip-next-if (= ,(make-register-symbol x) ,(make-register-symbol y))))

(defmethod op-code ((n0 (eql #x6)) x n2 n3)
  ;; 6xnn: LD Vx, byte
  ;; Set Vx = k
  (let ((k (combine-2-nibbles n2 n3)))
    `(setf ,(make-register-symbol x) ,k)))

(defmethod op-code ((n0 (eql #x7)) x n2 n3)
  ;; 7xnn: ADD Vx, byte
  ;; Set Vx = Vx + k
  (let ((k (combine-2-nibbles n2 n3)))
    `(incf ,(make-register-symbol x) ,k)))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x0)))
  ;; 8xy0: LD Vx, Vy
  ;; Set Vx = Vy
  `(setf ,(make-register-symbol x) ,(make-register-symbol y)))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x1)))
  ;; 8xy1: OR Vx, Vy
  ;; Set Vx = Vx OR Vy
  `(setf ,(make-register-symbol x)
         (logior ,(make-register-symbol x) ,(make-register-symbol y))))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x2)))
  ;; 8xy2: AND Vx, Vy
  ;; Set Vx = Vx AND Vy
  `(setf ,(make-register-symbol x)
         (logand ,(make-register-symbol x) ,(make-register-symbol y))))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x3)))
  ;; 8xy3: XOR Vx, Vy
  ;; Set Vx = Vx XOR Vy
  `(setf ,(make-register-symbol x)
         (logxor ,(make-register-symbol x) ,(make-register-symbol y))))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x4)))
  ;; 8xy4: ADD Vx, Vy
  ;; Set Vx = Vx + Vy, set VF = carry
  `(let ((sum (+ ,(make-register-symbol x) ,(make-register-symbol y))))
     (setf vF (if (> sum 255) 1 0))
     (setf ,(make-register-symbol x) (mod sum 256))))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x5)))
  ;; 8xy5: SUB Vx, Vy
  ;; Set Vx = Vx - Vy, set VF = NOT borrow
  `(let ((diff (- ,(make-register-symbol x) ,(make-register-symbol y))))
     (setf vF (if (> 0 diff) 0 1))
     (multiple-value-bind (q r) (floor diff 256)
       (declare (ignore q))
       (setf ,(make-register-symbol x) r))))

(defmethod op-code ((n0 (eql #x8)) x (n2 (eql #x0)) (n3 (eql #x6)))
  ;; 8x06: SHR Vx {, Vy}
  ;; Set Vx = Vx SHR 1
  (let* ((x-sym (make-register-symbol x)))
    `(progn
       (setf vF (ldb (byte 1 0) ,x-sym))
       (setf ,x-sym (ash ,x-sym -1)))))

(defmethod op-code ((n0 (eql #x8)) x y (n3 (eql #x7)))
  ;; 8xy7: SUBN Vx, Vy
  ;; Set Vx = Vy - Vx, set VF = NOT borrow
  `(let ((diff (- ,(make-register-symbol y) ,(make-register-symbol x))))
     (setf vF (if (> 0 diff) 0 1))
     (multiple-value-bind (q r) (floor diff 256)
       (declare (ignore q))
       (setf ,(make-register-symbol x) r))))

(defmethod op-code ((n0 (eql #x8)) x (n2 (eql #x0)) (n3 (eql #xE)))
  ;; 8xyE: SHL Vx {, Vy}
  ;; Set Vx = Vx SHL 1
  (let ((x-sym (make-register-symbol x)))
    `(progn
       (setf vF (ldb (byte 1 7) ,x-sym))
       (setf ,x-sym (ash ,x-sym 1)))))

(defmethod op-code ((n0 (eql #x9)) x y (n3 (eql #x0)))
  ;; 9xy0: SNE Vx, Vy
  ;; Skip next instruction if Vx != Vy
  `(:skip-next-if (/= ,(make-register-symbol x) ,(make-register-symbol y))))

(defmethod op-code ((n0 (eql #xA)) n1 n2 n3)
  ;; Annn: LD I, addr
  ;; Set I = n
  (let ((n (combine-3-nibbles n1 n2 n3)))
    `(setf i ,n)))

(defmethod op-code ((n0 (eql #xB)) n1 n2 n3)
  ;; Bnnn: JP V0, addr
  ;; Jump to location n + V0
  (let ((n (combine-3-nibbles n1 n2 n3)))
    `(ecase (+ ,n v0)
       ,@(loop
            for address upfrom n to (+ n 255) by 2
            collect `(,address (go ,address))))))

(defmethod op-code ((n0 (eql #xC)) x n2 n3)
  ;; Cxnn: RND Vx, byte
  ;; Set Vx = random byte AND k
  (let ((k (combine-2-nibbles n2 n3)))
    `(setf ,(make-register-symbol x) (logand ,k (random 256)))))

(defmethod op-code ((n0 (eql #xD)) x y n3)
  ;; Dxyn: DRW Vx, Vy, nibble
  ;; Display n-byte sprite starting at memory location I at (Vx, Vy),
  ;; set VF = collision
  `(let ((collision (chip-8-display:draw display
                                         ,(make-register-symbol x)
                                         ,(make-register-symbol y)
                                         (chip-8-ram:read-bytes memory i ,n3))))
     (setf vF (if collision 1 0))))

(defmethod op-code ((n0 (eql #xE)) x (n2 (eql #x9)) (n3 (eql #xE)))
  ;; Ex9E: SKP Vx
  ;; Skip next instruction if key with the value of Vx is pressed
  `(:skip-next-if
    (chip-8-keyboard:key-pressed? keyboard ,(make-register-symbol x))))

(defmethod op-code ((n0 (eql #xE)) x (n2 (eql #xA)) (n3 (eql #x1)))
  ;; ExA1: SKNP Vx
  ;; Skip next instruction if key with the value of Vx is not pressed
  `(:skip-next-if
    (not (chip-8-keyboard:key-pressed? keyboard ,(make-register-symbol x)))))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x0)) (n3 (eql #x7)))
  ;; Fx07: LD Vx, DT
  ;; Set Vx = delay timer value
  `(setf ,(make-register-symbol x) dt))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x0)) (n3 (eql #xA)))
  ;; Fx0A: LD Vx, K
  ;; Wait for a key press, store the value of the key in Vx
  `(let ((key (chip-8-keyboard:wait-for-keypress keyboard)))
     (setf ,(make-register-symbol x) key)))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x1)) (n3 (eql #x5)))
  ;; Fx15: LD DT, Vx
  ;; Set delay timer = Vx
  `(progn
     (setf dt ,(make-register-symbol x))
     (schedule-timer zero-dt-timer (/ dt 60))))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x1)) (n3 (eql #x8)))
  ;; Fx18: LD ST, Vx
  ;; Set sound timer = Vx
  ;; Replace with call to sound-making interface
  ;; NOTE: There are no instructions for reading the sound timer which
  ;; means that we don't have to implement an actual timer that
  ;; decrements the DT register at 60 Hz. Just calculate for how long it
  ;; should beep and call the buzzer.
  `(chip-8-buzzer:beep buzzer (/ ,(make-register-symbol x) 60)))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x1)) (n3 (eql #xE)))
  ;; Fx1E: ADD I, Vx
  ;; Set I = I + Vx
  `(incf i ,(make-register-symbol x)))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x2)) (n3 (eql #x9)))
  ;; Fx29: LD F, Vx
  ;; Set I = location of sprite for digit Vx
  `(setf i (chip-8-ram:get-digit-sprite-address ,(make-register-symbol x))))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x3)) (n3 (eql #x3)))
  ;; Fx33: LD B, Vx
  ;; Store BCD representation of Vx in memory locations I, I+1, and I+2
  (let ((x-sym (make-register-symbol x)))
    `(chip-8-ram:write-bytes memory i (bcd ,x-sym) 3)))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x5)) (n3 (eql #x5)))
  ;; Fx55: LD [I], Vx
  ;; Store registers V0 through Vx in memory starting at location I
  `(chip-8-ram:write-bytes memory i
                           (vector ,@(loop
                                        for v upfrom 0 to x
                                        collect (make-register-symbol v)))
                           ,(1+ x)))

(defmethod op-code ((n0 (eql #xF)) x (n2 (eql #x6)) (n3 (eql #x5)))
  ;; Fx65: LD Vx, [I]
  ;; Read registers V0 through Vx from memory starting at location I
  `(let ((bytes (chip-8-ram:read-bytes memory i ,(1+ x))))
     (setf ,@(loop
                for v upfrom 0 to x
                append `(,(make-register-symbol v) (aref bytes ,v))))))


(defmacro bcd (n)
  `(multiple-value-bind (100s 10s-and-1s) (truncate ,n 100)
     (multiple-value-bind (10s 1s) (truncate 10s-and-1s 10)
       (vector 100s 10s 1s))))

(defun combine-2-nibbles (n1 n2)
  (logior (ash n1 4) n2))

(defun combine-3-nibbles (n1 n2 n3)
  (logior (ash n1 8) (ash n2 4) n3))

(defun make-register-symbol (n)
  (intern (format nil "V~X" n) :chip-8-dynarec))
