(defpackage :vm
  (:use
   :cl)
  (:export
   :+registers-start+
   :+registers-end+
   :machine
   :program
   :pc
   :register
   :stack
   :get-register
   :set-register
   :get-address
   :set-address
   :inc-pc
   :value-or-register
   :get-instruction-method
   :run-program-from-file
   :read-program-from-file)
  (:documentation "Module for the Synacor challenge VM"))
(in-package :vm)

(defconstant +registers-start+ 32768
  "The start of the numbers signifying registers.  All values lower are plain values")

(defconstant +registers-end+ 32775
  "The end of the numbers signifying registers.  All values above are invalid")

(defun read-int-u16le (in)
  "Read an unsigned 16-bit little endian integer"
  (let ((lb (read-byte in nil nil))
        (hb (read-byte in nil nil))
        (u2 0))
    (and lb hb
         (progn
           (setf (ldb (byte 8 0) u2) lb)
           (setf (ldb (byte 8 8) u2) hb)
           u2))))

(defclass machine ()
  ((program
    :initarg :program
    :accessor program
    :documentation "The program to run in memory")
   (pc
    :initform 0
    :accessor pc
    :documentation "Program counter")
   (registers
    :initform (loop for r from +registers-start+ to +registers-end+ collect (list r 0))
    :accessor registers
    :documentation "List of registers")
   (stack
    :initform nil
    :accessor stack
    :documentation "The program stack")))

(defmethod inc-pc ((m machine) &optional (a 1)) (incf (pc m) a))

(defmethod set-register ((m machine) register value)
  (setf (second (assoc register (registers m))) value))

(defmethod get-register ((m machine) register)
  (second (assoc register (registers m))))

(defmethod get-address ((m machine) address) (elt (program m) address))

(defmethod set-address ((m machine) address value) (setf (elt (program m) address) value))

(defmethod value-or-register ((m machine) v)
  "Either returns a value or the value of a register depending on the value of v"
  (cond ((<= 0 v (1- +registers-start+)) v)
        ((<= +registers-start+ v +registers-end+) (get-register m v))
        (t (error (format nil "Improper value: ~a" v)))))

(defmacro definstr (machine arg-list lookup-list &body body)
  "Returns a function with the number of args to be called
variables in the arg-list are passed in as-is
variables in the lookup list are pulled from a register or passed in depending on the value"
  `(values #'(lambda ,(append arg-list lookup-list)
               (progn
                 ,@(mapcar #'(lambda (x)
                              `(setf ,x (value-or-register ,machine ,x)))
                          lookup-list)
                 ,@body))
           ,(+ (length arg-list)
               (length lookup-list))))

(defmacro def-reg-instr (machine arg-list lookup-list &body body)
  "Returns a function defined by definstr and increments the program counter"
  `(definstr ,machine ,arg-list ,lookup-list
     (progn
       ,@body
       (inc-pc ,machine))))

(defmethod get-instruction-method ((m machine) instr)
  "Returns the method to call in order to run the instruction in the VM"
  (case instr
    (0 (values (lambda () 'halt) 0))

    (1 (def-reg-instr m (a) (b)
         (set-register m a b)))

    (2 (def-reg-instr m () (a)
         (push a (stack m))))

    (3 (def-reg-instr m (a) ()
         (if (null (stack m))
             (error "Stack is empty!")
             (set-register m a (pop (stack m))))))

    (4 (def-reg-instr m (a) (b c)
         (set-register m a (if (= b c) 1 0))))

    (5 (def-reg-instr m (a) (b c)
         (set-register m a (if (> b c) 1 0))))

    (6 (definstr m () (a)
           (setf (pc m) a)))

    (7 (definstr m () (a b)
         (if (zerop a)
             (inc-pc m)
             (setf (pc m) b))))

    (8 (definstr m () (a b)
         (if (zerop a)
             (setf (pc m) b)
             (inc-pc m))))

    (9 (def-reg-instr m (a) (b c)
         (set-register m a (mod (+ b c) +registers-start+))))

    (10 (def-reg-instr m (a) (b c)
          (set-register m a (mod (* b c) +registers-start+))))

    (11 (def-reg-instr m (a) (b c)
          (set-register m a (mod b c))))

    (12 (def-reg-instr m (a) (b c)
          (set-register m a (logand b c))))

    (13 (def-reg-instr m (a) (b c)
          (set-register m a (logior b c))))

    (14 (def-reg-instr m (a) (b)
          (set-register m a (lognot b))))

    (15 (def-reg-instr m (a) (b)
          (set-register m a (get-address m b))))

    (16 (def-reg-instr m () (a b)
          (set-address m a b)))

    (17 (definstr m () (a)
          (push (1+ (pc m)) (stack m))
          (setf (pc m) a)))

    (19 (def-reg-instr m (a) ()
          (format t "~a" (code-char a))))

    (20 (def-reg-instr m (a) ()
          (set-register m a (char-code (elt (read-line) 0)))))

    (21 (def-reg-instr m () () ()))))

(defun run (machine)
  machine)

(defun read-program-from-file (src)
  (with-open-file (in src :element-type '(unsigned-byte 8))
    (apply #'vector
           (loop for addr = (read-int-u16le in)
                 while addr
                 collect addr))))

(defun run-program-from-file (src)
  "Runs a program from a file"
  (run (make-instance 'machine :program (read-program-from-file src))))
