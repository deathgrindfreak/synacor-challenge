(defpackage :vm
  (:use
   :cl
   :constants
   :read-binary)
  (:export
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
   :call-instruction
   :run-program
   :run-program-from-file
   :read-program-from-file)
  (:documentation "Module for the Synacor challenge VM"))
(in-package :vm)

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

(defun make-machine (program)
  "Make a machine instance from a program.  Program should be a list"
  (make-instance 'machine :program (apply #'vector program)))

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
     (let ((b (progn ,@body)))
       (progn
         (inc-pc ,machine ,(+ 1 (length arg-list) (length lookup-list)))
         b))))

(defmacro def-math-instr (machine (reg) lookup-list op)
  "Returns a function that operates with a math function mod 32768"
  `(def-reg-instr ,machine (,reg) ,lookup-list
     (set-register ,machine ,reg
                   (mod (funcall ,op ,@lookup-list) +registers-start+))))

(defmethod get-instruction-method ((m machine) op &optional (s *standard-input*))
  "Returns the method to call in order to run the instruction in the VM"
  (case op
    (0 (def-reg-instr m () () 'halt))

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
             (inc-pc m 3)
             (setf (pc m) b))))

    (8 (definstr m () (a b)
         (if (zerop a)
             (setf (pc m) b)
             (inc-pc m 3))))

    (9 (def-math-instr m (a) (b c) #'+))

    (10 (def-math-instr m (a) (b c) #'*))

    (11 (def-math-instr m (a) (b c) #'mod))

    (12 (def-math-instr m (a) (b c) #'logand))

    (13 (def-math-instr m (a) (b c) #'logior))

    (14 (def-math-instr m (a) (b) #'lognot))

    (15 (def-reg-instr m (a) (b)
          (set-register m a (get-address m b))))

    (16 (def-reg-instr m () (a b)
          (set-address m a b)))

    (17 (definstr m () (a)
          (push (+ 2 (pc m)) (stack m))
          (setf (pc m) a)))

    (18 (definstr m () ()
          (if (null (stack m))
              'halt
              (let ((top (pop (stack m))))
                (setf (pc m) top)))))

    (19 (def-reg-instr m () (a)
          (format t "~a" (code-char a))))

    (20 (def-reg-instr m (a) ()
          (let ((ch (read-char s nil :eof)))
            ;; If :eof is encountered, set the stream to stdin and read a char from there
            (when (eq ch :eof)
              (setf s *standard-input*
                    ch (read-char s)))
            (set-register m a (char-code ch)))))

    (21 (def-reg-instr m () ()))

    (otherwise (error "Bad op code: 0x~x" op))))

(defmethod call-instruction ((m machine) instruction &optional (s *standard-input*))
  "Call the instruction on the running machine"
  (multiple-value-bind (instr-fun num-args)
      (get-instruction-method m instruction s)
    (let* ((curr-pc (pc m))
           (args (subseq (program m) (1+ curr-pc) (+ 1 num-args curr-pc))))
      (apply instr-fun (coerce args 'list)))))

(defun run-program (machine &optional (s *standard-input*))
  "Runs a program till it halts"
  (loop for curr-instr = (elt (program machine) (pc machine))
     for instr-call = (call-instruction machine curr-instr s)
     when (or (>= (pc machine)
                  (length (program machine)))
              (eq instr-call 'halt))
     return nil))

(defun run-program-from-file (src &optional (s *standard-input*))
  "Runs a program from a file"
  (run-program (make-machine (read-program-from-file src)) s))

(defun run (&optional (challenge-program "../bin/challenge.bin") script)
  "Run the included program"
  (if script
      (with-open-file (in script)
        (run-program-from-file challenge-program in))
      (run-program-from-file challenge-program)))
