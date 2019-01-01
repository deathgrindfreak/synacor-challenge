(ql:quickload :FiveAM)

(defpackage :vm.tests
  (:use
   :cl
   :fiveam
   :vm)
  (:documentation "Tests for vm package"))
(in-package :vm.tests)

(def-suite vm-suite :description "VM test suite")
(in-suite vm-suite)

(defun make-test-machine ()
  (make-instance 'machine
                 :program (list 9 32768 32769 4 19 32768)))

(test inc-pc-test
  "Test incrementing pc properly"
  (let ((m (make-test-machine)))
    (is (= 0 (pc m)))

    (inc-pc m)
    (is (= 1 (pc m)))

    (inc-pc m 2)
    (is (= 3 (pc m)))))

(test set-get-register
  "Test getting and setting of registers"
  (let ((m (make-test-machine)))
    (is (= 0 (get-register m +registers-start+)))

    (set-register m +registers-start+ 1)
    (is (= 1 (get-register m +registers-start+)))))

(test value-or-register
  "Should return a value or the value in a register"
  (let ((m (make-test-machine)))
    (is (= 12 (value-or-register m 12)))

    (set-register m +registers-start+ 100)
    (is (= 100 (value-or-register m +registers-start+)))))

(defmacro test-instruction (instr-num (machine-var instr-fun-var num-args-var) doc-str &body body)
  `(test ,(read-from-string (concatenate 'string "instruction-" (write-to-string instr-num)))
     ,doc-str
     (let ((,machine-var (make-test-machine)))
       (multiple-value-bind (,instr-fun-var ,num-args-var)
           (get-instruction-method ,machine-var ,instr-num)
         ,@body))))

(test-instruction 0 (m instr num-args)
  "Test the halt instruction"
  (is (= 0 num-args))
  (is (eq 'halt (funcall instr))))

(test-instruction 1 (m instr num-args)
  "Test the set instruction"
  (is (= 2 num-args))

  (funcall instr +registers-start+ 100)
  (is (= 100 (get-register m +registers-start+))))

(test-instruction 2 (m instr num-args)
  "Test the push instruction"
  (is (= 1 num-args))

  (funcall instr 100)
  (is (equal '(100) (stack m))))

(test-instruction 3 (m instr num-args)
  "Test the pop instruction"
  (is (= 1 num-args))

  ;; Set the stack
  (setf (stack m) '(100))

  (funcall instr +registers-start+)
  (is (= 100 (get-register m +registers-start+)))

  (is (null (stack m)))

  ;; Should throw an error for empty stack
  (handler-case (funcall instr +registers-start+)
    (error (e) (is (eq 'simple-error (type-of e))))))

(test-instruction 4 (m instr num-args)
  "Test the eq instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 1 2)
  (is (= 0 (get-register m +registers-start+)))

  (funcall instr +registers-start+ 1 1)
  (is (= 1 (get-register m +registers-start+)))

  ;; Should work with registers as well
  (set-register m (1+ +registers-start+) 100)
  (set-register m (+ 2 +registers-start+) 100)

  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 1 (get-register m +registers-start+))))

(test-instruction 5 (m instr num-args)
  "Test the gt instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 1 2)
  (is (= 0 (get-register m +registers-start+)))

  (funcall instr +registers-start+ 1 1)
  (is (= 0 (get-register m +registers-start+)))

  (funcall instr +registers-start+ 2 1)
  (is (= 1 (get-register m +registers-start+)))

  ;; Should work with registers as well
  (set-register m (1+ +registers-start+) 101)
  (set-register m (+ 2 +registers-start+) 100)

  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 1 (get-register m +registers-start+))))

(test-instruction 6 (m instr num-args)
  "Test the jmp instruction"
  (is (= 1 num-args))

  (funcall instr 4)
  (is (= 4 (pc m)))

  (set-register m +registers-start+ 100)
  (funcall instr +registers-start+)
  (is (= 100 (pc m))))

(test-instruction 7 (m instr num-args)
  "Test the jt instruction"
  (is (= 2 num-args))

  (funcall instr 0 4)
  (is (= 1 (pc m)))

  (funcall instr 1 4)
  (is (= 4 (pc m)))

  (set-register m +registers-start+ 1)
  (funcall instr +registers-start+ 100)
  (is (= 100 (pc m))))

(test-instruction 8 (m instr num-args)
  "Test the jf instruction"
  (is (= 2 num-args))

  (funcall instr 0 4)
  (is (= 4 (pc m)))

  (funcall instr 1 4)
  (is (= 5 (pc m)))

  (set-register m +registers-start+ 0)
  (funcall instr +registers-start+ 100)
  (is (= 100 (pc m))))

(test-instruction 9 (m instr num-args)
  "Test the add instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 2 2)
  (is (= 4 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 2)
  (set-register m (+ 2 +registers-start+) 10)
  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 12 (get-register m +registers-start+))))

(test-instruction 10 (m instr num-args)
  "Test the mult instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 10 10)
  (is (= 100 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 2)
  (set-register m (+ 2 +registers-start+) 10)
  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 20 (get-register m +registers-start+))))

(test-instruction 11 (m instr num-args)
  "Test the mod instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 3 2)
  (is (= 1 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 15)
  (set-register m (+ 2 +registers-start+) 4)
  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 3 (get-register m +registers-start+))))

(test-instruction 12 (m instr num-args)
  "Test the and instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 3 2)
  (is (= 2 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 15)
  (set-register m (+ 2 +registers-start+) 4)
  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 4 (get-register m +registers-start+))))

(test-instruction 13 (m instr num-args)
  "Test the or instruction"
  (is (= 3 num-args))

  (funcall instr +registers-start+ 3 2)
  (is (= 3 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 15)
  (set-register m (+ 2 +registers-start+) 4)
  (funcall instr +registers-start+
           (1+ +registers-start+) (+ 2 +registers-start+))
  (is (= 15 (get-register m +registers-start+))))

(test-instruction 14 (m instr num-args)
  "Test the not instruction"
  (is (= 2 num-args))

  (funcall instr +registers-start+ 3)
  (is (= -4 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 15)
  (funcall instr +registers-start+ (1+ +registers-start+))
  (is (= -16 (get-register m +registers-start+))))

(test-instruction 15 (m instr num-args)
  "Test the rmem instruction"
  (is (= 2 num-args))

  (funcall instr +registers-start+ 3)
  (is (= 4 (get-register m +registers-start+)))

  (set-register m (1+ +registers-start+) 4)
  (funcall instr +registers-start+ (1+ +registers-start+))
  (is (= 19 (get-register m +registers-start+))))
