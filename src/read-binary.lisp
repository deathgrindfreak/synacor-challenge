(defpackage :read-binary
  (:use
   :cl)
  (:export
   :read-int-u16le
   :read-program-from-file)
  (:documentation "Module for the Synacor challenge VM"))
(in-package :read-binary)

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

(defun read-program-from-file (src)
  "Reads a program from file"
  (with-open-file (in src :element-type '(unsigned-byte 8))
    (loop for addr = (read-int-u16le in)
          while addr
          collect addr)))
