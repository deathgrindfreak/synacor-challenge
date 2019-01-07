(defpackage :constants
  (:use
   :cl)
  (:export
   :+registers-start+
   :+registers-end+)
  (:documentation "Module for common constants"))
(in-package :constants)

(defconstant +registers-start+ 32768
  "The start of the numbers signifying registers.  All values lower are plain values")

(defconstant +registers-end+ 32775
  "The end of the numbers signifying registers.  All values above are invalid")
