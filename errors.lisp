(in-package :lox)
(defparameter *had-error* nil)

(defmacro report-error (line msg &rest rest)
  `(progn
     (format t "[Line ~a] " ,line)
     (format t ,msg ,@rest)
     (setf *had-error* t)))
