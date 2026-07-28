(defpackage :lox
  (:use :cl))

(in-package :lox)

(defparameter *had-error* nil)

(defun start-repl ()
  (loop for input = (read-line *standard-input* nil 'eof)
        until (or (string-equal input "quit") (eq input 'eof))
        do (run-string input)))

(defun report-error (line msg &optional where)
  (format t "[Line ~a] ~a: ~a" line where msg)
  (setf *had-error* t))

(defun run-string (input)
  (setf *had-error* nil))
