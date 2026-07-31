(defpackage :lox
  (:use :cl))

(in-package :lox)

(defparameter *had-error* nil)

(defun start-repl ()
  (loop for input = (read-line *standard-input* nil 'eof)
        until (or (string-equal input "quit") (eq input 'eof))
        do (print (run-string input))))

(defmacro report-error (line msg &rest rest)
  `(progn
     (format t "[Line ~a] " ,line)
     (format t ,msg ,@rest)
     (setf *had-error* t)))

(defun run-string (input)
  (setf *had-error* nil)
  (let ((scanner (make-scanner input)))
    (scan-tokens scanner)
    (map 'list #'token-to-string (tokens scanner))))
