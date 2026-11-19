(defpackage :lox
  (:use :cl))

(in-package :lox)

(defparameter *had-error* nil)
(defun start-repl ()
  (loop for input = (read-line *standard-input* nil 'eof)
        until (or (string-equal input "quit") (eq input 'eof))
        do (print (run-string input))))


(defun run-string (input)
  (setf *had-error* nil)
  (let ((scanner (make-scanner input)))
    (scan-tokens scanner)
    (let* ((parser (create-parser (tokens scanner)))
           (expr (try-parse-expression parser)))
      (if expr
          (view-ast expr)))))
