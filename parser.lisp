(in-package :lox)

(defclass parser ()
  ((toks
    :initarg :toks
    :accessor toks)
   (current
    :initarg :current
    :initform 0
    :accessor current)))

(defun create-parser (toks)
  (make-instance 'parser :toks toks))

(defun parse-expression (parser)
  (parse-equality parser))

;(defun parse-equality (parser)
;  (let ((expr (comparison parser)))
;    (loop for operator = (previous parser)
;          for right = (parse-comparison parser)
;          do (setf expr)
;      while (match parser :bang-equal :equal-equal))))
