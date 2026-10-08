(in-package :lox)

(defclass parser ()
  ((toks
    :initarg :toks
    :accessor toks)
   (current
    :initarg :current
    :initform 0
    :accessor current)))

(defun check (parser type)
  (cond
	((is-at-end parser) nil)
	(t (eq (tok-type (peek parser)) type))))

(defun match (parser &rest types)
  (when (loop for type in types
              thereis (check parser type))
    (advance parser)
    t))

(defun create-parser (toks)
  (make-instance 'parser :toks toks))

(defun parse-expression (parser)
  (parse-equality parser))

(defun parse-equality (parser)
  (let ((expr (comparison parser)))
    (loop while (match parser :bang-equal :equal-equal)
          for operator = (previous parser)
          for right = (parse-comparison parser)
          do (setf expr (create-binary-expr expr operator right)))
    expr))
