(in-package :lox)

(defmacro defexpr (name &rest slots)
  (let ((constructor-name (intern (format nil "CREATE-~A" name))))
    `(progn
       (defclass ,name ()
         ,(mapcar (lambda (slot)
                    `(,slot
                      :initarg ,(intern (symbol-name slot) :keyword)
                      :accessor ,slot))
                  slots))
       (defun ,constructor-name (,@slots)
         (make-instance ',name
                        ,@(mapcan (lambda (slot)
                                    (list (intern (symbol-name slot) :keyword) slot))
                                  slots))))))


(defexpr binary-expr left right operator-token)
(defexpr unary-expr right operator-token)
(defexpr literal-expr val)
(defexpr grouping-expr expr)

(defgeneric view-ast (expr)
  (:documentation "Returns a human readable string representation of the AST"))

(defmethod view-ast ((expr grouping-expr))
  (format nil "(~a)" (view-ast (expr expr))))

(defmethod view-ast ((expr unary-expr))
  (format nil "~a~a" (lexeme (operator-token expr)) (view-ast (right expr))))

(defmethod view-ast ((expr binary-expr))
  (format nil "~a ~a ~a" (view-ast (left expr)) (lexeme (operator-token expr)) (view-ast (right expr))))
