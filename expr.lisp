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
