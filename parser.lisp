(in-package :lox)

(defclass parser ()
  ((toks
    :initarg :toks
    :accessor toks)
   (current
    :initarg :current
    :initform 0
    :accessor current)))


(defmethod peek-n ((stream parser) n)
  (if (is-at-end stream)
      nil
      (with-slots (current toks) stream
        (elt toks (+ n current)))))

(defmethod advance ((stream parser))
  (with-slots (current toks) stream
    (incf current)
    (elt toks (1- current))))

(defmethod is-at-end ((stream parser))
  (with-slots (current toks) stream
    (>= current (length toks))))

(defun check (parser type)
  (cond
    ((is-at-end parser) nil)
    (t (eq (tok-type (peek parser)) type))))

(defun previous (parser)
  (with-slots (toks current) parser
      (elt toks (1- current))))

(defun match (parser &rest types)
  (when (loop for type in types
              thereis (check parser type))
    (advance parser)
    t))

(defun create-parser (toks)
  (make-instance 'parser :toks toks))

(defun parse-expression (parser)
  (parse-equality parser))

(defun try-parse-expression (parser)
  (handler-case
      (parse-expression parser)
    (parser-error (c)
      nil)))

(defmacro create-left-assoc-binary-parser (name higher-rule &rest operators)
  `(defun ,name (parser)
     (let ((expr (,higher-rule parser)))
       (loop while (match parser ,@operators)
             for operator = (previous parser)
             for right = (,higher-rule parser)
             do (setf expr (create-binary-expr expr right operator )))
       expr)))

(create-left-assoc-binary-parser parse-equality
                                 parse-comparison
                                 :bang-equal :equal-equal)

(create-left-assoc-binary-parser parse-comparison
                                 parse-term
                                 :greater-equal :greater :less-equal :less)

(create-left-assoc-binary-parser parse-term
                                 parse-factor
                                 :minus :plus)

(create-left-assoc-binary-parser parse-factor
                                 parse-unary
                                 :slash :star)

(defun parse-unary (parser)
  (if (match parser :minus :bang)
      (let ((operator (previous parser))
            (right (parse-unary parser)))
        (create-unary-expr right operator))
      (parse-primary parser)))

(defun parse-primary (parser)
  (cond
    ((match parser :false) (create-literal-expr :false))
    ((match parser :true)  (create-literal-expr :true))
    ((match parser :nil)   (create-literal-expr :nil))
    ((match parser :num :string) (create-literal-expr (tok-literal (previous parser))))
    ((match parser :left-paren) (let ((expr (parse-expression parser)))
                           (next-is parser :right-paren "Expected a closing )")
                           (create-grouping-expr expr)))
    (t (error (create-parser-error (peek parser) "Expected an expression")))))

(define-condition parser-error (error)
  ())

(defun create-parser-error (tok message)
  (if (eq (tok-type tok) :eof)
      (report-error (tok-line tok) "at end: ~a"  message)
      (report-error (tok-line tok) "at ~a: ~a" (lexeme tok) message))
  (make-condition 'parser-error))

;; This is my version of Nystrom's consume method
;; Because i'm doing this all in one namespace I find that
;; the name "consume" shares too much with the lexer functions
;; when conceptually this is a much stricter operation
;; Hence, the new name, this can be considered a type of assert
(defun next-is (parser type &optional err)
  (if (check parser type)
      (advance parser)
      (error (create-parser-error (peek parser) err))))

