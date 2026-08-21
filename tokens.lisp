(in-package :lox)


; This technically doesn't need to exist, but I'm
; trying to be as faithful as possible to the original text
; and I think it serves as useful documentation.
(deftype token-type ()
  `(member
    ; single character tokens
    :left-paren :right-paren :left-brace :right-brace
    :comma :dot :plus :minus :slash :star :semicolon

    ; one or two character tokens
    :bang :bang-equal :equal :equal-equal
    :greater :greater-equal :less :less-equal

    ; literals
    :identifier :string :num

    ; keywords
    :and :class :else :false
    :fun :for :if :nil
    :or :print :return :super
    :this :true :var :while

    :eof))


(defclass token ()
  ((tok-type
    :initarg :tok-type
    :accessor tok-type
    :type token-type)
   (lexeme
    :initarg :lexeme
    :accessor lexeme
    :type string)
   (literal
    :initarg :literal
    :accessor tok-literal)
   (line
    :initarg :line
    :accessor tok-line
    :type integer)))

(defun token-to-string (token)
  (with-slots (tok-type lexeme literal) token
      (format nil "~a ~a ~a" tok-type lexeme literal)))

(defun make-token (tok-type lexeme literal line)
  (make-instance 'token :tok-type tok-type :lexeme lexeme :literal literal :line line))
