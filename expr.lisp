(in-package :lox)

(defclass binary-expr ()
  ((left
    :initarg :left
    :accessor left)
   (right
    :initarg :right
    :accessor right)
   (operator-token
    :initarg :operator-token
    :accessor operator-token)))

(defclass unary-expr ()
  ((right
    :initarg :right
    :accessor right)
   (operator-token
    :initarg :operator-token
    :accessor operator-token)))

(defclass literal ()
  ((val
    :initarg val
    :accessor val)))

(defclass groping ()
  ((expr
    :initarg expr
    :accessor expr)))
