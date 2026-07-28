(in-package :lox)

(defclass scanner ()
  ((source
    :initarg :source
    :accessor source
    :type string)
   (tokens
    :initform `()
    :accessor tokens
    :type list)
   (start
    :initform 0
    :accessor start
    :type integer)
   (current
    :initform 0
    :accessor current
    :type integer)
   (line
    :initform 1
    :accessor line
    :type integer)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

(defun scan-tokens (scanner)
  (loop until (is-at-end scanner)
        do (progn
             (setf (start scanner) (current scanner))
             (scan-token scanner))))

(defun is-at-end (scanner)
  (with-slots (current source) scanner
    (>= current (length source))))


(defun add-token (scanner token-type &optional literal)
  (with-slots (tokens source start current line) scanner
    (let ((lexeme (subseq source start current)))
      (alexandria:appendf tokens (list (make-token token-type lexeme literal line))))))

(defun peek (scanner)
  (if (is-at-end scanner)
      nil
      (with-slots (current source) scanner
        (elt source current))))

;  what i thought the code was
(defun advance (scanner)
  (with-slots (current source) scanner
    (incf current)
    (elt source current)))

(defun scan-token (scanner)
  (labels ((tok-if-match (expected true-tok false-tok)
             (cond
               ((is-at-end scanner) false-tok)
               ((eql (peek scanner) expected) true-tok)
               (t false-tok)))
           (consume-while-not (ch)
             (loop for c = (peek scanner)
                   until (or (not c) (char= c ch))
                   do (advance scanner))))
    (let* ((c (advance scanner))
           (token-type (case c
                         (#\( :left-paren)
                         (#\) :right-paren)
                         (#\{ :left-brace)
                         (#\} :right-brace)
                         (#\, :comma)
                         (#\. :dot)
                         (#\- :minus)
                         (#\+ :plus)
                         (#\; :semicolon)
                         (#\* :star)
                         (#\Space nil)
                         (#\Return nil)
                         (#\Tab nil)
                         (#\Newline (incf (line scanner)) nil)
                         (#\! (tok-if-match #\e :bang-equal :bang))
                         (#\= (tok-if-match #\= :equal-equal :equal))
                         (#\< (tok-if-match #\= :less-equal :less))
                         (#\> (tok-if-match #\= :greater-equal :greater))
                         (#\/ (if (char= (peek scanner) #\/)
                                  (consume-while-not #\Newline)
                                  :slash))
                         (otherwise (report-error (line scanner) "Unrecognized character ~a" c)
                          nil))))
      (if token-type (add-token scanner token-type)))))
