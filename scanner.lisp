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
  (peek-n scanner 0))

(defun peek-next (scanner)
  (peek-n scanner 1))

(defun peek-n (scanner n)
  (if (is-at-end scanner)
      nil
      (with-slots (current source) scanner
        (elt source (+ n current)))))

(defun advance (scanner)
  (with-slots (current source) scanner
    (incf current)
    (elt source (1- current))))

(defun is-char-digit (ch)
  (char<= #\0 ch #\9))

(defun scan-token (scanner)
  (labels ((tok-if-match (expected true-tok false-tok)
             (cond
               ((is-at-end scanner) false-tok)
               ((eql (peek scanner) expected) true-tok)
               (t false-tok)))
           (consume-while-not (ch)
             (consume-while-pred #'(lambda (c) (char= c ch))))
           (consume-while-pred (pred)
             (loop for c = (peek scanner)
                   until (or (not c) (not (funcall pred c)))
                   when (char= c #\Newline)
                     do (incf (line scanner))
                   do (advance scanner)))
           (consume-string ()
             (consume-while-not #\")
             (if (is-at-end scanner)
                 (report-error (line scanner) "Unterminated string"))
             (advance scanner)
             (add-token scanner :string (with-slots (source start current) scanner
                                          (subseq source (1+ start) (1- current)))))
           (consume-number ()
             (consume-while-pred #'is-char-digit)
             (when (and (not (is-at-end scanner))
                        (char= (peek scanner) #\.)
                        (is-char-digit (peek-next scanner)))
               (advance scanner)
               (consume-while-pred #'is-char-digit))
             (add-token scanner :num (with-slots (source start current) scanner
                                       (parse-float:parse-float source :start start :end current)))))
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
                         (#\" (consume-string))
                         (#\Newline (incf (line scanner)) nil)
                         (#\! (tok-if-match #\e :bang-equal :bang))
                         (#\= (tok-if-match #\= :equal-equal :equal))
                         (#\< (tok-if-match #\= :less-equal :less))
                         (#\> (tok-if-match #\= :greater-equal :greater))
                         (#\/ (if (char= (peek scanner) #\/)
                                  (consume-while-not #\Newline)
                                  :slash))
                         (otherwise (if (is-char-digit c)
                                        (consume-number)
                                        (report-error (line scanner) "Unrecognized character ~a" c))
                          nil))))
      (if token-type (add-token scanner token-type)))))
