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
(defmethod peek-n ((stream scanner) n)
   (if (is-at-end stream)
       nil
       (with-slots (current source) stream
         (elt source (+ n current)))))
 
(defmethod advance ((stream scanner))
  (with-slots (line current source) stream
     (incf current)
    (let ((ch (elt source (1- current))))
      (if (char= ch #\Newline)
          (incf line))
      ch)))
 
 (defmethod is-at-end ((stream scanner))
   (with-slots (current source) stream
     (>= current (length source))))
 
(defun scan-tokens (scanner)
  (loop until (is-at-end scanner)
        do (progn
             (setf (start scanner) (current scanner))
             (scan-token scanner))))

(defun reserved-words (word)
  (alexandria:switch (word :test #'string=)
                     ("and" :and)
                     ("class" :class)
                     ("else" :else)
                     ("false" :false)
                     ("for" :for)
                     ("fun" :fun)
                     ("if" :if)
                     ("nil" :nil)
                     ("or" :or)
                     ("print" :print)
                     ("return" :return)
                     ("super" :super)
                     ("this" :this)
                     ("true" :true)
                     ("var" :var)
                     ("while" :while)
                     (otherwise :identifier)))

(defun scan-string (source)
  (let ((scanner (make-scanner source)))
    (scan-tokens scanner)
    (tokens scanner)))

;; Utility function for testing
(defun scan-string-to-string-list (source)
  (mapcar #'token-to-string (scan-string source)))

(defun add-token (scanner token-type &optional literal)
  (with-slots (tokens source start current line) scanner
    (let ((lexeme (subseq source start current)))
      (alexandria:appendf tokens (list (make-token token-type lexeme literal line))))))

(defun is-char-digit (ch)
  (char<= #\0 ch #\9))

(defun is-char-alphanum (ch)
  (or (is-char-digit ch) (is-char-alpha ch)))

(defun is-char-alpha (ch)
  (or (char<= #\a ch #\z)
      (char<= #\A ch #\Z)))


(defun consume-string (scanner)
             (consume-while-not scanner #\")
             (if (is-at-end scanner)
                 (report-error (line scanner) "Unterminated string")
                 (progn
                   (advance scanner)
                   (add-token scanner :string (with-slots (source start current) scanner
                                                (subseq source (1+ start) (1- current)))))))
(defun consume-number (scanner)
             (consume-while-pred scanner #'is-char-digit)
             (when (and (not (is-at-end scanner))
                        (char= (peek scanner) #\.)
                        (is-char-digit (peek-next scanner)))
               (advance scanner)
               (consume-while-pred scanner #'is-char-digit))
             (add-token scanner :num (with-slots (source start current) scanner
                                       (parse-float:parse-float source :start start :end current))))
(defun consume-identifier (scanner)
  (consume-while-pred scanner (lambda (c) (or (is-char-alphanum c)
                                              (char= c #\_))))
             (add-token scanner (reserved-words (with-slots (source current start) scanner
                                                  (subseq source start current)))))

(defun scan-token (scanner)
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
                       (#\" (consume-string scanner) nil)
                       (#\Newline nil) ; advance method increments our lines for us
                       (#\! (if-match scanner #\= :bang-equal :bang))
                       (#\= (if-match scanner #\= :equal-equal :equal))
                       (#\< (if-match scanner #\= :less-equal :less))
                       (#\> (if-match scanner #\= :greater-equal :greater))
                       (#\/ (if (and (peek scanner) (char= (peek scanner) #\/))
                                (consume-while-not scanner #\Newline)
                                :slash))
                       (otherwise (cond ((is-char-digit c) (consume-number scanner))
                                        ((or (is-char-alpha c) (char= c #\_)) (consume-identifier scanner))
                                        (t (report-error (line scanner) "Unrecognized character ~a" c)))
                        nil))))
    (if token-type (add-token scanner token-type))))

;;;; TESTS ---------------------------------
(test-eq (scan-string-to-string-list "( ) {} , . - + ; * /")
         '("LEFT-PAREN ( NIL" "RIGHT-PAREN ) NIL"
           "LEFT-BRACE { NIL" "RIGHT-BRACE } NIL" "COMMA , NIL"
           "DOT . NIL" "MINUS - NIL" "PLUS + NIL" "SEMICOLON ; NIL"
           "STAR * NIL" "SLASH / NIL"))

(test-eq (scan-string-to-string-list "!= == <= >= // A comment")
         '("BANG-EQUAL != NIL"
          "EQUAL-EQUAL == NIL"
          "LESS-EQUAL <= NIL"
          "GREATER-EQUAL >= NIL"))

(test-eq (scan-string-to-string-list
          " and
            class
            else
            false
            for
            fun
            if
            nil
            or
            foo
            bar
            print
            return
            super
            this
            true
            var
            while ")
         '("AND and NIL" "CLASS class NIL" "ELSE else NIL" "FALSE false NIL"
           "FOR for NIL" "FUN fun NIL" "IF if NIL" "NIL nil NIL" "OR or NIL"
           "IDENTIFIER foo NIL" "IDENTIFIER bar NIL"
           "PRINT print NIL" "RETURN return NIL" "SUPER super NIL" "THIS this NIL"
           "TRUE true NIL" "VAR var NIL" "WHILE while NIL"))

(test-eq (scan-string-to-string-list "55.0 67")
         '("NUM 55.0 55.0" "NUM 67 67.0"))

(test-eq (scan-string-to-string-list "\" CRAZY \"")
         '("STRING \" CRAZY \"  CRAZY "))
