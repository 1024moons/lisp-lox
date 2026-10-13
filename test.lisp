(in-package :lox)

(defmacro test-eq (code expected)
  (let ((val (gensym)))
    `(let ((,val ,code))
       (if (equal ,val ,expected)
           t
           (format t "TEST FAILED ~a: EXPECTED ~a GOT ~a"
                   (prin1-to-string ',code)
                   ,expected
                   ,val)))))
