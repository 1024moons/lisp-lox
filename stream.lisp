(in-package :lox)
(defgeneric advance (stream)
  (:documentation "Consume an element from the stream"))

(defgeneric is-at-end (stream)
  (:documentation "returns whether the stream is entirely consumed"))

(defgeneric peek-n (stream n)
  ( :documentation "peeks ahead in the stream"))

(defun if-match (stream expected true-val false-val)
             (cond
               ((is-at-end stream) false-val)
               ((eql (peek stream) expected) (progn
                                               (advance stream)
                                               true-val))
               (t false-val)))

(defun consume-while-pred (stream pred)
             (loop for e = (peek stream)
                   until (or (not e) (not (funcall pred e)))
                   do (advance stream)))
(defun consume-while-not (stream val)
             (consume-while-pred stream #'(lambda (x) (not (eql x val)))))

(defun peek (stream)
  (peek-n stream 0))

(defun peek-next (stream)
  (peek-n stream 1))
