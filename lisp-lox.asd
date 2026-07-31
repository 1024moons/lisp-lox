(asdf:defsystem "lisp-lox"
  :description "An implementation of the simple tree-walking interpreter in common lisp"
  :author "Talha Qamar"
  :depends-on ("alexandria" "parse-float")
  :components ((:file "lox")
               (:file "expr")
               (:file "tokens" :depends-on ("lox"))
               (:file "scanner" :depends-on ("lox" "tokens"))))
