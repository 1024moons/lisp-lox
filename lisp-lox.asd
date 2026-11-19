(asdf:defsystem "lisp-lox"
  :description "An implementation of the simple tree-walking interpreter in common lisp"
  :author "Talha Qamar"
  :depends-on ("alexandria" "parse-float")
  :components ((:file "lox")
               (:file "expr")
               (:file "test")
               (:file "stream")
               (:file "errors")
               (:file "tokens" :depends-on ("lox"))
               (:file "parser" :depends-on ("expr" "tokens" "errors" "scanner"))
               (:file "scanner" :depends-on ("stream" "tokens" "test" "errors"))))
