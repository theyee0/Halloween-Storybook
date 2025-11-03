;;;; halloween-storybook.asd

(asdf:defsystem #:halloween-storybook
  :description "Describe halloween-storybook here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "lexer")
               (:file "parser")
               (:file "halloween-storybook")))
