;;;; package.lisp

(defpackage #:lexer
  (:use #:cl)
  (:export #:trie-insert #:trie-lookup #:build-lexeme-lookup #:lex-string #:lex-lines
           #:load-file #:build-lines #:clean-file))

(defpackage #:parser
  (:use #:cl)
  (:export #:get-literal #:parse-function #:parse-funcall
           #:get-precedence #:eval-rpn-stack #:parse-expression
           #:parse-if #:parse-when #:parse-print #:parse-while
           #:parse-string #:parse-set #:parse-let #:parse-readline
           #:parse-line #:parse-lines))

(defpackage #:halloween-storybook
  (:use #:cl #:lexer #:parser))
