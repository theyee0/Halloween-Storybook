;;;; halloween-storybook.lisp

(in-package #:halloween-storybook)

(defun dev-script (&rest args)
  (when (/= (length args) 1)
    (format t "Expected exactly one argument: filename~%")
    (return-from dev-script))
  (lexer:build-lexeme-lookup)
  (let* ((file-contents (lexer:load-file (car args)))
         (file-lines (lexer:clean-file file-contents))
         (lexed-lines (lexer:lex-lines file-lines))
         (parsed-lines (parser:parse-lines lexed-lines)))
    (pprint lexed-lines)
    (pprint parsed-lines)))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (when (/= (length args) 2)
      (format t "Expected exactly two arguments: [INPUT FILENAME] [OUTPUT FILENAME]~%")
      (return-from main))
    (lexer:build-lexeme-lookup)
    (let* ((file-contents (lexer:load-file (car args)))
           (file-lines (lexer:clean-file file-contents))
           (lexed-lines (lexer:lex-lines file-lines))
           (parsed-lines (parser:parse-lines lexed-lines)))
      (with-open-file (output (cadr args)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (loop :for line :in parsed-lines
              :do (format output "~S~%" line)))
      (pprint parsed-lines))))
