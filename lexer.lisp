;;;; lexer.lisp

(in-package #:lexer)

(defparameter +lexemes+
  '(("had the same feeling as" . :equal)
    ("was completely different from" . :not-equal)
    ("was lesser than" . :less-than)
    ("was greater than" . :greater-than)
    ("was no better than" . :leq-to)
    ("was no less than" . :geq-to)
    ("merged with" . :plus)
    ("had from it exorcised" . :minus)
    ("was conflagrated by" . :times)
    ("was split into groups of" . :divide)
    ("were left as a group of those who couldn't make a group of" . :mod)
    ("observe the faraway graveyard" . :defparameter)
    ("an extra ghoul joined to" . :inc)
    ("a ghoul left from" . :dec)
    ("the ghouls, spirit world or not in" . :abs)
    ("the largest crowd in" . :max)
    ("the smallest crowd in" . :min)
    ("a blind apparition of at most" . :random)
    ("consider the graveyard" . :set)
    ("let me describe" . :let)
    ("as" . :as)
    ("recalling the fable of" . :funcall)
    ("let me tell you about a fable called" . :defun)
    ("so long as" . :while)
    ("about" . :with)
    ("with" . :with)
    ("the end" . :block-end)
    ("if only" . :if)
    ("it would be true that" . :then)
    ("but in reality" . :else)
    ("pretending that" . :when)
    ("." . :period)
    ("\"" . :quotation)
    ("but also" . :also)
    ("in addition to" . :and)
    ("but alternatively that" . :or)
    ("it is untrue that" . :not)
    ("greet" . :print)
    ("the chant" . :string)
    ("the trick-or-treater" . :true)
    ("nothingness" . :nil)
    ("the trick or treat cry" . :read-line))
  "Describes the list of possible 'keywords' in the language
along with tokens to represent their meaning")

(defparameter *keyword-tree*
  (list nil nil nil)
  "Variable storing the root of the trie that lists all the
nodes in the lookup map for a string")

;; Trie structure:
;; (char value (node1 node2 node3..))

(defun trie-insert (trie string object)
  "Insert a value in the root of a given trie"
  (if (= (length string) 0)
      (setf (cadr trie) object)
      (let ((next-char (char string 0)))
        (when (null (assoc next-char (caddr trie)))
          (push (list next-char nil nil) (caddr trie)))
        (trie-insert (assoc next-char (caddr trie)) (subseq string 1) object))))

(defun trie-lookup (trie string &optional (depth 0))
  "Lookup the value matching the longest prefix of string"
  (if (= (length string) 0)
      (values (cadr trie) depth)
      (let* ((next-char (char string 0))
             (next-trie (assoc next-char (caddr trie))))
        (if (null next-trie)
            (values (cadr trie) depth)
            (trie-lookup next-trie (subseq string 1) (1+ depth))))))


(defun build-lexeme-lookup ()
  "Build the lookup trie for the lexer, depending on the list of
string/symbol pairs in +lexemes+"
  (loop :for (string . lexeme) :in +lexemes+
        :do
           (trie-insert *keyword-tree* string lexeme)))

(defun lex-string (line)
  "Get the value of a string until the next quotation mark"
  (dotimes (i (length line))
    (when (char= (char line i) #\")
      (return-from lex-string (values (subseq line 0 i) (1+ i)))))
  (values nil -1))

(defun lex-line (line)
  "Get the value of a line"
  (cond
    ((string= line "") nil)
    ((char= (char line 0) #\") (multiple-value-bind (string-val index) (lex-string (subseq line 1))
                                 (cons (cons :literal string-val)
                                       (lex-line (subseq line
                                                         (min (+ 2 index)
                                                              (length line)))))))
    (T (multiple-value-bind (lexeme index) (trie-lookup *keyword-tree* line)
         (let ((remaining (lex-line (subseq line (min (1+ index) (length line))))))
           (if (eq lexeme :string)
               (cons (cons lexeme (cdar remaining))
                     (cdr remaining))
               (cons (cons lexeme nil)
                     remaining)))))))

(defun lex-lines (lines)
  "Given a list of lines, return a list of those lines, tokenized"
  (map 'list
       #'lex-line
       lines))

(defun load-file (filename)
  "Load an entire file as a string"
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun build-lines (words)
  "Separate period-delimited lines into different strings in a list"
  (labels ((build-next-line (words current)
             (if (null words)
                 (if (string= current "") nil (list (subseq current 1)))
                 (let ((new-line (concatenate 'string current " " (car words))))
                   (if (char= (char new-line (1- (length new-line))) #\.)
                       (cons (subseq new-line 1 (1- (length new-line))) (build-next-line (cdr words) ""))
                       (build-next-line (cdr words) new-line))))))
    (build-next-line words "")))

(defun clean-file (file-string)
  "Break file apart by spaces into words"
  (let ((words (uiop:split-string file-string :separator '(#\Space #\Newline))))
    (build-lines (mapcan (lambda (x) (and (not (string= x "")) (list x)))
                         words))))
