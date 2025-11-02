;;;; parser.lisp

(in-package #:parser)

(defun strip-spaces (line)
  (coerce (mapcan (lambda (x) (and (not (char= x #\Space)) (list x)))
                  (coerce line 'list))
          'string))

(defun literal-type (lexeme)
  (cond
    ((every #'digit-char-p lexeme) (parse-integer lexeme))
    ((every (lambda (x) (or (alpha-char-p x) (char= x #\Space))) lexeme) (strip-spaces lexeme))
    (T nil)))

(defun split-list (list separator)
  (labels ((split-aux (list current-list separator)
             (cond
               ((null list) (list (nreverse current-list)))
               ((eq (caar list) separator) (cons (nreverse current-list) (split-aux (cdr list) (list) separator)))
               (t (split-aux (cdr list) (cons (car list) current-list) separator)))))
    (split-aux list (list) separator)))

(defun parse-function (lines)
  (let* ((current-line (car lines))
         (name (cdadr current-line))
         (components (split-list current-line :then))
         (parameters (cadr (split-list (car components) :with)))
         (parameter-list (split-list parameters :and)))
    (setf current-line (cdr lines))
    (values (nconc (list 'defun (intern (strip-spaces name)) (map 'list (lambda (x) (intern (literal-type (cdar x)))) parameter-list))
                   (loop :while (not (eq (caaar current-line) :block-end))
                         :collect (setf current-line (parse-line current-line))))
            (cdr current-line))))

(defun parse-expression (expr-list)
  nil)

(defun parse-if (lines)
  (let* ((current-line (car lines))
         (current-word (cdr current-line))
         (condition (loop :while (not (eq (car current-word) :then))
                          :do (setf current-word (cdr current-word))
                          :collect (car current-word)))
         (then-block (loop :while (and current-word (not (eq (car current-word) :else)))
                           :do (setf current-word (cdr current-word))
                           :collect (car current-word)))
         (else-block (loop :while current-word
                           :do (setf current-word (cdr current-word))
                           :collect (car current-word))))
    (values (nconc (list 'if (parse-expression condition))
                   (parse-expression then-block)
                   (parse-expression else-block))
            (cdr lines))))

(defun parse-when (lines)
  (let* ((current-line (car lines))
         (current-word (cdr current-line))
         (condition (loop :while (not (eq (car current-word) :then))
                          :do (setf current-word (cdr current-word))
                          :collect (car current-word)))
         (then-block (loop :while current-word
                           :do (setf current-word (cdr current-word))
                           :collect (car current-word))))
    (values (nconc (list 'when (parse-expression condition)
                         (parse-expression then-block)))
            (cdr lines))))

(defun parse-print (lines)
  (let* ((current-line (car lines))
         (current-word (setf current-line (cdr current-line)))
         (destination (car current-word))
         (control-string (car (setf current-word (cdr current-word))))
         (value (loop :for (lexeme next) :on (cdr current-word) :by #'cddr :while (or lexeme next)
                      :if (or (not (eq (car lexeme) :literal)) (and (not (eq (car next) :and)) next))
                        :do (return)
                      :else
                        :collect (cdr lexeme)
                      :end)))
    (values (nconc (list 'format
                         (parse-expression destination)
                         (parse-expression control-string))
                   (map 'list
                        #'parse-expression
                        value))
            (cdr lines))))

(defun parse-while (lines)
  "Parse an expression of the form :while [condition]... :then [operations]..."
  (let* ((current-line (split-list (car lines) :then))
         (condition (cdar current-line))
         (operations (cdr current-line)))
    (values (list 'loop :while (parse-expression condition) :do (parse-line operations))
            (cdr lines))))

(defun parse-set (lines)
  (let* ((current-line (car lines)))
    (values (list 'setf (cadr lines) (caddr lines))
           (cdr lines))))

(defun parse-let (lines)
  (let* ((current-line (car lines))
         (current-word (car current-line)))))

(defun parse-readline (lines)
  (list 'read-line))

(defun parse-line (lines)
  (format t "~A~%" lines)
  (let* ((current-line (car lines)))
    (case (car current-line)
      (:defun (parse-function lines))
      (:while (parse-while lines))
      (:if (parse-if lines))
      (:when (parse-when lines))
      (:print (parse-print lines))
      (:read-line (parse-readline lines))
      (:set (parse-set lines))
      (:let (parse-let lines))
      (otherwise (values :parse-error :parse-error)))))

(defun parse-lines (lines)
  (loop :while lines
        :collect (multiple-value-bind (value line) (parse-line lines)
                   (setf lines line)
                   (if (eq value :parse-error)
                       (progn
                         (format t "Error parsing reached")
                         (return-from parse-lines))
                       value))))
