;;; templatel --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-error 'templatel-syntax-error "Syntax Error" 'templatel-error)

(defun scanner/new (input)
  "Create scanner for INPUT."
  (list input 0))

(defun scanner/input (scanner)
  "Input that SCANNER is operating on."
  (car scanner))

(defun scanner/cursor (scanner)
  "Cursor position of SCANNER."
  (cadr scanner))

(defun scanner/cursor/set (scanner value)
  "Set SCANNER's cursor to VALUE."
  (setf (cadr scanner) value))

(defun scanner/cursor/incr (scanner)
  "Increment SCANNER's cursor."
  (scanner/cursor/set scanner (+ 1 (scanner/cursor scanner))))

(defun scanner/current (scanner)
  "Peak the nth cursor of SCANNER's input."
  (if (scanner/eos scanner)
      (scanner/error scanner "EOF")
    (elt (scanner/input scanner)
         (scanner/cursor scanner))))

(defun scanner/error (_scanner msg)
  "Generate error in SCANNER and document with MSG."
  (signal 'templatel-syntax-error msg))

(defun scanner/eos (scanner)
  "Return t if cursor is at the end of SCANNER's input."
  (eq (scanner/cursor scanner)
      (length (scanner/input scanner))))

(defun scanner/next (scanner)
  "Push SCANNER's cursor one character."
  (if (scanner/eos scanner)
      (scanner/error scanner "EOF")
    (scanner/cursor/incr scanner)))

(defun scanner/any (scanner)
  "Match any character on SCANNER's input minus EOF."
  (let ((current (scanner/current scanner)))
    (scanner/next scanner)
    current))

(defun scanner/match (scanner c)
  "Match current character under SCANNER's to C."
  (if (eq c (scanner/current scanner))
      (progn (scanner/next scanner) c)
    (scanner/error scanner
     (format
      "Expected %s, got %s" c (scanner/current scanner)))))

(defun scanner/matchs (scanner s)
  "Match SCANNER's input to string S."
  (mapcar #'(lambda (i) (scanner/match scanner i)) s))

(defun scanner/range (scanner a b)
  "Succeed if SCANNER's current entry is between A and B."
  (let ((c (scanner/current scanner)))
    (if (and (>= c a) (<= c b))
        (scanner/any scanner)
      (format "Expected %s-%s, got %s" a b c))))

(defun scanner/or (scanner options)
  "SCANNER OPTIONS."
  (if (null options)
      (scanner/error scanner "No valid options")
    (let ((cursor (scanner/cursor scanner)))
      (condition-case nil
          (funcall (car options))
        (templatel-error
         (progn (scanner/cursor/set scanner cursor)
                (scanner/or scanner (cdr options))))))))

(defun scanner/not (scanner expr)
  "SCANNER EXPR."
  (let* ((cursor (scanner/cursor scanner))
         (succeeded (condition-case nil
                        (funcall expr)
                      (templatel-error
                       nil))))
    (scanner/cursor/set scanner cursor)
    (if succeeded
        (scanner/error scanner "Not meant to succeed")
      t)))

(defun scanner/zero-or-more (scanner expr)
  "SCANNER EXPR."
  (let ((item (condition-case nil
                  (funcall expr)
                (templatel-error nil))))
    (if (null item)
        item
      (cons item (scanner/zero-or-more scanner expr)))))

(defun scanner/one-or-more (scanner expr)
  "SCANNER EXPR."
  (cons (funcall expr)
        (scanner/zero-or-more scanner expr)))

(defun token/expr-op (scanner)
  "Read '{{' off SCANNER's input."
  (scanner/matchs scanner "{{"))

(defun token/expr-cl (scanner)
  "Read '}}' off SCANNER's input."
  (scanner/matchs scanner "}}"))

(defun token/stm-op (scanner)
  "Read '{%' off SCANNER's input."
  (scanner/matchs scanner "{%"))

(defun token/stm-cl (scanner)
  "Read '%}' off SCANNER's input."
  (scanner/matchs scanner "%}"))

(defun tk/dot (scanner)
  "Read '.' off SCANNER's input."
  (scanner/matchs scanner "."))

(defun parser/join-chars (chars)
  "CHARS."
  (string-join (mapcar 'byte-to-string chars) ""))

;; Text <- (!(_EXPR_OPEN / _STM_OPEN) .)+
(defun parser/text (scanner)
  "Parse Text entries from SCANNER's input."
  (cons
   "Text"
   (parser/join-chars
    (scanner/one-or-more
     scanner
     #'(lambda()
         (scanner/not
          scanner
          (lambda()
            (scanner/or
             scanner
             (list
              #'(lambda() (token/expr-op scanner))
              #'(lambda() (token/stm-op scanner))))))
         (scanner/any scanner))))))

(defun parser/template (scanner)
  "Parse Template entry from SCANNER's input."


  (parser/text scanner)

  ;; (scanner/matchs scanner "{%")

  ;; (scanner/one-or-more scanner #'(lambda() (scanner/matchs scanner "*")))
  )

(defun templatel-parse-string (input)
  "Parse INPUT."
  (let ((s (scanner/new input)))
    (parser/template s)))


(message "%s" (templatel-parse-string "Stuff Stuff Stuff"))


(defun templatel-render-string (template &rest context)
  "Render TEMPLATE string with CONTEXT variables."
  (templatel-parse-string template)
  (message "%s" context))


(templatel-render-string
 "Hello, {{ name }}.  It's now {{ time }}!"
 :name "Lincoln"
 :time "dinner time")

;;; templatel.el ends here
