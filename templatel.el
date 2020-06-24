;;; templatel --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(define-error 'templatel-syntax-error "Syntax Error" 'templatel-error)

;; --- Scanner ---

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
      (scanner/error scanner (format "Expected %s-%s, got %s" a b c)))))

(defun scanner/or (scanner options)
  "Read the first one of OPTIONS that works SCANNER."
  (if (null options)
      (scanner/error scanner "No valid options")
    (let ((cursor (scanner/cursor scanner)))
      (condition-case nil
          (funcall (car options))
        (templatel-error
         (progn (scanner/cursor/set scanner cursor)
                (scanner/or scanner (cdr options))))))))

(defun scanner/optional (scanner expr)
  "Read EXPR from SCANNER returning nil if it fails."
  (let ((cursor (scanner/cursor scanner)))
    (condition-case nil
        (funcall expr)
      (templatel-error
       (scanner/cursor/set scanner cursor)
       nil))))

(defun scanner/not (scanner expr)
  "Fail if EXPR succeed, succeed when EXPR fail using SCANNER."
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
  "Read EXPR zero or more time from SCANNER."
  (let ((cursor (scanner/cursor scanner)))
    (condition-case nil
        (cons (funcall expr) (scanner/zero-or-more scanner expr))
      (templatel-error
       (scanner/cursor/set scanner cursor)
       nil))))

(defun scanner/one-or-more (scanner expr)
  "Read EXPR one or more time from SCANNER."
  (cons (funcall expr)
        (scanner/zero-or-more scanner expr)))

(defun token/expr-op (scanner)
  "Read '{{' off SCANNER's input."
  (scanner/matchs scanner "{{")
  (parser/_ scanner))

(defun token/stm-op (scanner)
  "Read '{%' off SCANNER's input."
  (scanner/matchs scanner "{%")
  (parser/_ scanner))

(defun token/comment-op (scanner)
  "Read '{#' off SCANNER's input."
  (scanner/matchs scanner "{#")
  (parser/_ scanner))

;; Notice these two tokens don't consume white spaces right after the
;; closing tag. That gets us a little closer to preserving entirely
;; the input provided to the parser.
(defun token/expr-cl (scanner)
  "Read '}}' off SCANNER's input."
  (scanner/matchs scanner "}}"))

(defun token/stm-cl (scanner)
  "Read '%}' off SCANNER's input."
  (scanner/matchs scanner "%}"))

(defun token/comment-cl (scanner)
  "Read '#}' off SCANNER's input."
  (scanner/matchs scanner "#}"))

(defun token/dot (scanner)
  "Read '.' off SCANNER's input."
  (scanner/matchs scanner ".")
  (parser/_ scanner))

(defun token/comma (scanner)
  "Read ',' off SCANNER's input."
  (scanner/matchs scanner ",")
  (parser/_ scanner))

(defun token/if (scanner)
  "Read 'if' off SCANNER's input."
  (scanner/matchs scanner "if")
  (parser/_ scanner))

(defun token/elif (scanner)
  "Read 'elif' off SCANNER's input."
  (scanner/matchs scanner "elif")
  (parser/_ scanner))

(defun token/else (scanner)
  "Read 'else' off SCANNER's input."
  (scanner/matchs scanner "else")
  (parser/_ scanner))

(defun token/endif (scanner)
  "Read 'endif' off SCANNER's input."
  (scanner/matchs scanner "endif")
  (parser/_ scanner))

(defun token/for (scanner)
  "Read 'for' off SCANNER's input."
  (scanner/matchs scanner "for")
  (parser/_ scanner))

(defun token/endfor (scanner)
  "Read 'endfor' off SCANNER's input."
  (scanner/matchs scanner "endfor")
  (parser/_ scanner))

(defun token/in (scanner)
  "Read 'in' off SCANNER's input."
  (scanner/matchs scanner "in")
  (parser/_ scanner))

(defun token/pipe (scanner)
  "Read '|' off SCANNER's input."
  (scanner/matchs scanner "|")
  (parser/_ scanner))

(defun token/paren-op (scanner)
  "Read '(' off SCANNER's input."
  (scanner/matchs scanner "(")
  (parser/_ scanner))

(defun token/paren-cl (scanner)
  "Read ')' off SCANNER's input."
  (scanner/matchs scanner ")")
  (parser/_ scanner))

(defun parser/join-chars (chars)
  "Join all the CHARS forming a string."
  (string-join (mapcar 'byte-to-string chars) ""))



;; --- Parser ---

;; Template      <- (Text / Statement / Expression)+
(defun parser/template (scanner)
  "Parse Template entry from SCANNER's input."
  (cons
   "Template"
   (scanner/one-or-more
    scanner
    #'(lambda() (scanner/or
            scanner
            (list #'(lambda() (parser/text scanner))
                  #'(lambda() (parser/statement scanner))
                  #'(lambda() (parser/expression scanner))))))))

;; Text <- (!(_EXPR_OPEN / _STM_OPEN / _COMMENT_OPEN) .)+
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
              #'(lambda() (token/stm-op scanner))
              #'(lambda() (token/comment-op scanner))))))
         (scanner/any scanner))))))

;; Statement     <- IfStatement / ForStatement
(defun parser/statement (scanner)
  "Parse a statement from SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda() (parser/if-stm scanner))
    #'(lambda() (parser/for-stm scanner)))))

;; IfStatement   <- _If Expr _STM_CLOSE Template Elif
;;                / _If Expr _STM_CLOSE Template Else
;;                / _If Expr _STM_CLOSE Template _EndIf
(defun parser/if-stm (scanner)
  "SCANNER."
  (scanner/or
   scanner
   (list #'(lambda() (parser/if-stm-elif scanner))
         #'(lambda() (parser/if-stm-else scanner))
         #'(lambda() (parser/if-stm-endif scanner)))))

;; _If Expr _STM_CLOSE Template Elif+ Else?
(defun parser/if-stm-elif (scanner)
  "Parse elif from SCANNER."
  (parser/if scanner)
  (let* ((expr (parser/expr scanner))
         (_ (token/stm-cl scanner))
         (tmpl (parser/template scanner))
         (elif (scanner/one-or-more scanner #'(lambda() (parser/elif scanner))))
         (else (scanner/optional scanner #'(lambda() (parser/else scanner)))))
    (cons "IfElif" (list expr tmpl elif else))))

;; _If Expr _STM_CLOSE Template Else
(defun parser/if-stm-else (scanner)
  "Parse else from SCANNER."
  (parser/if scanner)
  (let* ((expr (parser/expr scanner))
         (_ (token/stm-cl scanner))
         (tmpl (parser/template scanner))
         (else (parser/else scanner)))
    (cons "IfElse" (list expr tmpl else))))

;; _If Expr _STM_CLOSE Template _EndIf
(defun parser/if-stm-endif (scanner)
  "Parse endif from SCANNER."
  (parser/if scanner)
  (let* ((expr (parser/expr scanner))
         (_    (token/stm-cl scanner))
         (tmpl (parser/template scanner))
         (_    (parser/endif scanner)))
    (cons "IfStatement" (list expr tmpl))))

;; Elif          <- _STM_OPEN _elif Expr _STM_CLOSE Template
(defun parser/elif (scanner)
  "Parse elif expression off SCANNER."
  (token/stm-op scanner)
  (token/elif scanner)
  (let ((expr (parser/expr scanner))
        (_    (token/stm-cl scanner))
        (tmpl (parser/template scanner)))
    (cons "Elif" (list expr tmpl))))

;; _If           <- _STM_OPEN _if
(defun parser/if (scanner)
  "Parse if condition off SCANNER."
  (token/stm-op scanner)
  (token/if scanner))

;; Else          <- _STM_OPEN _else _STM_CLOSE Template _EndIf
(defun parser/else (scanner)
  "Parse else expression off SCANNER."
  (token/stm-op scanner)
  (token/else scanner)
  (token/stm-cl scanner)
  (let ((tmpl (parser/template scanner)))
    (parser/endif scanner)
    (cons "Else" (list tmpl))))

;; _EndIf        <- _STM_OPEN _endif _STM_CLOSE
(defun parser/endif (scanner)
  "Parse endif tag off SCANNER."
  (token/stm-op scanner)
  (token/endif scanner)
  (token/stm-cl scanner))

;; ForStatement  <- _For Expr _in Expr _STM_CLOSE Template _EndFor
;; _For          <- _STM_OPEN _for
(defun parser/for-stm (scanner)
  "Parse for statement from SCANNER."
  (token/stm-op scanner)
  (token/for scanner)
  (let ((iter (parser/expr scanner))
        (_ (token/in scanner))
        (iterable (parser/expr scanner))
        (_ (token/stm-cl scanner))
        (tmpl (parser/template scanner))
        (_ (parser/endfor scanner)))
    (cons "ForStatement" (list iter iterable tmpl))))

;; _EndFor       <- _STM_OPEN _endfor _STM_CLOSE
(defun parser/endfor (scanner)
  "Parse {% endfor %} statement from SCANNER."
  (token/stm-op scanner)
  (token/endfor scanner)
  (token/stm-cl scanner))

;; Expression    <- _EXPR_OPEN Expr _EXPR_CLOSE
(defun parser/expression (scanner)
  "SCANNER."
  (token/expr-op scanner)
  (let ((expr (parser/expr scanner)))
    (token/expr-cl scanner)
    (cons "Expression" (list expr))))

;; Expr          <- Element Filter
(defun parser/expr (scanner)
  "Read an expression from SCANNER."
  (let ((element (parser/element scanner))
        (filters (parser/filter scanner)))
    (cons
     "Expr"
     (if (null filters)
         (list element)
       (list element (cons "Filter" filters))))))

;; Filter        <- (_PIPE (FnCall / Identifier))*
(defun parser/filter (scanner)
  "Read a filter from SCANNER."
  (scanner/zero-or-more
   scanner
   #'(lambda()
      (token/pipe scanner)
      (scanner/or
       scanner
       (list
        #'(lambda() (parser/fncall scanner))
        #'(lambda() (parser/identifier scanner)))))))

;; Attribute     <- Identifier (_dot Identifier)+
(defun parser/attribute (scanner)
  "Read an Attribute from SCANNER."
  (cons
   "Attribute"
   (cons
    (parser/identifier scanner)
    (progn
      (token/dot scanner)
      (scanner/one-or-more
       scanner
       #'(lambda() (parser/identifier scanner)))))))

;; Element       <- Value / Attribute / FnCall / Identifier
(defun parser/element (scanner)
  "Read Element off SCANNER."
  (cons
   "Element"
   (list
    (scanner/or
     scanner
     (list
      #'(lambda() (parser/value scanner))
      #'(lambda() (parser/attribute scanner))
      #'(lambda() (parser/fncall scanner))
      #'(lambda() (parser/identifier scanner)))))))

;; FnCall        <- Identifier ParamList
(defun parser/fncall (scanner)
  "Read FnCall off SCANNER."
  (cons
   "FnCall"
   (list
    (parser/identifier scanner)
    (parser/paramlist scanner))))

;; -paramlist   <- _PAREN_OPEN Expr (_COMMA Expr)* _PAREN_CLOSE
(defun parser/-paramlist (scanner)
  "Read parameter list from SCANNER."
  (token/paren-op scanner)
  (let ((first (parser/expr scanner))
        (rest (scanner/zero-or-more
               scanner
               #'(lambda()
                   (token/comma scanner)
                   (parser/expr scanner)))))
    (token/paren-cl scanner)
    (cons first rest)))

;; ParamList     <- -paramlist
;;                / _PAREN_OPEN _PAREN_CLOSE
(defun parser/paramlist (scanner)
  "Read parameter list off SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda() (parser/-paramlist scanner))
    #'(lambda()
        (token/paren-op scanner)
        (token/paren-cl scanner)
        nil))))

;; Value         <- Number / BOOL / NIL / String
(defun parser/value (scanner)
  "Read Value from SCANNER."
  (let ((value (scanner/or
                scanner
                (list
                 #'(lambda() (parser/number scanner))
                 #'(lambda() (parser/bool scanner))
                 #'(lambda() (parser/nil scanner))
                 #'(lambda() (parser/string scanner))))))
    (parser/_ scanner)
    value))

;; Number        <- BIN / HEX / FLOAT / INT
(defun parser/number (scanner)
  "Read Number off SCANNER."
  (cons
   "Number"
   (scanner/or
    scanner
    (list
     #'(lambda() (parser/bin scanner))
     #'(lambda() (parser/hex scanner))
     #'(lambda() (parser/float scanner))
     #'(lambda() (parser/int scanner))))))

;; INT           <- [0-9]+                  _
(defun parser/int (scanner)
  "Read integer off SCANNER."
  (string-to-number
   (parser/join-chars
    (scanner/one-or-more
     scanner
     #'(lambda() (scanner/range scanner ?0 ?9))))
   10))

;; FLOAT         <- [0-9]* '.' [0-9]+       _
(defun parser/float (scanner)
  "Read float from SCANNER."
  (append
   (scanner/zero-or-more scanner #'(lambda() (scanner/range scanner ?0 ?9)))
   (scanner/matchs scanner ".")
   (scanner/one-or-more scanner #'(lambda() (scanner/range scanner ?0 ?9)))))

;; BIN           <- '0b' [0-1]+             _
(defun parser/bin (scanner)
  "Read binary number from SCANNER."
  (scanner/matchs scanner "0b")
  (string-to-number
   (parser/join-chars
    (append
     (scanner/one-or-more
      scanner
      #'(lambda() (scanner/range scanner ?0 ?1)))))
   2))

;; HEX           <- '0x' [0-9a-fA-F]+       _
(defun parser/hex (scanner)
  "Read hex number from SCANNER."
  (scanner/matchs scanner "0x")
  (string-to-number
   (parser/join-chars
    (append
     (scanner/one-or-more
      scanner
      #'(lambda()
          (scanner/or
           scanner
           (list #'(lambda() (scanner/range scanner ?0 ?9))
                 #'(lambda() (scanner/range scanner ?a ?f))
                 #'(lambda() (scanner/range scanner ?A ?F))))))))
   16))

;; BOOL          <- ('true' / 'false')         _
(defun parser/bool (scanner)
  "Read boolean value from SCANNER."
  (cons
   "Bool"
   (scanner/or
    scanner
    (list #'(lambda() (scanner/matchs scanner "true") t)
          #'(lambda() (scanner/matchs scanner "false") nil)))))

;; NIL           <- 'nil'                      _
(defun parser/nil (scanner)
  "Read nil constant from SCANNER."
  (scanner/matchs scanner "nil")
  (cons "Nil" nil))

;; String        <- _QUOTE (!_QUOTE .)* _QUOTE _
(defun parser/string (scanner)
  "Read a double quoted string from SCANNER."
  (scanner/match scanner ?\")
  (let ((str (scanner/zero-or-more
              scanner
              #'(lambda()
                  (scanner/not
                   scanner
                   #'(lambda() (scanner/match scanner ?\")))
                  (scanner/any scanner)))))
    (scanner/match scanner ?\")
    (cons "String" (parser/join-chars str))))

;; IdentStart    <- [a-zA-Z_]
(defun parser/identstart (scanner)
  "Read the first character of an identifier from SCANNER."
  (scanner/or
   scanner
   (list #'(lambda() (scanner/range scanner ?a ?z))
         #'(lambda() (scanner/range scanner ?A ?Z))
         #'(lambda() (scanner/match scanner ?_)))))

;; IdentCont    <- [a-zA-Z0-9_]*  _
(defun parser/identcont (scanner)
  "Read the rest of an identifier from SCANNER."
  (scanner/zero-or-more
   scanner
   #'(lambda()
       (scanner/or
        scanner
        (list #'(lambda() (scanner/range scanner ?a ?z))
              #'(lambda() (scanner/range scanner ?A ?Z))
              #'(lambda() (scanner/range scanner ?0 ?9))
              #'(lambda() (scanner/match scanner ?_)))))))

;; Identifier   <- IdentStart IdentCont
(defun parser/identifier (scanner)
  "Read Identifier entry from SCANNER."
  (cons
   "Identifier"
   (let ((identifier (parser/join-chars
                      (cons (parser/identstart scanner)
                            (parser/identcont scanner)))))
     (parser/_ scanner)
     identifier)))

;; _               <- (Space / Comment)*
(defun parser/_ (scanner)
  "Read whitespaces from SCANNER."
  (scanner/zero-or-more
   scanner
   #'(lambda()
       (scanner/or
        scanner
        (list
         #'(lambda() (parser/space scanner))
         #'(lambda() (parser/comment scanner)))))))

;; Space           <- ' ' / '\t' / _EOL
(defun parser/space (scanner)
  "Consume spaces off SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda() (scanner/matchs scanner " "))
    #'(lambda() (scanner/matchs scanner "\t"))
    #'(lambda() (parser/eol scanner)))))

;; _EOL            <- '\r\n' / '\n' / '\r'
(defun parser/eol (scanner)
  "Read end of line from SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda() (scanner/matchs scanner "\r\n"))
    #'(lambda() (scanner/matchs scanner "\n"))
    #'(lambda() (scanner/matchs scanner "\r")))))

;; Comment         <- "{#" (!"#}" .)* "#}"
(defun parser/comment (scanner)
  "Read comment from SCANNER."
  (scanner/matchs scanner "{#")
  (let ((str (scanner/zero-or-more
              scanner
              #'(lambda()
                  (scanner/not
                   scanner
                   #'(lambda() (scanner/matchs scanner "#}")))
                  (scanner/any scanner)))))
    (scanner/matchs scanner "#}")
    (cons "Comment" (parser/join-chars str))))



;; --- Compiler ---

(defun compiler/wrap (tree)
  "Compile root node into a function with TREE as body."
  `(lambda(env)
     (let* ((envstk (list env))
            (valstk (list))
            (filters '(("upper" . filters/upper)
                       ("lower" . filters/lower)
                       ("sum" . filters/sum)
                       ("plus1" . filters/plus1)
                       ("int" . filters/int)))
            (runtime/lookup-var
             (lambda(name)
               (catch '-brk
                 (dolist (ienv (reverse envstk))
                   (let ((value (assoc name ienv)))
                     (when (not (null value))
                       (throw '-brk (cdr value)))))
                 (error (format "Variable `%s' not declared" name))))))
       (with-temp-buffer
         ,@tree
         (buffer-string)))))

(defun compiler/element (tree)
  "Compile an element from TREE."
  `(let ((value ,@(compiler/run tree)))
     (push value valstk)
     value))

(defun compiler/expr (tree)
  "Compile an expr from TREE."
  `(progn
     ,@(mapcar #'compiler/run tree)))

(defun compiler/-attr (tree)
  "Walk through attributes on TREE."
  (if (null (cdr tree))
      (compiler/identifier (cdar tree))
    `(cdr (assoc ,(cdar tree) ,(compiler/-attr (cdr tree))))))

(defun compiler/attribute (tree)
  "Compile attribute access from TREE."
  (compiler/-attr (reverse tree)))

(defun compiler/filter-identifier (item)
  "Compile a filter without params from ITEM.

This filter takes a single parameter: the value being piped into
it.  The code generated must first ensure that such filter is
registered in the local `filters' variable, failing if it isn't.
If the filter exists, it must then call its associated handler."
  (let ((fname (cdr item)))
    `(let ((entry (assoc ,fname filters)))
       (if (null entry)
           (signal
            'templatel-syntax-error
            (format "Filter `%s' doesn't exist" ,fname))
         (push (funcall (cdr entry) (pop valstk)) valstk)))))

(defun compiler/filter-fncall (item)
  "Compiler filter with params from ITEM.

A filter can have multiple parameters.  In that case, the value
piped into the filter becomes the first parameter and the other
parameters are shifted to accommodate this change.  E.g.:

  {{ number | int(16) }}

Will be converted into the following:

  (int number 16)

Notice the paramter list is compiled before being passed to the
function call."
  (let ((fname (cdr (car (cdr item))))
        (params (car (cdr (cdr item)))))
    `(let ((entry (assoc ,fname filters)))
       (if (null entry)
           (signal
            'templatel-syntax-error
            (format "Filter `%s' doesn't exist" ,fname))
         (push (apply
                (cdr entry)
                (cons (pop valstk)
                      (list ,@(compiler/run params))))
               valstk)))))

(defun compiler/filter-item (item)
  "Handle compilation of single filter described by ITEM.

This function routes the item to be compiled to the appropriate
function.  A filter could be either just an identifier or a
function call."
  (if (string= (car item) "Identifier")
      (compiler/filter-identifier item)
    (compiler/filter-fncall item)))

(defun compiler/filter-list (tree)
  "Compile filters from TREE.

TREE contains a list of filters that can be either Identifiers or
FnCalls.  This functions job is to iterate over the this list and
call `compiler/filter-item' on each entry."
  `(progn
     ,@(mapcar #'compiler/filter-item tree)))

(defun compiler/expression (tree)
  "Compile an expression from TREE."
  `(progn
     ,@(compiler/run tree)
     (insert (format "%s" (pop valstk)))))

(defun compiler/text (tree)
  "Compile text from TREE."
  `(insert ,tree))

(defun compiler/identifier (tree)
  "Compile identifier from TREE."
  `(funcall runtime/lookup-var ,tree))

(defun compiler/if-elif-cond (tree)
  "Compile cond from elif statements in TREE."
  (let ((expr (cadr tree))
        (tmpl (caddr tree)))
    `((,@(compiler/run expr)) ,@(compiler/run tmpl))))

(defun compiler/if-elif (tree)
  "Compile if/elif/else statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree))
        (elif (caddr tree))
        (else (cadr (cadddr tree))))
    `(cond (,(compiler/run expr) ,@(compiler/run body))
           ,@(mapcar #'compiler/if-elif-cond elif)
           (t ,@(compiler/run else)))))

(defun compiler/if-else (tree)
  "Compile if/else statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree))
        (else (cadr (caddr tree))))
    `(if ,(compiler/run expr)
         ,@(compiler/run body)
       ,@(compiler/run else))))

(defun compiler/if (tree)
  "Compile if statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree)))
    `(if ,(compiler/run expr)
         ,@(compiler/run body))))

(defun compiler/for (tree)
  "Compile for statement off TREE."
  (let ((id (cdadr (cadar tree))))
    `(let ((subenv '((,id . nil)))
           (iterable ,(compiler/run (cadr tree))))
       (push subenv envstk)
       (mapcar
        #'(lambda(id)
            (setf (alist-get ,id subenv) id)
            ,@(compiler/run (caddr tree)))
        iterable)
       (pop envstk))))

(defun compiler/run (tree)
  "Compile TREE into bytecode."
  (pcase tree
    (`() nil)
    (`("Template"       . ,a) (compiler/run a))
    (`("Text"           . ,a) (compiler/text a))
    (`("Identifier"     . ,a) (compiler/identifier a))
    (`("Attribute"      . ,a) (compiler/attribute a))
    (`("Filter"         . ,a) (compiler/filter-list a))
    (`("Expr"           . ,a) (compiler/expr a))
    (`("Expression"     . ,a) (compiler/expression a))
    (`("Element"        . ,a) (compiler/element a))
    (`("IfElse"         . ,a) (compiler/if-else a))
    (`("IfElif"         . ,a) (compiler/if-elif a))
    (`("IfStatement"    . ,a) (compiler/if a))
    (`("ForStatement"   . ,a) (compiler/for a))
    (`("Number"         . ,a) a)
    (`("String"         . ,a) a)
    ((pred listp)             (mapcar #'compiler/run tree))
    (_ (message "NOENTIENDO: `%s`" tree))))



(defun filters/upper (s)
  "Upper case all chars of S."
  (upcase s))

(defun filters/lower (s)
  "Lowewr case all chars of S."
  (downcase s))

(defun filters/sum (s)
  "Sum all entries in S."
  (apply '+ s))

(defun filters/plus1 (s)
  "Add one to S."
  (1+ s))

(defun filters/int (s base)
  "Convert S into integer of base BASE."
  (string-to-number
   (replace-regexp-in-string "^0[xXbB]" "" s) base))

;; --- Public API ---

(defun templatel-parse-string (input)
  "Parse INPUT into a tree."
  (let ((s (scanner/new input)))
    (parser/template s)))

(defun templatel-compile-string (input)
  "Compile INPUT to Lisp code."
  (let ((tree (templatel-parse-string input)))
    (compiler/wrap (compiler/run tree))))

(defun templatel-render-code (code env)
  "Render CODE to final output with variables from ENV."
  (funcall (eval code) env))

(defun templatel-render-string (input env)
  "Render INPUT to final output with variables from ENV."
  (let ((code (templatel-compile-string input)))
    (templatel-render-code code env)))

(provide 'templatel)
;;; templatel.el ends here
