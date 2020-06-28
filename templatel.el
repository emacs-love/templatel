;;; templatel --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;;
;; Copyright (C) 2020  Lincoln Clarete
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Inspired by Jinja, this teeny language compiles templates into
;; Emacs Lisp functions that can be called with different sets of
;; variables.  Among its main features, it supports if statements, for
;; loops, and a good amount of expressions that make it simpler to
;; manipulate data within the template.
;;
;;; Code:

(require 'subr-x)

(define-error 'templatel-syntax-error "Syntax Error" 'templatel-error)

(define-error 'templatel-runtime-error "Runtime Error" 'templatel-error)

(define-error 'templatel-backtracking "Backtracking" 'templatel-internal)

;; --- Scanner ---

(defun scanner/new (input)
  "Create scanner for INPUT."
  (list input 0 0 0))

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

(defun scanner/line (scanner)
  "Line the SCANNER's cursor is in."
  (caddr scanner))

(defun scanner/line/set (scanner value)
  "Set SCANNER's line to VALUE."
  (setf (caddr scanner) value))

(defun scanner/line/incr (scanner)
  "Increment SCANNER's line and reset col."
  (scanner/col/set scanner 0)
  (scanner/line/set scanner (+ 1 (scanner/line scanner))))

(defun scanner/col (scanner)
  "Column the SCANNER's cursor is in."
  (cadddr scanner))

(defun scanner/col/set (scanner value)
  "Set column of the SCANNER as VALUE."
  (setf (cadddr scanner) value))

(defun scanner/col/incr (scanner)
  "Increment SCANNER's col."
  (scanner/col/set scanner (+ 1 (scanner/col scanner))))

(defun scanner/state (scanner)
  "Return a copy o SCANNER's state."
  (copy-sequence (cdr scanner)))

(defun scanner/state/set (scanner state)
  "Set SCANNER's state with STATE."
  (scanner/cursor/set scanner (car state))
  (scanner/line/set scanner (cadr state))
  (scanner/col/set scanner (caddr state)))

(defun scanner/current (scanner)
  "Peak the nth cursor of SCANNER's input."
  (if (scanner/eos scanner)
      (scanner/error scanner "EOF")
    (elt (scanner/input scanner)
         (scanner/cursor scanner))))

(defun scanner/error (_scanner msg)
  "Generate error in SCANNER and document with MSG."
  (signal 'templatel-backtracking msg))

(defun scanner/eos (scanner)
  "Return t if cursor is at the end of SCANNER's input."
  (eq (scanner/cursor scanner)
      (length (scanner/input scanner))))

(defun scanner/next (scanner)
  "Push SCANNER's cursor one character."
  (if (scanner/eos scanner)
      (scanner/error scanner "EOF")
    (progn
      (scanner/col/incr scanner)
      (scanner/cursor/incr scanner))))

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
    (let ((state (scanner/state scanner)))
      (condition-case nil
          (funcall (car options))
        (templatel-internal
         (progn (scanner/state/set scanner state)
                (scanner/or scanner (cdr options))))))))

(defun scanner/optional (scanner expr)
  "Read EXPR from SCANNER returning nil if it fails."
  (let ((state (scanner/state scanner)))
    (condition-case nil
        (funcall expr)
      (templatel-internal
       (scanner/state/set scanner state)
       nil))))

(defun scanner/not (scanner expr)
  "Fail if EXPR succeed, succeed when EXPR fail using SCANNER."
  (let* ((cursor (scanner/cursor scanner))
         (succeeded (condition-case nil
                        (funcall expr)
                      (templatel-internal
                       nil))))
    (scanner/cursor/set scanner cursor)
    (if succeeded
        (scanner/error scanner "Not meant to succeed")
      t)))

(defun scanner/zero-or-more (scanner expr)
  "Read EXPR zero or more time from SCANNER."
  (let ((state (scanner/state scanner)))
    (condition-case nil
        (cons (funcall expr) (scanner/zero-or-more scanner expr))
      (templatel-internal
       (scanner/state/set scanner state)
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
  (let ((m (scanner/matchs scanner "in")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/and (scanner)
  "Read 'and' off SCANNER's input."
  (let ((m (scanner/matchs scanner "and")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/not (scanner)
  "Read 'not' off SCANNER's input."
  (let ((m (scanner/matchs scanner "not")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/or (scanner)
  "Read 'or' off SCANNER's input."
  (let ((m (scanner/matchs scanner "or")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/paren-op (scanner)
  "Read '(' off SCANNER's input."
  (scanner/matchs scanner "(")
  (parser/_ scanner))

(defun token/paren-cl (scanner)
  "Read ')' off SCANNER's input."
  (scanner/matchs scanner ")")
  (parser/_ scanner))

(defun token/| (scanner)
  "Read '|' off SCANNER's input."
  (let ((m (scanner/matchs scanner "|")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/|| (scanner)
  "Read '||' off SCANNER's input."
  (let ((m (scanner/matchs scanner "||")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/+ (scanner)
  "Read '+' off SCANNER's input."
  (let ((m (scanner/matchs scanner "+")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/- (scanner)
  "Read '-' off SCANNER's input."
  (let ((m (scanner/matchs scanner "-")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/* (scanner)
  "Read '*' off SCANNER's input."
  (let ((m (scanner/matchs scanner "*")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/** (scanner)
  "Read '**' off SCANNER's input."
  (let ((m (scanner/matchs scanner "**")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token// (scanner)
  "Read '/' off SCANNER's input."
  (let ((m (scanner/matchs scanner "/")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/// (scanner)
  "Read '//' off SCANNER's input."
  (let ((m (scanner/matchs scanner "//")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/== (scanner)
  "Read '==' off SCANNER's input."
  (let ((m (scanner/matchs scanner "==")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/!= (scanner)
  "Read '!=' off SCANNER's input."
  (let ((m (scanner/matchs scanner "!=")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/> (scanner)
  "Read '>' off SCANNER's input."
  (let ((m (scanner/matchs scanner ">")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/< (scanner)
  "Read '<' off SCANNER's input."
  (let ((m (scanner/matchs scanner "<")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/>= (scanner)
  "Read '>=' off SCANNER's input."
  (let ((m (scanner/matchs scanner ">=")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/<= (scanner)
  "Read '<=' off SCANNER's input."
  (let ((m (scanner/matchs scanner "<=")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/<< (scanner)
  "Read '<<' off SCANNER's input."
  (let ((m (scanner/matchs scanner "<<")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/>> (scanner)
  "Read '>>' off SCANNER's input."
  (let ((m (scanner/matchs scanner ">>")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/& (scanner)
  "Read '&' off SCANNER's input."
  (let ((m (scanner/matchs scanner "&")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/~ (scanner)
  "Read '~' off SCANNER's input."
  (let ((m (scanner/matchs scanner "~")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/% (scanner)
  "Read '%' off SCANNER's input."
  ;; This is needed or allowing a cutting point to be introduced right
  ;; after the operator of a binary expression.
  (scanner/not scanner #'(lambda() (token/stm-cl scanner)))
  (let ((m (scanner/matchs scanner "%")))
    (parser/_ scanner)
    (parser/join-chars m)))

(defun token/^ (scanner)
  "Read '^' off SCANNER's input."
  (let ((m (scanner/matchs scanner "^")))
    (parser/_ scanner)
    (parser/join-chars m)))

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
  (let ((iter (parser/identifier scanner))
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
    (parser/cut
     scanner
     #'(lambda() (token/expr-cl scanner))
     "Unclosed bracket")
    (cons "Expression" (list expr))))

;; Expr          <- Filter
(defun parser/expr (scanner)
  "Read an expression from SCANNER."
  (cons
   "Expr"
   (list (parser/filter scanner))))

(defun parser/cut (scanner fn msg)
  "Try to parse FN off SCANNER or error with MSG.

There are two types of errors emitted by this parser:
 1. Backtracking (internal), which is caught by most scanner
    functions, like scanner/or and scanner/zero-or-more.
 2. Syntax Error (public), which signals an unrecoverable parsing
    error.

This function catches backtracking errors and transform them in
syntax errors.  It must be carefully explicitly on places where
backtracking should be interrupted earlier."
  (condition-case nil
      (funcall fn)
    (templatel-internal
     (signal 'templatel-syntax-error
             (format "%s at %s:%s"
                     msg
                     (scanner/line scanner)
                     (scanner/col scanner))))))

(defun parser/item-or-named-collection (name first rest)
  "NAME FIRST REST."
  (if (null rest)
      first
    (cons name (cons first rest))))

(defun parser/binary (scanner name randfn ratorfn)
  "Parse binary operator NAME from SCANNER.

A binary operator needs two functions: one for reading the
operands (RANDFN) and another one to read the
operator (RATORFN)."
  (parser/item-or-named-collection
   (if (null name) "BinOp" name)
   (funcall randfn scanner)
   (scanner/zero-or-more
    scanner
    #'(lambda()
        (cons
           (funcall ratorfn scanner)
           (parser/cut
            scanner
            #'(lambda() (funcall randfn scanner))
            "Missing operand after binary operator"))))))

;; Filter        <- Logical (_PIPE Logical)*
(defun parser/filter (scanner)
  "Read Filter from SCANNER."
  (parser/binary scanner "Filter" #'parser/logical #'token/|))

;; Logical       <- BitLogical ((AND / OR) BitLogical)*
(defun parser/logical (scanner)
  "Read Logical from SCANNER."
  (parser/binary
   scanner
   nil ; "Logical"
   #'parser/bit-logical
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/and s))
         #'(lambda() (token/or s)))))))

;; BitLogical    <- Comparison ((BAND / BXOR / BOR) Comparison)*
(defun parser/bit-logical (scanner)
  "Read BitLogical from SCANNER."
  (parser/binary
   scanner
   nil ; "BitLogical"
   #'parser/comparison
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/& s))
         #'(lambda() (token/^ s))
         #'(lambda() (token/|| s)))))))

;; Comparison    <- BitShifting ((EQ / NEQ / LTE / GTE / LT / GT / IN) BitShifting)*
(defun parser/comparison (scanner)
  "Read a Comparison from SCANNER."
  (parser/binary
   scanner
   nil ; "Comparison"
   #'parser/bit-shifting
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/== s))
         #'(lambda() (token/!= s))
         #'(lambda() (token/<= s))
         #'(lambda() (token/>= s))
         #'(lambda() (token/< s))
         #'(lambda() (token/> s))
         #'(lambda() (token/in s)))))))

;; BitShifting   <- Term ((RSHIFT / LSHIFT) Term)*
(defun parser/bit-shifting (scanner)
  "Read a BitShifting from SCANNER."
  (parser/binary
   scanner
   nil ; "BitShifting"
   #'parser/term
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/>> s))
         #'(lambda() (token/<< s)))))))

;; Term          <- Factor ((PLUS / MINUS) Factor)*
(defun parser/term (scanner)
  "Read Term from SCANNER."
  (parser/binary
   scanner
   nil ; "Term"
   #'parser/factor
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/+ s))
         #'(lambda() (token/- s)))))))

;; Factor        <- Power ((STAR / DSLASH / SLASH) Power)*
(defun parser/factor (scanner)
  "Read Factor from SCANNER."
  (parser/binary
   scanner
   nil ; "Factor"
   #'parser/power
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/* s))
         #'(lambda() (token/// s))
         #'(lambda() (token// s)))))))

;; Power         <- Unary ((POWER / MOD) Unary)*
(defun parser/power (scanner)
  "Read Power from SCANNER."
  (parser/binary
   scanner
   nil ; "Power"
   #'parser/unary
   #'(lambda(s)
       (scanner/or
        s
        (list
         #'(lambda() (token/** s))
         #'(lambda() (token/% s)))))))

;; UnaryOp       <- PLUS / MINUS / NOT / BNOT
(defun parser/unary-op (scanner)
  "Read an Unary operator from SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda() (token/+ scanner))
    #'(lambda() (token/- scanner))
    #'(lambda() (token/~ scanner))
    #'(lambda() (token/not scanner)))))

;; Unary         <- UnaryOp Unary / UnaryOp Primary / Primary
(defun parser/unary (scanner)
  "Read Unary from SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda()
        (cons
         "Unary"
         (list
          (parser/unary-op scanner)
          (parser/unary scanner))))
    #'(lambda()
        (cons
         "Unary"
         (list
          (parser/unary-op scanner)
          (parser/cut
            scanner
            #'(lambda() (parser/primary scanner))
            "Missing operand after unary operator"))))
    #'(lambda() (parser/primary scanner)))))

;; Primary       <- _PAREN_OPEN Expr _PAREN_CLOSE
;;                / Element
(defun parser/primary (scanner)
  "Read Primary from SCANNER."
  (scanner/or
   scanner
   (list
    #'(lambda()
        (token/paren-op scanner)
        (let ((expr (parser/expr scanner)))
          (token/paren-cl scanner)
          expr))
    #'(lambda() (parser/element scanner)))))

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
   (cons
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
  (let ((eol (scanner/or
              scanner
              (list
               #'(lambda() (scanner/matchs scanner "\r\n"))
               #'(lambda() (scanner/matchs scanner "\n"))
               #'(lambda() (scanner/matchs scanner "\r"))))))
    (scanner/line/incr scanner)
    eol))

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
                 (signal
                  'templatel-runtime-error
                  (format "Variable `%s' not declared" name))))))
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
  (let ((fname (cdar (cdr item))))
    `(let ((entry (assoc ,fname filters)))
       (if (null entry)
           (signal
            'templatel-runtime-error
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
  (let ((fname (cdr (cadr (cadr item))))
        (params (cddr (cadr item))))
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
  (if (string= (caar (cddr item)) "Identifier")
      (compiler/filter-identifier (cdr item))
    (compiler/filter-fncall (cdr item))))

(defun compiler/filter-list (tree)
  "Compile filters from TREE.

TREE contains a list of filters that can be either Identifiers or
FnCalls.  This functions job is to iterate over the this list and
call `compiler/filter-item' on each entry."
  `(progn
     ,(compiler/run (car tree))
     ,@(mapcar #'compiler/filter-item (cdr tree))))

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
    `((progn ,(compiler/run expr) (pop valstk))
      ,@(compiler/run tmpl))))

(defun compiler/if-elif (tree)
  "Compile if/elif/else statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree))
        (elif (caddr tree))
        (else (cadr (cadddr tree))))
    `(cond ((progn ,(compiler/run expr) (pop valstk))
            ,@(compiler/run body))
           ,@(mapcar #'compiler/if-elif-cond elif)
           (t ,@(compiler/run else)))))

(defun compiler/if-else (tree)
  "Compile if/else statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree))
        (else (cadr (caddr tree))))
    `(if (progn ,(compiler/run expr) (pop valstk))
         ,@(compiler/run body)
       ,@(compiler/run else))))

(defun compiler/if (tree)
  "Compile if statement off TREE."
  (let ((expr (car tree))
        (body (cadr tree)))
    `(if (progn ,(compiler/run expr) (pop valstk))
         ,@(compiler/run body))))

(defun compiler/for (tree)
  "Compile for statement off TREE."
  (let ((id (cdar tree)))
    `(let ((subenv '((,id . nil)))
           (iterable ,(compiler/run (cadr tree))))
       (push subenv envstk)
       (mapc
        #'(lambda(id)
            (setf (alist-get ,id subenv) id)
            ,@(compiler/run (caddr tree)))
        iterable)
       (pop envstk))))

(defun compiler/binop-item (tree)
  "Compile item from list of binary operator/operand in TREE."
  (if (not (null tree))
      (let* ((tag (caar tree))
             (val (compiler/run (cdr (car tree))))
             (op (cadr (assoc tag '(;; Arithmetic
                                    ("*" *)
                                    ("/" /)
                                    ("+" +)
                                    ("-" -)
                                    ;; Logic
                                    ("and" and)
                                    ("or" or)
                                    ;; Bit Logic
                                    ("&" logand)
                                    ("||" logior)
                                    ("^" logxor)
                                    ;; Comparison
                                    ("<" <)
                                    (">" >)
                                    ("!=" (lambda(a b) (not (equal a b))))
                                    ("==" equal)
                                    (">=" >=)
                                    ("<=" <=)
                                    ("in" (lambda(a b) (not (null (member a b))))))))))
        (if (not (null val))
            `(progn
               ,val
               ,(compiler/binop-item (cdr tree))
               (let ((b (pop valstk))
                     (a (pop valstk)))
                 (push (,op a b) valstk)))))))

(defun compiler/binop (tree)
  "Compile a binary operator from the TREE."
  `(progn
     ,(compiler/run (car tree))
     ,(compiler/binop-item (cdr tree))))

(defun compiler/unary (tree)
  "Compile a unary operator from the TREE."
  (let* ((tag (car tree))
         (val (cadr tree))
         (op (cadr (assoc tag '(("+" (lambda(x) (if (< x 0) (- x) x)))
                                ("-" -)
                                ("~" lognot)
                                ("not" not))))))
    `(progn
       ,(compiler/run val)
       (push (,op (pop valstk)) valstk))))

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
    (`("BinOp"          . ,a) (compiler/binop a))
    (`("Unary"          . ,a) (compiler/unary a))
    (`("Number"         . ,a) a)
    (`("String"         . ,a) a)
    (`("Bool"           . ,a) a)
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

(defun templatel-render-file (path env)
  "Render file at PATH into a tree with variables from ENV."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((code (templatel-compile-string (buffer-string))))
      (templatel-render-code code env))))

(provide 'templatel)
;;; templatel.el ends here
