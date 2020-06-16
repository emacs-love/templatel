;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'templatel)

;; --- Renderer --

(ert-deftest render-template-elif0 ()
  (should
   (equal
    (templatel-render-string
"<h1>Hello
{% if one %}
  {{ name1 }}
{% elif two %}
  {{ name2 }}
{% elif three %}
  Three
{% else %}
  Four
{% endif %}
</h1>
"
     '(("name1" . "Emacs")
       ("name2" . "Gnu")
       ("two" . t)))
    "<h1>Hello\n\n  Gnu\n\n</h1>\n")))

(ert-deftest render-template-if-else-n ()
  (should
   (equal
    (templatel-render-string
     "<h1>Hello {% if enabled %}{{ name }}{% else %}Non-MX{% endif %}</h1>"
     '(("name" . "Emacs")
       ("enabled" . nil)))
    "<h1>Hello Non-MX</h1>")))

(ert-deftest render-template-if-else-t ()
  (should
   (equal
    (templatel-render-string
     "<h1>Hello {% if enabled %}{{ name }}{% else %}Non-Emacs{% endif %}</h1>"
     '(("name" . "Emacs")
       ("enabled" . t)))
    "<h1>Hello Emacs</h1>")))

(ert-deftest render-template-if-true ()
  (should
   (equal
    (templatel-render-string
     "<h1>Hello {% if enabled %}{{ name }}{% endif %}</h1>"
     '(("name" . "Emacs")
       ("enabled" . t)))
    "<h1>Hello Emacs</h1>")))

(ert-deftest render-template-if-false ()
  (should
   (equal
    (templatel-render-string
     "<h1>Hello {% if enabled %}{{ name }}{% endif %}</h1>"
     '(("name" . "Emacs")
       ("enabled" . nil)))
    "<h1>Hello </h1>")))

(ert-deftest render-template-variable ()
  (should (equal
           (templatel-render-string "<h1>Hello {{ name }}</h1>" '(("name" . "Emacs")))
           "<h1>Hello Emacs</h1>")))

(ert-deftest render-template ()
  (should (equal
           (templatel-render-string "<h1>Hello Emacs</h1>" nil)
           "<h1>Hello Emacs</h1>")))



;; --- Compiler ---

(ert-deftest compile-template ()
  (let* ((s (scanner/new "<h1>Hello Emacs</h1>"))
         (tree (parser/template s)))
    (should (equal
             (compiler/run tree)
             '((insert "<h1>Hello Emacs</h1>"))))))

(ert-deftest compile-text ()
  (let* ((s (scanner/new "<h1>Hello Emacs</h1>"))
         (tree (parser/text s)))
    (should (equal
             (compiler/run tree)
             '(insert "<h1>Hello Emacs</h1>")))))



;; --- Parser & Scanner ---

(ert-deftest template-elif ()
  (let* ((s (scanner/new "
{% if one %}
  One
{% elif two %}
  Two
{% elif three %}
  Three
{% else %}
  Four
{% endif %}
"))
         (txt (parser/template s)))
    (should (equal
             txt
             '("Template"
               ("Text" . "\n")
               ("IfElif"
                ("Expr" ("Identifier" . "one"))
                ("Template" ("Text" . "\n  One\n"))
                (("Elif"
                  ("Expr" ("Identifier" . "two"))
                  ("Template" ("Text" . "\n  Two\n")))
                 ("Elif" ("Expr" ("Identifier" . "three"))
                  ("Template" ("Text" . "\n  Three\n"))))
                ("Else" ("Template" ("Text" . "\n  Four\n"))))
               ("Text" . "\n")
               )))))

(ert-deftest template-if-else ()
  (let* ((s (scanner/new "{% if show %}{{ show }}{% else %}Hide{% endif %}"))
         (txt (parser/template s)))
    (should (equal
             txt
             '("Template"
               ("IfElse"
                ("Expr"
                 ("Identifier" . "show"))
                ("Template"
                 ("Expression" ("Expr" ("Identifier" . "show"))))
                ("Else"
                 ("Template" ("Text" . "Hide")))))))))

(ert-deftest template-if ()
  (let* ((s (scanner/new "{% if show %}{{ show }}{% endif %}"))
         (txt (parser/template s)))
    (should (equal
             txt
             '("Template"
               ("IfStatement"
                ("Expr" ("Identifier" . "show"))
                ("Template" ("Expression" ("Expr" ("Identifier" . "show"))))))))))

(ert-deftest template-text ()
  (let ((s (scanner/new "Hello, {{ name }}!")))
    (should (equal
             (parser/template s)
             '("Template" . (("Text" . "Hello, ")
                             ("Expression" ("Expr" . (("Identifier" . "name"))))
                             ("Text" . "!")))))))

(ert-deftest expr-value-string ()
  (let ((s (scanner/new "\"fun with Emacs\"")))
    (should (equal
             (parser/value s)
             '("Value" . ("String" . "fun with Emacs"))))))

(ert-deftest expr-value-number ()
  (let ((s (scanner/new "325")))
    (should (equal
             (parser/value s)
             '("Value" . ("Number" . "325"))))))

(ert-deftest expr-value-bool-true ()
  (let ((s (scanner/new "true")))
    (should (equal
             (parser/value s)
             '("Value" . ("Bool" . t))))))

(ert-deftest expr-value-bool-false-with-comment ()
  (let ((s (scanner/new "false {# not important #}")))
    (should (equal
             (parser/value s)
             '("Value" . ("Bool" . nil))))))

;;; templatel-tests.el ends here
