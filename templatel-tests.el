;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'templatel)

;; --- Renderer --

(ert-deftest render-expr-filter-pipe ()
  (should (equal (templatel-render-string
                  "Awww {{ qts|sum|plus1 }}."
                  '(("qts" . (1 2 3 4 5))))
                 "Awww 16.")))

(ert-deftest render-expr-filter-upper ()
  (should (equal (templatel-render-string
                  "Awww {{ user.name|upper }}."
                  '(("user" . (("name" . "Gnu")))))
                 "Awww GNU.")))

(ert-deftest render-expr-attr ()
  (should (equal (templatel-render-string
                  "Hi {{ user.name }}, happy {{ user.greeting }}"
                  '(("user" . (("name" . "Gnu")
                               ("greeting" . "Hacking")))))
                 "Hi Gnu, happy Hacking")))

(ert-deftest render-expr-string ()
  (should (equal (templatel-render-string "{{ \"something\" }}" '()) "something")))

(ert-deftest render-expr-number-bin ()
  (should (equal (templatel-render-string "{{ 0b1010 }}" '()) "10")))

(ert-deftest render-expr-number-hex ()
  (should (equal (templatel-render-string "{{ 0xFF }}" '()) "255")))

(ert-deftest render-expr-number ()
  (should (equal (templatel-render-string "{{ 1280 }}" '()) "1280")))

(ert-deftest render-template-forloop ()
  (should (equal
           (templatel-render-string
            "{% for name in names %}{{ name }} {% endfor %}"
            '(("names" . ("One" "Two" "Three"))))
           "One Two Three ")))

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
       ("one" . nil)
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



;; --- Filters ---


(ert-deftest filter-upper ()
  (should (equal (filters/upper "stuff") "STUFF")))

(ert-deftest filter-lower ()
  (should (equal (filters/lower "STUFF") "stuff")))



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

(ert-deftest template-for ()
  (let* ((s (scanner/new "
{% for name in names %}
  {{ name }}
{% endfor %}
"))
         (txt (parser/template s)))
    (should (equal
             txt
             '("Template"
               ("Text" . "\n")
               ("ForStatement"
                ("Expr" ("Element" ("Identifier" . "name")))
                ("Expr" ("Element" ("Identifier" . "names")))
                ("Template"
                 ("Text" . "\n  ")
                 ("Expression"
                  ("Expr"
                   ("Element"
                    ("Identifier" . "name"))))
                 ("Text" . "\n")))
               ("Text" . "\n"))))))

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
                ("Expr"
                 ("Element"
                  ("Identifier" . "one")))
                ("Template" ("Text" . "\n  One\n"))
                (("Elif"
                  ("Expr"
                   ("Element"
                    ("Identifier" . "two")))
                  ("Template" ("Text" . "\n  Two\n")))
                 ("Elif" ("Expr" ("Element" ("Identifier" . "three")))
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
                 ("Element"
                  ("Identifier" . "show")))
                ("Template"
                 ("Expression"
                  ("Expr"
                   ("Element"
                    ("Identifier" . "show")))))
                ("Else"
                 ("Template" ("Text" . "Hide")))))))))

(ert-deftest template-if ()
  (let* ((s (scanner/new "{% if show %}{{ show }}{% endif %}"))
         (txt (parser/template s)))
    (should (equal
             txt
             '("Template"
               ("IfStatement"
                ("Expr"
                 ("Element"
                  ("Identifier" . "show")))
                ("Template"
                 ("Expression"
                  ("Expr"
                   ("Element"
                    ("Identifier" . "show")))))))))))

(ert-deftest template-text ()
  (let* ((s (scanner/new "Hello, {{ name }}!"))
         (tree (parser/template s)))
    (should (equal
             tree
             '("Template"
               ("Text" . "Hello, ")
               ("Expression"
                ("Expr"
                 ("Element"
                  ("Identifier" . "name"))))
               ("Text" . "!"))))))

(ert-deftest expr-value-string ()
  (let ((s (scanner/new "\"fun with Emacs\"")))
    (should (equal
             (parser/value s)
             '("String" . "fun with Emacs")))))

(ert-deftest expr-value-number ()
  (let ((s (scanner/new "325")))
    (should (equal
             (parser/value s)
             '("Number" . 325)))))

(ert-deftest expr-value-number-bin ()
  (let ((s (scanner/new "0b1010")))
    (should (equal
             (parser/value s)
             '("Number" . 10)))))

(ert-deftest expr-value-number-hex ()
  (let ((s (scanner/new "0xff")))
    (should (equal
             (parser/value s)
             '("Number" . 255)))))

(ert-deftest expr-value-bool-true ()
  (let ((s (scanner/new "true")))
    (should (equal
             (parser/value s)
             '("Bool" . t)))))

(ert-deftest expr-value-bool-false-with-comment ()
  (let ((s (scanner/new "false {# not important #}")))
    (should (equal
             (parser/value s)
             '("Bool" . nil)))))

;;; templatel-tests.el ends here
