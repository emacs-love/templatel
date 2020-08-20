;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
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
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'templatel)

;; --- Error ---

(ert-deftest err-parse-incomplete ()
  (condition-case err
      (templatel-render-string "{% for i in a %}stuff" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,22: Missing endfor statement")))))
  (condition-case err
      (templatel-render-string "{% for i in a }{% endfor %}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,15: Statement not closed with \"%}\"")))))

  (condition-case err
      (templatel-render-string "{% if true %}stuff" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,19: Missing endif statement")))))
  (condition-case err
      (templatel-render-string "{% if true }{% endif %}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,12: Statement not closed with \"%}\"")))))

  (condition-case err
      (templatel-render-string "{% extends \"stuff.html\" }" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,25: Statement not closed with \"%}\"")))))

  (condition-case err
      (templatel-render-string "{% extends %}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,12: Missing template name in extends statement")))))

  (condition-case err
      (templatel-render-string "{% block %}stuff{% endblock %}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,10: Missing block name")))))

  (condition-case err
      (templatel-render-string "\n\n{% block blah %}\n\nstuff" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 5,6: Missing endblock statement")))))

  (condition-case err
      (templatel-render-string "{{ a + }}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,8: Missing operand after binary operator")))))

  (condition-case err
      (templatel-render-string "{{ - }}" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,6: Missing operand after unary operator")))))

  (condition-case err
      (templatel-render-string "{{ -a " '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,7: Unclosed bracket")))))

  (condition-case err
      (templatel-render-string "{{ -a }" '())
    (templatel-error
     (should (equal err '(templatel-syntax-error . "<string> at 1,8: Unclosed bracket"))))))

;; --- Renderer --

(ert-deftest render-block-extends-override-some-empty-blocks ()
  (let ((env (templatel-env-new
              :importfn #'(lambda(e name)
                            ;; Base template define the block
                            (templatel-env-add-template
                             e name
                             (templatel-new "{% block first %}{% endblock %} {% block second %}second-default{% endblock %}"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"layout.html\" %}{% block second %}brand new{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) " brand new"))))

(ert-deftest render-block-extends-override-some ()
  (let ((env (templatel-env-new
              :importfn #'(lambda(e name)
                             ;; Base template define the block
                             (templatel-env-add-template
                              e name
                              (templatel-new "{% block first %}first-default{% endblock %} {% block second %}second-default{% endblock %}"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"layout.html\" %}{% block second %}brand new{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) "first-default brand new"))))

(ert-deftest render-block-extends ()
  (let* ((env (templatel-env-new
               :importfn #'(lambda(e name)
                             ;; Base template define the block
                             (templatel-env-add-template
                              e name
                              (templatel-new "Always {% block stuff %}default{% endblock %}"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"layout.html\" %}{% block stuff %}look at the bright side{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) "Always look at the bright side"))))

(ert-deftest render-block-extends-super ()
  (let ((env (templatel-env-new
               :importfn #'(lambda(e name)
                             ;; Base template define the block
                             (templatel-env-add-template
                              e name
                              (templatel-new "{% block greeting %}Hello{% endblock %}"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"nav.html\" %}{% block greeting %}{{ super }} world{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) "Hello world"))))

(ert-deftest render-block-default ()
  (should (equal (templatel-render-string "{% block stuff %}default{% endblock %}" '()) "default")))

(ert-deftest render-expr-logic ()
  (should (equal (templatel-render-string "{{ true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false }}" '()) "nil"))
  (should (equal (templatel-render-string "{{ false or true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false and true }}" '()) "nil"))
  (should (equal (templatel-render-string "{{ true and true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ true or false }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false or true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ not not a }}" '(("a" . t))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . nil) ("b" . t))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . t) ("b" . nil))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . nil) ("b" . nil))) "nil"))
  (should (equal (templatel-render-string "{{ not a }}" '(("a" . nil))) "t"))
  (should (equal (templatel-render-string "{{ not not a }}" '(("a" . t))) "t"))
  (should (equal (templatel-render-string "{{ not a and not b }}"
                                          '(("a" . nil)
                                            ("b" . t)))
                 "nil"))
  (should (equal (templatel-render-string "{{ not a and not b }}"
                                          '(("a" . nil)
                                            ("b" . nil)))
                 "t")))

(ert-deftest render-expr-unary ()
  (should (equal (templatel-render-string "{{ +a }}" '(("a" . -10))) "10"))
  (should (equal (templatel-render-string "{{ +a }}" '(("a" . 10))) "10"))
  (should (equal (templatel-render-string "{{ -a }}" '(("a" . -10))) "10"))
  (should (equal (templatel-render-string "{{ -a }}" '(("a" . 10))) "-10")))

(ert-deftest render-expr-all-bitlogic ()
  (should (equal (templatel-render-string "{{ a & 0b11 }}" '(("a" . 10))) "2"))
  (should (equal (templatel-render-string "{{ a || 0b1 }}" '(("a" . 10))) "11"))
  (should (equal (templatel-render-string "{{ a ^ 0b11 }}" '(("a" . 10))) "9"))
  (should (equal (templatel-render-string "{{ ~a }}" '(("a" . 10))) "-11")))

(ert-deftest render-expr-all-cmp ()
  (should (equal (templatel-render-string "{{ a == 10 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a == 11 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a != 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a != 10 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a >= 10 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a >= 11 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a <= 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a <=  9 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a  < 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a  < 10 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a  >  9 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a  > 10 }}" '(("a" . 10))) "nil"))
  (should (equal (templatel-render-string "{{ a in b }}" '(("a" . 10)
                                                           ("b" . (0 5 10)))) "t"))
  (should (equal (templatel-render-string "{{ a in b }}" '(("a" . 10)
                                                           ("b" . (0 5 15)))) "nil")))

(ert-deftest render-if-elif-expr-cmp ()
  (should (equal (templatel-render-string
                  "{% if a > 10 %}Big{% elif a > 5 %}Med{% else %}Small{% endif %}"
                  '(("a" . 11)))
                 "Big"))
  (should (equal (templatel-render-string
                  "{% if a > 10 %}Big{% elif a > 5 %}Med{% else %}Small{% endif %}"
                  '(("a" . 7)))
                 "Med"))
  (should (equal (templatel-render-string
                  "{% if a > 10 %}Big{% elif a > 5 %}Med{% else %}Small{% endif %}"
                  '(("a" . 4)))
                 "Small")))

(ert-deftest render-if-else-expr-cmp ()
  (should (equal (templatel-render-string
                  "{% if a > 3 %}{{ (2 + 3) * a }}{% else %}ecase{% endif %}"
                  '(("a" . 2)))
                 "ecase")))

(ert-deftest render-if-expr-cmp ()
  (should (equal (templatel-render-string
                  "{% if a > 3 %}{{ (2 + 3) * a }}{% endif %}"
                  '(("a" . 2)))
                 ""))
  (should (equal (templatel-render-string
                  "{% if a > 3 %}{{ (2 + 3) * a }}{% endif %}"
                  '(("a" . 10)))
                 "50")))

(ert-deftest render-expr-math-paren ()
  (should (equal (templatel-render-string
                  "{{ (2 + 3) * 4 }}"
                  '())
                 "20")))

(ert-deftest render-expr-math-mix ()
  (should (equal (templatel-render-string
                  "{{ 2 + 3 * 4 }}"
                  '())
                 "14")))

(ert-deftest render-expr-math-div ()
  (should (equal (templatel-render-string
                  "{{ 150 / 3 }}"
                  '())
                 "50")))

(ert-deftest render-expr-math-mul ()
  (should (equal (templatel-render-string
                  "{{ 2 * 3 * 4 }}"
                  '())
                 "24")))

(ert-deftest render-expr-math-sub ()
  (should (equal (templatel-render-string
                  "{{ 150 - 3 }}"
                  '())
                 "147")))

(ert-deftest render-expr-math-sub ()
  (should (equal (templatel-render-string
                  "{{ 150 + 3 }}"
                  '())
                 "153")))

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

(ert-deftest render-expr-filter-int ()
  (should (equal (templatel-render-string
                  "You won {{ user.byte|int(16) }} in bars of gold"
                  '(("user" . (("byte" . "0xFF")))))
                 "You won 255 in bars of gold")))

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

(ert-deftest render-template-no-sp-stm ()
  (should (equal
           (templatel-render-string
            "{%for name in names%}{{name}} {%endfor%}"
            '(("names" . ("One" "Two" "Three"))))
           "One Two Three "))
  (should
   (equal
    (templatel-render-string
     "{%if false%}aaa{{ b }}aaa{%elif true%}b{%else%}c{%endif%}"
     '())
    "b")))

(ert-deftest render-template-forloop ()
  (should (equal
           (templatel-render-string
            "{% for name in names %}{{ name }} {% endfor %}"
            '(("names" . ("One" "Two" "Three"))))
           "One Two Three ")))

(ert-deftest render-if-else-elif-no-else ()
  (should
   (equal
    (templatel-render-string
     "before{% if post.image %}postimg{% elif blog.image %}blogimg{% endif %}after"
     '(("post" . ())
       ("blog" . ())))
    "beforeafter"))
  (should
   (equal
    (templatel-render-string
     "{% block x %}before{% if post.image %}postimg{% elif blog.image %}blogimg{% endif %}after{% endblock %}"
     '(("post" . ())
       ("blog" . ())))
    "beforeafter")))

(ert-deftest render-if-else-elif-two-stmt ()
  (should
   (equal
    (templatel-render-string
     "{% if false %}aaa{{ b }}aaa{% elif true %}b{% else %}c{% endif %}"
     '())
    "b"))
  (should
   (equal
    (templatel-render-string
     "{% if false %}aaa{{ b }}aaa{% elif false %}bbbb{{ b }}bbb{% else %}c{% endif %}"
     '())
    "c")))

(ert-deftest render-if-else-two-stmt ()
  (should
   (equal
    (templatel-render-string
     "{% if true %}stuff{% else %}before{{ a }}after{% endif %}"
     '())
    "stuff"))
  (should
   (equal
    (templatel-render-string
     "{% if false %}before{{ a }}after{% else %}stuff{% endif %}"
     '())
    "stuff")))

(ert-deftest render-if-two-stmt ()
  (should
   (equal
    (templatel-render-string
     "aqui ali {% if a.b %}before{{ a.b }}after{% endif %} e acola"
     '(("a" . (("a" . 1)
               ("c" . 3)))))
    "aqui ali  e acola")))

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
           (templatel-render-string "<h1>Hello {{name }}</h1>" '(("name" . "Emacs")))
           "<h1>Hello Emacs</h1>"))
  (should (equal
           (templatel-render-string "<h1>Hello {{ name}}</h1>" '(("name" . "Emacs")))
           "<h1>Hello Emacs</h1>"))
  (should (equal
           (templatel-render-string "<h1>Hello {{name}}</h1>" '(("name" . "Emacs")))
           "<h1>Hello Emacs</h1>"))
  (should (equal
           (templatel-render-string "<h1>Hello {{ name }}</h1>" '(("name" . "Emacs")))
           "<h1>Hello Emacs</h1>")))

(ert-deftest render-template ()
  (should (equal
           (templatel-render-string "<h1>Hello Emacs</h1>" nil)
           "<h1>Hello Emacs</h1>")))



;; --- Filters ---


(ert-deftest filter-upper ()
  (should (equal (templatel-filters-upper "stuff") "STUFF")))

(ert-deftest filter-lower ()
  (should (equal (templatel-filters-lower "STUFF") "stuff")))



;; --- Compiler ---

(ert-deftest compile-template ()
  (let* ((s (templatel--scanner-new "<h1>Hello Emacs</h1>" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             (templatel--compiler-run tree)
             '((insert "<h1>Hello Emacs</h1>"))))))

(ert-deftest compile-text ()
  (let* ((s (templatel--scanner-new "<h1>Hello Emacs</h1>" "<string>"))
         (tree (templatel--parser-text s)))
    (should (equal
             (templatel--compiler-run tree)
             '(insert "<h1>Hello Emacs</h1>")))))



;; --- Parser & Scanner ---

(ert-deftest template-extends ()
  (let* ((s (templatel--scanner-new "{% extends \"layout.html\" %}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("ExtendsStatement"
                ("String" . "layout.html")))))))

(ert-deftest template-block ()
  (let* ((s (templatel--scanner-new "{% block stuff %}default{% endblock %}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("BlockStatement"
                ("Identifier" . "stuff")
                ("Template"
                 ("Text" . "default"))))))))

(ert-deftest template-for ()
  (let* ((s (templatel--scanner-new "
{% for name in names %}
  {{ name }}
{% endfor %}
" "<string>"))
         (txt (templatel--parser-template s)))
    (should (equal
             txt
             '("Template"
               ("Text" . "\n")
               ("ForStatement"
                ("Identifier" . "name")
                ("Expr" ("Element" ("Identifier" . "names")))
                ("Template"
                 ("Text" . "\n  ")
                 ("Expression"
                  ("Expr"
                   ("Element"
                    ("Identifier" . "name"))))
                 ("Text" . "\n")))
               ("Text" . "\n"))))))

(ert-deftest template-elif-no-else ()
  (let* ((s (templatel--scanner-new "
{% if one %}
  One
{% elif two %}
  Two
{% elif three %}
  Three
{% endif %}
" "<string>"))
         (txt (templatel--parser-template s)))
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
                  ("Template" ("Text" . "\n  Three\n")))))
               ("Text" . "\n"))))))

(ert-deftest template-elif ()
  (let* ((s (templatel--scanner-new "
{% if one %}
  One
{% elif two %}
  Two
{% elif three %}
  Three
{% else %}
  Four
{% endif %}
" "<string>"))
         (txt (templatel--parser-template s)))
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
               ("Text" . "\n"))))))

(ert-deftest template-if-else ()
  (let* ((s (templatel--scanner-new "{% if show %}{{ show }}{% else %}Hide{% endif %}" "<string>"))
         (txt (templatel--parser-template s)))
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
  (let* ((s (templatel--scanner-new "{% if show %}{{ show }}{% endif %}" "<string>"))
         (txt (templatel--parser-template s)))
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

(ert-deftest template-expr-binop ()
  (let* ((s (templatel--scanner-new "Hello, {{ 1 * 2 }}!" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("Text" . "Hello, ")
               ("Expression"
                ("Expr"
                 ("BinOp"
                  ("Element" ("Number" . 1))
                  ("*" "Element" ("Number" . 2)))))
               ("Text" . "!"))))))

(ert-deftest template-variable ()
  (let* ((s (templatel--scanner-new "Hello, {{ name }}!" "<string>"))
         (tree (templatel--parser-template s)))
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
  (let ((s (templatel--scanner-new "\"fun with Emacs\"" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("String" . "fun with Emacs")))))

(ert-deftest expr-value-number ()
  (let ((s (templatel--scanner-new "325" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Number" . 325)))))

(ert-deftest expr-value-number-bin ()
  (let ((s (templatel--scanner-new "0b1010" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Number" . 10)))))

(ert-deftest expr-value-number-hex ()
  (let ((s (templatel--scanner-new "0xff" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Number" . 255)))))

(ert-deftest expr-value-bool-true ()
  (let ((s (templatel--scanner-new "true" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Bool" . t)))))

(ert-deftest expr-value-bool-false-with-comment ()
  (let ((s (templatel--scanner-new "false {# not important #}" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Bool" . nil)))))

;;; templatel-tests.el ends here
