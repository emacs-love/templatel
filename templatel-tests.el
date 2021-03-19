;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;;
;; Copyright (C) 2020-2021  Lincoln Clarete
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


;; --- Operator Precedence ---

(ert-deftest operator-precedence ()
  ;; attribute access operator has the highest precedence, even over
  ;; unary operators
  (should (equal "-5" (templatel-render-string "{{ -obj.prop }}" '(("obj" . (("prop" . 5)))))))
  (should (equal "5" (templatel-render-string "{{ -obj.prop }}" '(("obj" . (("prop" . -5)))))))
  ;; unary operators have higher precedence than filter operators
  (should (equal "5" (templatel-render-string "{{ -5 | abs }}" '())))
  ;; pipes (for filters) have higher precedence then other binary ops
  (should (equal "10" (templatel-render-string "{{ 2 * 3 | plus1 | plus1 }}" '())))
  (should (equal "11" (templatel-render-string "{{ (2 * 3 | plus1 | plus1) | plus1 }}" '())))
  ;; Arithmetic
  (should (equal "10" (templatel-render-string "{{ 2 * 3 + 4 }}" '())))
  (should (equal "10" (templatel-render-string "{{ 80 / 2 ** 3 }}" '())))
  (should (equal "5" (templatel-render-string "{{ 13 % 9 + 1 }}" '())))
  (should (equal "5" (templatel-render-string "{{ 2 * 3 + 4 % 2 + 1 - 2 }}" '()))))


;; --- Auto escaping ---

(ert-deftest autoescaping ()
  ;; Ensure string replacemet function works; thanks to this:
  ;; https://github.com/pallets/markupsafe/blob/337a79f8caeff25422b28771e851c140d5d744ac/tests/test_escape.py#L6
  (dolist (test
           '(("" . "")
             ;; ascii
             ("abcd&><'\"efgh" . "abcd&amp;&gt;&lt;&#39;&#34;efgh")
             ("&><'\"efgh" . "&amp;&gt;&lt;&#39;&#34;efgh")
             ("abcd&><'\"" . "abcd&amp;&gt;&lt;&#39;&#34;")
             ;; 2 byte
             ("こんにちは&><'\"こんばんは" . "こんにちは&amp;&gt;&lt;&#39;&#34;こんばんは")
             ("&><'\"こんばんは" . "&amp;&gt;&lt;&#39;&#34;こんばんは")
             ("こんにちは&><'\"" . "こんにちは&amp;&gt;&lt;&#39;&#34;")
             ;; 4 byte
             ("<🥕 & 🌽 & 🌶>" . "&lt;🥕 &amp; 🌽 &amp; 🌶&gt;")))
    (let ((input (car test))
          (expected (cdr test)))
      (should (equal expected (templatel-escape-string input)))))

  ;; Comes disabled by default
  (should (equal
           (templatel-render-string
            "<p>{{ post }}</p>"
            '(("post" . "<script>alert(1)</script>")))
           "<p><script>alert(1)</script></p>"))

  ;; Can be enabled with the autoescape flag
  (should (equal
           (templatel-render-string
            "<p>{{ post }}</p>"
            '(("post" . "<script>alert(1)</script>"))
            :autoescape t)
           "<p>&lt;script&gt;alert(1)&lt;/script&gt;</p>"))

  ;; Test out the `safe` filter when auto escaping is enabled; should
  ;; disable escaping locally
  (should (equal
           (templatel-render-string
            "<p>{{ post|safe }}</p>"
            '(("post" . "<script>alert(1)</script>"))
            :autoescape t)
           "<p><script>alert(1)</script></p>"))

  ;; Test out the `safe` filter when auto escaping is disabled; should
  ;; be a no-op
  (should (equal
           (templatel-render-string
            "<p>{{ post|safe }}</p>"
            '(("post" . "<script>alert(1)</script>")))
           "<p><script>alert(1)</script></p>"))

  ;; Disable autoescaping
  (let ((env (templatel-env-new)))
    (templatel-env-set-autoescape env nil)
    (templatel-env-add-template env "<string>" (templatel-new "<p>{{ post }}</p>"))
    (should (equal
             (templatel-env-render env "<string>" '(("post" . "<script>alert(1)</script>")))
             "<p><script>alert(1)</script></p>"))))

(ert-deftest autoescaping-blocks ()
  ;; The base template DID NOT escape the variable
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          (templatel-env-add-template
                           e name
                           (templatel-new "{% block greeting %}Hello {{ name }}{% endblock %}"))))))
    (templatel-env-set-autoescape env t)
    ;; The override that calls super also didn't escape anything
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"nav.html\" %}{% block greeting %}{{ super() }} and world{% endblock %}"))
    ;; The output should be escaped
    (should (equal (templatel-env-render env "page.html" '(("name" . "<h1>Title</h1>")))
                   "Hello &lt;h1&gt;Title&lt;/h1&gt; and world")))

  ;; The base template ESCAPED the variable
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          ;; Base template define the block
                          (templatel-env-add-template
                           e name
                           (templatel-new "{% block greeting %}Hello {{ name|safe }}{% endblock %}"))))))
    (templatel-env-set-autoescape env t)
    ;; The override that calls super trusts what the base template did
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"nav.html\" %}{% block greeting %}{{ super() }} and world{% endblock %}"))
    ;; the output should be safe
    (should (equal (templatel-env-render env "page.html" '(("name" . "<h1>Title</h1>")))
                   "Hello <h1>Title</h1> and world")))

  ;; The base template DID NOT escape the variable
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          ;; Base template define the block
                          (templatel-env-add-template
                           e name
                           (templatel-new "{% block greeting %}Hello {{ name }}{% endblock %}"))))))
    (templatel-env-set-autoescape env t)
    ;; The override that calls super trusts what the base template did
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"nav.html\" %}{% block greeting %}{{ super()|safe }} and world{% endblock %}"))
    ;; the output won't have been escaped
    (should (equal (templatel-env-render env "page.html" '(("name" . "<h1>Title</h1>")))
                   "Hello &lt;h1&gt;Title&lt;/h1&gt; and world"))))


;; --- Unicode ---

(ert-deftest render-unicode ()
  ;; "render the same way 🥕 🌽 🌶"
  (should (equal (templatel-render-string "0" '()) "0"))
  (should (equal (templatel-render-string "0a" '()) "0a"))
  (should (equal (templatel-render-string "render the same way 🥕 🌽 🌶" '())
                 "render the same way 🥕 🌽 🌶")))


;; --- Renderer --

(ert-deftest render-filter-named-parameter-syntax ()
  (let* ((env (templatel-env-new))
         ;; Register the filter within the template environment
         (_ (templatel-env-add-filter
             env "accepts_named_params"
             (lambda(opts)
               (string-join (mapcar (lambda(pair) (format "%s=%s" (car pair) (cdr pair))) opts) ","))))
         ;; Add the template that contains the function call with just
         ;; named parameters
         (_ (templatel-env-add-template
             env "page.html"
             (templatel-new "{{ accepts_named_params(a=1, b=\"oi\") }}")))
         (out (templatel-env-render env "page.html" '())))
    (should (equal out "a=1,b=oi")))

  (let* ((env (templatel-env-new))
         ;; Register the filter within the template environment
         (_ (templatel-env-add-filter
             env "accepts_named_params"
             (lambda(v1 opts)
               ;;(message "V1: %s" v1)
               (string-join (cons (format "%d" v1)
                                  (mapcar (lambda(pair) (format "%s=%s" (car pair) (cdr pair))) opts)) ","))))
         ;; Add the template that contains the function call with just
         ;; named parameters
         (_ (templatel-env-add-template
             env "page.html"
             (templatel-new "{{ accepts_named_params(v1, a=v2, b=v3) }}")))
         (out (templatel-env-render env "page.html" '(("v1" . 10)
                                                      ("v2" . 42)
                                                      ("v3" . 55)))))
    (should (equal out "10,a=42,b=55"))))

(ert-deftest render-standalone-filter-syntax ()
  (condition-case err
      (templatel-render-string "{{ stuff() }}" '())
    (templatel-error
     (should (equal err '(templatel-runtime-error . "Filter `stuff' doesn't exist")))))
  (let* ((env (templatel-env-new))
         (_ (templatel-env-add-filter env "mysum" (lambda(a b) (+ a b))))
         (_ (templatel-env-add-template env "page.html" (templatel-new "{{ mysum(2, 3) }}")))
         (out (templatel-env-render env "page.html" '())))
    (should (equal out "5")))
  (let* ((env (templatel-env-new))
         (_ (templatel-env-add-filter env "spam" (lambda(spam) (format "%s%s%s%s" spam spam spam spam))))
         (_ (templatel-env-add-template env "page.html" (templatel-new "{{ spam(\"sPaM\") }}")))
         (out (templatel-env-render env "page.html" '())))
    (should (equal out "sPaMsPaMsPaMsPaM"))))

(ert-deftest render-user-filter ()
  (let ((env (templatel-env-new)))
    (templatel-env-add-filter env "greetings" (lambda(name p) (format "Hello %s%s%s%s" name p p p)))
    (templatel-env-add-template env "page.html" (templatel-new "{{ name|greetings(\"!\") }}"))
    (should (equal (templatel-env-render env "page.html" '(("name" . "GNU"))) "Hello GNU!!!"))))

(ert-deftest render-block-extends-sub-sub ()
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          (templatel-env-add-template
                           e name
                           (let ((templates (make-hash-table :test 'equal)))
                             (puthash "base.html"
                                      (templatel-new "BASE:
{% block block0 %}base0{% endblock %}
{% block block1 %}base1{% endblock %}
{% block block2 %}base2{% endblock %}
{% block block3 %}base3{% endblock %}
base-end")
                                      templates)
                             (puthash "post.html"
                                      (templatel-new "{% extends \"base.html\" %}
{% block block0 %}post0{% endblock %}won't be rendered
{% block block1 %}post1{% endblock %}")
                                      templates)
                             (gethash name templates)))))))
    (templatel-env-add-template
     env "log/post.html"
     (templatel-new
      "{% extends \"post.html\" %}
{% block block0 %}log0{% endblock %}
{% block block2 %}log2{% endblock %}"))
    (should (equal
             (templatel-env-render env "log/post.html" '())
             "BASE:
log0
post1
log2
base3
base-end"))))

(ert-deftest render-block-extends-sub-sub-super ()
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          (templatel-env-add-template
                           e name
                           (let ((templates (make-hash-table :test 'equal)))
                             (puthash "base.html"
                                      (templatel-new "BASE:
{% block block0 %}base0{% endblock %}
{% block block1 %}base1{% endblock %}
{% block block2 %}base2{% endblock %}
{% block block3 %}base3{% endblock %}
base-end")
                                      templates)
                             (puthash "p0.html"
                                      (templatel-new "{% extends \"base.html\" %}
{% block block0 %}p0;{{ super() }}{% endblock %}won't be rendered
{% block block1 %}p0;{{ super() }}{% endblock %}won't be rendered
{% block block2 %}p0{% endblock %}")
                                      templates)
                             (puthash "p1.html"
                                      (templatel-new "{% extends \"p0.html\" %}
{% block block0 %}p1;{{ super() }}{% endblock %}won't be rendered
{% block block1 %}p1;{{ super() }}{% endblock %}")
                                      templates)
                             (gethash name templates)))))))
    (templatel-env-add-template
     env "p2.html"
     (templatel-new
      "{% extends \"p1.html\" %}
{% block block0 %}p2;{{ super() }}{% endblock %}
{% block block2 %}p2;{{ super() }}{% endblock %}"))

    (let ((text (templatel-env-render env "p2.html" '())))
      (should (equal
               text
               "BASE:
p2;p1;p0;base0
p1;p0;base1
p2;p0
base3
base-end")))))

(ert-deftest render-block-extends-override-full-block-with-empty-one ()
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
                          ;; Base template define the block
                          (templatel-env-add-template
                           e name
                           (templatel-new "-{% block blk %}something{% endblock %}-"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"layout.html\" %}{% block blk %}{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) "--"))))

(ert-deftest render-block-extends-override-some-empty-blocks ()
  (let ((env (templatel-env-new
              :importfn (lambda(e name)
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
              :importfn (lambda(e name)
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
               :importfn (lambda(e name)
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
              :importfn (lambda(e name)
                          ;; Base template define the block
                          (templatel-env-add-template
                           e name
                           (templatel-new "{% block greeting %}Hello{% endblock %}"))))))
    ;; Template that extends the layout and override the block
    (templatel-env-add-template
     env "page.html"
     (templatel-new "{% extends \"nav.html\" %}{% block greeting %}{{ super() }} world{% endblock %}"))
    (should (equal (templatel-env-render env "page.html" '()) "Hello world"))))

(ert-deftest render-block-default ()
  (should (equal (templatel-render-string "{% block stuff %}default{% endblock %}" '()) "default")))

(ert-deftest render-nil-value ()
  (should (equal (templatel-render-string "{{ myvar }}" '(("myvar" . nil))) ""))
  (should (equal (templatel-render-string "{{ nil }}" '()) "")))

(ert-deftest render-expr-logic ()
  (should (equal (templatel-render-string "{{ true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false }}" '()) ""))
  (should (equal (templatel-render-string "{{ false or true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false and true }}" '()) ""))
  (should (equal (templatel-render-string "{{ true and true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ true or false }}" '()) "t"))
  (should (equal (templatel-render-string "{{ false or true }}" '()) "t"))
  (should (equal (templatel-render-string "{{ not not a }}" '(("a" . t))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . nil) ("b" . t))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . t) ("b" . nil))) "t"))
  (should (equal (templatel-render-string "{{ a or b }}" '(("a" . nil) ("b" . nil))) ""))
  (should (equal (templatel-render-string "{{ not a }}" '(("a" . nil))) "t"))
  (should (equal (templatel-render-string "{{ not not a }}" '(("a" . t))) "t"))
  (should (equal (templatel-render-string "{{ not a and not b }}"
                                          '(("a" . nil)
                                            ("b" . t)))
                 ""))
  (should (equal (templatel-render-string "{{ not a and not b }}"
                                          '(("a" . nil)
                                            ("b" . nil)))
                 "t")))

(ert-deftest render-expr-unary ()
  (should (equal (templatel-render-string "{{ +a }}" '(("a" . -10))) "10"))
  (should (equal (templatel-render-string "{{ +a }}" '(("a" . 10))) "10"))
  (should (equal (templatel-render-string "{{ -a }}" '(("a" . -10))) "10"))
  (should (equal (templatel-render-string "{{ -a }}" '(("a" . 10))) "-10")))

(ert-deftest render-expr-all-cmp ()
  (should (equal (templatel-render-string "{{ a == 10 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a == 11 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a != 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a != 10 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a >= 10 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a >= 11 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a <= 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a <=  9 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a  < 11 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a  < 10 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a  >  9 }}" '(("a" . 10))) "t"))
  (should (equal (templatel-render-string "{{ a  > 10 }}" '(("a" . 10))) ""))
  (should (equal (templatel-render-string "{{ a in b }}" '(("a" . 10)
                                                           ("b" . (0 5 10))))
                 "t"))
  (should (equal (templatel-render-string "{{ a in b }}" '(("a" . 10)
                                                           ("b" . (0 5 15))))
                 "")))

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


;; --- Filters ---

(ert-deftest render-expr-filter-filter-abs ()
  (should (equal "10" (templatel-render-string "{{ -10 | abs }}" '())))
  (should (equal "10" (templatel-render-string "{{ 10 | abs }}" '()))))

(ert-deftest render-expr-filter-attr ()
  (should (equal "Hi Jaci, you are a Emacs user!"
                 (templatel-render-string
                  "Hi {{ user | attr(\"name\") }}, you are a {{ user | attr(\"editor\") }} user!"
                  '(("user" . (("name" . "Jaci")
                               ("editor" . "Emacs"))))))))

(ert-deftest render-expr-filter-capitalize ()
  (should (equal "Emacs"
                 (templatel-render-string
                  "{{ \"emacs\" | capitalize }}"
                  '())))
  (should (equal "Emacs lisp"
                 (templatel-render-string
                  "{{ \"emacs lisp\" | capitalize }}"
                  '()))))

(ert-deftest render-expr-filter-default ()
  (should (equal "Hi valuable human!"
                 (templatel-render-string
                  "Hi {{ user | default(\"valuable human\") }}!"
                  '(("user" . nil))))))

(ert-deftest render-expr-filter-escape ()
  ;; Notice auto-escaping is off
  (should (equal "Hi &lt;Something&gt;!"
                 (templatel-render-string
                  "Hi {{ content | escape }}!"
                  '(("content" . "<Something>")))))
  ;; Ensure alias exists
  (should (equal "Hi &lt;Something&gt;!"
                 (templatel-render-string
                  "Hi {{ content | e }}!"
                  '(("content" . "<Something>"))))))

(ert-deftest render-expr-filter-first ()
  (should (equal "Hi Jaci!"
                 (templatel-render-string
                  "Hi {{ users | first }}!"
                  '(("users" . ("Jaci" "Moon" "Lua")))))))

(ert-deftest render-expr-filter-first-attr ()
  (should (equal "Hi Jaci!"
                 (templatel-render-string
                  "Hi {{ users | first | attr(\"name\") }}!"
                  '(("users" . ((("name" . "Jaci"))
                                (("name" . "Moon"))
                                (("name" . "Lua")))))))))

(ert-deftest render-expr-filter-float ()
  ;; here we see the pipe (|) takes higher precedence over the plus
  ;; sign (+), convert the string content to float and then sum them.
  (should (equal "1.5"
                 (templatel-render-string
                  "{{ 1 + content | float }}"
                  '(("content" . "0.5")))))
  (should (equal "3.0"
                 (templatel-render-string
                  "{{ 1 + content | float }}"
                  '(("content" . 2)))))
  ;; there will be type errors
  (condition-case err
      (templatel-render-string
       "{{ 1 + content | float }}"
       '(("content" . '(1 2))))
    (templatel-error
     (should (equal err
                    '(templatel-runtime-error . "Can't convert type cons to float"))))))

(ert-deftest render-expr-filter-int ()
  (should (equal "5"
                 (templatel-render-string
                  "{{ content | int }}"
                  '(("content" . "5.5")))))
  (should (equal "3"
                 (templatel-render-string
                  "{{ 1 + content | int }}"
                  '(("content" . 2)))))
  (should (equal "You won 255 in bars of gold"
                 (templatel-render-string
                  "You won {{ user.byte|int(16) }} in bars of gold"
                  '(("user" . (("byte" . "0xFF")))))))
  (should (equal "You won 42 in bars of gold"
                 (templatel-render-string
                  "You won {{ user.bin|int(2) }} in bars of gold"
                  '(("user" . (("bin" . "0b101010")))))))
  (condition-case err
      (templatel-render-string
       "{{ content | int }}"
       '(("content" . '(1 2))))
    (templatel-error
     (should (equal err
                    '(templatel-runtime-error . "Can't convert type cons to int"))))))

(ert-deftest render-expr-filter-join ()
  (should (equal "1234"
                 (templatel-render-string
                  "{{ numbers | join }}"
                  '(("numbers" . (1 2 3 4))))))
  (should (equal "1, 2, 3, 4"
                 (templatel-render-string
                  "{{ numbers | join(\", \") }}"
                  '(("numbers" . (1 2 3 4)))))))

(ert-deftest render-expr-filter-last ()
  (should (equal "Hi Lua!"
                 (templatel-render-string
                  "Hi {{ users | last }}!"
                  '(("users" . ("Jaci" "Moon" "Lua")))))))

(ert-deftest render-expr-filter-length ()
  (should (equal "3"
                 (templatel-render-string
                  "{{ users | length }}"
                  '(("users" . ("Jaci" "Moon" "Lua")))))))

(ert-deftest render-expr-filter-lower ()
  (should (equal "happy hacking"
                 (templatel-render-string
                  "{{ phrase | lower }}"
                  '(("phrase" . "HApPY HaCKiNg"))))))

(ert-deftest render-expr-filter-max ()
  (should (equal "10"
                 (templatel-render-string
                  "{{ numbers | max }}"
                  '(("numbers" . (5 8 10 9 7 4 3)))))))

(ert-deftest render-expr-filter-min ()
  (should (equal "3"
                 (templatel-render-string
                  "{{ numbers | min }}"
                  '(("numbers" . (5 8 10 9 7 4 3)))))))

(ert-deftest render-expr-filter-round ()
  (should (equal "1"
                 (templatel-render-string
                  "{{ 1.49999 | round }}"
                  '())))
  (should (equal "2"
                 (templatel-render-string
                  "{{ 1.5 | round }}"
                  '())))
  (should (equal "2"
                 (templatel-render-string
                  "{{ 1.50001 | round }}"
                  '()))))

(ert-deftest render-expr-filter-title ()
  (should (equal "What If The World Was A Cone"
                 (templatel-render-string
                  "{{ phrase | title }}"
                  '(("phrase" . "what if the world was a cone"))))))


(ert-deftest render-expr-filter-apply ()
  (should (equal (templatel-render-string
                  "Awww {{ qts|sum|plus1 }}."
                  '(("qts" . (1 2 3 4 5))))
                 "Awww 16.")))

(ert-deftest render-expr-filter-upper ()
  (should (equal (templatel-render-string
                  "Awww {{ user.name|upper }}."
                  '(("user" . (("name" . "Gnu")))))
                 "Awww GNU.")))


;; --- Tests ---

(ert-deftest render-expr-test-defined ()
  (let ((vars '(("user" . (("name" . "Gnu"))))))
    (should (equal ""  (templatel-render-string "{{ user is defined }}" '())))
    (should (equal "t" (templatel-render-string "{{ user is defined }}" vars)))
    (should (equal "t" (templatel-render-string "{{ user.name is defined }}" vars)))
    (should (equal ""  (templatel-render-string "{{ user.online is defined }}" vars)))))

(ert-deftest render-expr-test-divisible ()
  (should (equal "" (templatel-render-string "{{ 10 is divisible(3) }}" '())))
  (should (equal "t" (templatel-render-string "{{ 9 is divisible(3) }}" '()))))


;; --- Attribute syntax ---

(ert-deftest render-expr-attribute ()
  (should (equal "Hi Gnu, happy Hacking"
                 (templatel-render-string
                  "Hi {{ user.name }}, happy {{ user.greeting }}"
                  '(("user" . (("name" . "Gnu")
                               ("greeting" . "Hacking")))))))
  ;; yields undefined when accessing unknown field
  (should (equal "Hi undefined!"
                 (templatel-render-string
                  "Hi {{ user.name }}!"
                  '(("user" . (("city" . "Belo Horizonte")))))))
  ;; with filters
  (should (equal "GNU"
                 (templatel-render-string
                  "{{ user.name | upper }}"
                  '(("user" . (("name" . "Gnu")))))))
  ;; with tests
  (should (equal "t"
                 (templatel-render-string
                  "{{ user.age is divisible(3) }}"
                  '(("user" . (("name" . "Gnu")
                               ("age" . 33)))))))
  ;; is falsy when an attribute doesn't exist
  (should (equal ""
                 (templatel-render-string
                  "{% if user.city %}{{ user.city }}{% endif %}"
                  '(("user" . (("name" . "Gnu")
                               ("greeting" . "Hacking"))))))))


;; --- Expression ---

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

(ert-deftest render-template-for-loop ()
  (should (equal
           "One Two Three "
           (templatel-render-string
            "{% for name in names %}{{ name }} {% endfor %}"
            '(("names" . ("One" "Two" "Three")))))))

(ert-deftest render-template-for-loop-with-filter ()
  (should (equal
           "1 2 3 4 5 6 7 8 9 "
           (templatel-render-string
            "{% for i in a|sort %}{{ i }} {% endfor %}"
            '(("a" . (3 6 1 4 9 5 8 2 7)))))))

(ert-deftest render-if-with-filter ()
  (should
   (equal
    "test"
    (templatel-render-string "{% if list | first %}test{% endif %}" '(("list" . (1))))))
  (should
   (equal
    "test"
    (templatel-render-string "{% if 1.6 | round > 1.4 %}test{% endif %}" '()))))

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
           "<h1>Hello undefined</h1>"
           (templatel-render-string "<h1>Hello {{ name }}</h1>" '())))
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

(ert-deftest template-fncall-positional-and-named-params ()
  (let* ((s (templatel--scanner-new "{{ stuff(x, y, a=1, b=\"other\") }}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("Expression"
                ("Expr"
                 ("Element"
                  ("FnCall"
                   ("Identifier" . "stuff")
                   ("NamedParams"
                    (("Identifier" . "a")
                     ("Expr"
                      ("Element"
                       ("Number" . 1))))
                    (("Identifier" . "b")
                     ("Expr"
                      ("Element"
                       ("String" . "other")))))
                   ("Expr"
                     ("Element"
                      ("Identifier" . "x")))
                   ("Expr"
                     ("Element"
                      ("Identifier" . "y"))))))))))))

(ert-deftest template-fncall-named-params ()
  (let* ((s (templatel--scanner-new "{{ stuff(a=1, b=\"other\") }}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("Expression"
                ("Expr"
                 ("Element"
                  ("FnCall"
                   ("Identifier" . "stuff")
                   ("NamedParams"
                    (("Identifier" . "a")
                     ("Expr" ("Element" ("Number" . 1))))
                    (("Identifier" . "b")
                     ("Expr" ("Element" ("String" . "other"))))))))))))))

(ert-deftest template-fncall-positional-params ()
  (let* ((s (templatel--scanner-new "{{ stuff(1, \"other\") }}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("Expression"
                ("Expr"
                 ("Element"
                  ("FnCall"
                   ("Identifier" . "stuff")
                   ("Expr" ("Element" ("Number" . 1)))
                   ("Expr" ("Element" ("String" . "other"))))))))))))

(ert-deftest template-fncall-no-params ()
  (let* ((s (templatel--scanner-new "{{ stuff() }}" "<string>"))
         (tree (templatel--parser-template s)))
    (should (equal
             tree
             '("Template"
               ("Expression"
                ("Expr"
                 ("Element"
                  ("FnCall"
                   ("Identifier" . "stuff"))))))))))

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

(ert-deftest expr-value-float ()
  (let ((s (templatel--scanner-new "3.14" "<string>")))
    (should (equal
             (templatel--parser-value s)
             '("Number" . 3.14)))))

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
