;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'templatel)

;; --- Renderer --

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
             '(lambda(env)
                (with-temp-buffer
                  (insert "<h1>Hello Emacs</h1>")
                  (buffer-string)))))))

(ert-deftest compile-text ()
  (let* ((s (scanner/new "<h1>Hello Emacs</h1>"))
         (tree (parser/text s)))
    (should (equal
             (compiler/run tree)
             '(insert "<h1>Hello Emacs</h1>")))))



;; --- Parser & Scanner ---

(ert-deftest template-text ()
  (let ((s (scanner/new "Hello, {{ name }}!")))
    (should (equal
             (parser/template s)
             '("Template" . (("Text" . "Hello, ")
                             ("Expr" . (("Identifier" . "name")))
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
