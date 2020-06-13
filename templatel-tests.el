;;; templatel-tests --- Templating language for Emacs-Lisp; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'templatel)

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
