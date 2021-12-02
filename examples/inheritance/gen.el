;;; gen.el --- minimal setup of templatel with inheritance
;;
;; This example depends on (emacs "26.1") because it uses
;; `file-attribute-type'.
;;
;;; Commentary:
;;
;; This file contains a commented example with the minimum setup to
;; use templatel with support to inheritance and a function for
;; finding templates in the file system.
;;
;; Run `emacs --script gen.el` to see the output of the file
;; "main.html" rendered in the standard output.
;;
;;; Code:

;; This allows using the latest version in development.  There's a
;; good chance that you won't want this line when you copy this file.
;; If you use anything to manage Emacs-Lisp dependencies, you can just
;; install templatel through what you use regularly.  Make sure you
;; check the install section in templatel's homepage if there's any
;; doubt.
(add-to-list 'load-path "../../")

(require 'templatel)

(defconst gen--current-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where this file is located.
It is used as the root of the template directory.")


(defconst gen--template-paths
  (list (expand-file-name "templates" gen--current-directory)
        "/var/www/templates")
  "List of directories that contain our templates.")


(defun gen--template-find (directories name)
  "Find template NAME within DIRECTORIES.

This function implements a search for templates within the
provided list of directories.  The search happens from left to
right and returns on the first successful match.  This behavior,
which is intentionally similar to the PATH variable in a shell,
allows the user to override just the templates they're interested
in but still take advantage of other default templates."
  (if (null directories)
      ;; didn't find it. Signal an error upwards: (must be caught with
      ;; `condition-case` or will raise an error to the user)
      (signal
       'file-missing
       (list "" "File not found" (format "Template `%s' not found" name)))

    ;; Let's see if we can find it in the next directory
    (let* ((path (expand-file-name name (car directories)))
           (attrs (file-attributes path)))
      (cond
       ;; doesn't exist; try next dir
       ((null attrs) (gen--template-find (cdr directories) name))
       ;; is a directory
       ((file-attribute-type attrs) nil)
       ;; we found it
       ((null (file-attribute-type attrs))
        path)))))


(defun gen--template-import (en name)
  "Import template NAME within environment EN."
  (message "Template %s imported" name)
  (templatel-env-add-template
   en name
   (templatel-new-from-file
    (gen--template-find gen--template-paths name))))


;; The main part


(let ((env (templatel-env-new :importfn 'gen--template-import))
      (main-template "main.html")
      (template-vars `(("shapes" . ("square" "circle" "triangle")))))

  ;; Apply environment settings
  (templatel-env-set-autoescape env t)

  ;; Add the main template to the environment
  (templatel-env-add-template
   env main-template
   (templatel-new-from-file
    (gen--template-find gen--template-paths main-template)))

  ;; Will print the rendered template
  (message "%s" (templatel-env-render env main-template template-vars)))

;;; gen.el ends here
