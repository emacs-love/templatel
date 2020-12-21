;;; publish.el --- Generate HTML Documentation; -*- lexical-binding: t -*-
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
;; Genenrate static websites off of Org Mode sources.
;;
;;; Code:

;; Initialize packaging & install `use-package' to download external
;; dependencies.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Install `use-package' enabled dependencies.  We need `jinja2-mode'
;; because `web-mode' doesn't seem to play well with `htmlize' on
;; Org-Mode blocks.
(use-package jinja2-mode :config :ensure t)
;; Org-Mode depends on `htmlize' in order to produce HTML off Org-Mode
;; blocks with syntax highlighting.
(use-package htmlize :config :ensure t)
;; Use the latest version of templatel by importing it from the
;; directory above
(add-to-list 'load-path "../")
;; Use the latest version of blorg, by either using a cloned version
;; or cloning it from scratch if we're running on the CI. Maybe we'll
;; move this require to use `use-package' after blorg is available via
;; melpa as well.
(if (file-directory-p "~/src/github.com/emacs-love/weblorg")
    (add-to-list 'load-path "~/src/github.com/emacs-love/weblorg")
  (shell-command
   "cd /tmp &&         # go somewhere we can't break things
    rm -rf weblorg &&  # clean up previous runs and get it
    git clone https://github.com/emacs-love/weblorg")
  (add-to-list 'load-path "/tmp/blorg"))

;; --- Actual HTML generation setup ---

(require 'weblorg)

;; Tells `htmlize' library to output HTML with css classes instead of
;; directly formatting the output.
(setq org-html-htmlize-output-type 'css)

;; Defaults to localhost:8000
(if (string= (getenv "ENV") "prod")
    (setq weblorg-default-url "https://clarete.li/templatel"))

;; Set site wide configuration
(weblorg-site :theme "autodoc")

;; Generate Index Page
(weblorg-route
 :name "index"
 :input-pattern "src/index.org"
 :template "index.html"
 :output "index.html"
 :url "/")

;; Generate API Reference
(weblorg-route
 :name "api"
 :input-source (weblorg-input-source-autodoc-sections
                `(("Render template strings" . "^templatel-render")
                  ("Template environments" . "^templatel-env")
                  ("Filters" . "^templatel-filter")
                  ("Exceptions" . ,(concat "templatel-" (regexp-opt '("syntax-error"
                                                                      "runtime-error"
                                                                      "backtracking"))))))
 :template "autodoc.html"
 :output "api.html"
 :url "/api.html")

(weblorg-copy-static
 :output "static/{{ file }}"
 :url "/static/{{ file }}")

(setq debug-on-error t)

(weblorg-export)

;;; publish.el ends here
