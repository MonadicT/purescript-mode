;;; flycheck-purescript.el --- Flycheck: purescript support      -*- lexical-binding: t; -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience, tools, languages
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (flycheck "0.22"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'psci)
(require 'flycheck)

(defun flycheck-purescript-purs-flags (directory)
  "Calculate the purescript psc command flags from DIRECTORY."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (bower-purs (psci-bower-directory-purescript-glob)))
    (list (expand-file-name "**/*.purs" bower-purs)
          (expand-file-name "src/**/*.purs")
          "--ffi" (expand-file-name "**/*.js" bower-purs)
          "--ffi" (expand-file-name "src/**/*.js"))))

(flycheck-define-checker psc
  "A Purescript syntax checker using psc."
  :command ("psc"
            "--no-magic-do" "--no-prefix" "--no-opts" "--verbose-errors" ; disable optimizations
            (eval (flycheck-purescript-purs-flags (purescript-project-root-or-error))) ; psci flags
            "--output" temporary-directory
            )
  :error-patterns
  ((error line-start
          (or (and (zero-or-more " ") "Error at " (file-name) " line " line ", column " column (zero-or-more " ") (or ":" "-") (zero-or-more not-newline))
              (and "\"" (file-name) "\" (line " line ", column " column "):"))
          (or (message (one-or-more not-newline))
              (and (or "\r" "\n" "\r\n")
                   (message (zero-or-more " ") (one-or-more not-newline)
                            (zero-or-more (or "\r" "\n")
                                          (zero-or-more space)
                                          (one-or-more not-newline)))))
          line-end)

   ;; XXX: PureScript 0.7.6 errors
   (error line-start "at " (file-name) " line " line ", column " column " - line " (+ num) ", column " (+ num)
          (or (message (one-or-more not-newline))
              (and (one-or-more "\n")
                   (message (zero-or-more (zero-or-more space) (one-or-more not-newline))
                            (one-or-more "\n")
                            (zero-or-more space) (one-or-more not-newline)
                            (one-or-more "\n")
                            (zero-or-more (zero-or-more space) (one-or-more not-newline))
                            (one-or-more "\n")
                            (zero-or-more (zero-or-more space) (one-or-more not-newline))
                            (zero-or-more (or "\r" "\n")
                                          (zero-or-more space)
                                          (one-or-more not-newline)))))
          line-end))
  :predicate (lambda () (purescript-project-root)) ; Only check when `purescript-project-root' is defined
  :modes purescript-mode)

;;;###autoload
(defun flycheck-purescript-setup ()
  "Setup Flycheck for psc.

Add `psc' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'psc))

(provide 'flycheck-purescript)

;;; flycheck-purescript.el ends here
