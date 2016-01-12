;;; flycheck-purescript.el --- Flycheck: purescript support      -*- lexical-binding: t; -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience, tools, languages
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (flycheck "0.22") (dash "2.12.0") (let-alist "1.0.4") (seq "1.11"))

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

;; # Setup
;;
;;     (add-hook 'purescript-mode-hook #'flycheck-purescript-setup)

;;; Code:
(eval-when-compile
  (require 'pcase)
  (require 'let-alist))

(require 'seq)
(require 'psci)
(require 'json)
(require 'dash)
(require 'flycheck)

(flycheck-def-option-var flycheck-purescript-project-root nil psc
  "Project root for PureScript syntax checker."
  :type '(choice (const :tag "None" nil)
                 (directory :tag "Custom project root"))
  :risky t)

(defun flycheck-purescript-purs-flags (directory)
  "Calculate the PureScript psc command flags from DIRECTORY."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (bower-purs (psci-bower-directory-purescript-glob)))
    (list (expand-file-name "**/*.purs" bower-purs)
          (expand-file-name "src/**/*.purs")
          "--ffi" (expand-file-name "**/*.js" bower-purs)
          "--ffi" (expand-file-name "src/**/*.js"))))


(defun flycheck-purescript-parse-json (output)
  "Read json errors from psc OUTPUT."
  ;; HACK: Emacs<25 doesn't allow capture stderr/stdout
  (let* ((lines (split-string output "\n" t))
         (errors (seq-find (lambda (line) (string-prefix-p "{" line)) lines)))
    (and errors (json-read-from-string errors))))

(defun flycheck-purescript-parse-errors (output checker buffer)
  "Do something with OUTPUT and CHECKER inside BUFFER."
  (let (errors)
    (pcase-dolist (`(,level . ,data) (flycheck-purescript-parse-json output))
      (setq level (pcase level
                    (`errors 'error)
                    (`warnings 'warning)))
      (seq-do (lambda (e)
                (let-alist e
                  (push (flycheck-error-new-at
                         .position.startLine
                         .position.startColumn
                         level
                         .message
                         :id .errorCode
                         :checker checker
                         :buffer buffer
                         :filename .filename)
                        errors)))
              data))
    errors))

(flycheck-define-checker psc
  "A PureScript syntax checker using psc."
  :command ("psc"
            "--no-magic-do" "--no-tco" "--no-prefix" "--no-opts" ; disable optimizations
            "--verbose-errors"             ; verbose errors
            "--json-errors"                ; json errors Purescript>=0.8
            "--output" temporary-directory ; write output to null device
            (eval (and flycheck-purescript-project-root
                       (flycheck-purescript-purs-flags flycheck-purescript-project-root))))
  :error-parser flycheck-purescript-parse-errors
  :predicate (lambda () flycheck-purescript-project-root)
  :modes purescript-mode)

;;;###autoload
(defun flycheck-purescript-setup ()
  "Setup Flycheck for psc.

Add `psc' to `flycheck-checkers'."
  (interactive)
  (when (buffer-file-name)
    (-when-let (root-dir (purescript-project-root))
      (setq-local flycheck-purescript-project-root root-dir))))

;;;###autoload
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'psc))

(provide 'flycheck-purescript)

;;; flycheck-purescript.el ends here
