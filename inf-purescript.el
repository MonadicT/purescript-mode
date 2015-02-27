;;; inf-purescript.el --- Interaction with an inferior PureScript process  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Mario Rodas <marsam@users.noreply.github.com>
;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: PureScript

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

;;

;;; Code:

(require 'comint)
(require 'purescript-mode)

(defgroup inferior-purescript nil
  "Settings for REPL interaction via `inferior-purescript-mode'"
  :prefix "inferior-purescript-"
  :prefix "purescript-"
  :group 'purescript)

(defcustom purescript-program-name "psci"
  "The name of the command to start the inferior PureScript process.
The command can include arguments."
  :group 'inferior-purescript
  :type '(choice string (repeat string)))

(defcustom inferior-purescript-prompt-regexp "^> "
  "Regular expression matching top level input prompt of psci shell."
  :group 'inferior-purescript
  :type 'string)

(defcustom inferior-purescript-find-project-root t
  "If non-nil, try and find the project root directory of this file."
  :group 'inferior-purescript
  :type 'boolean)

(defconst inferior-purescript-psci-modules-folder ".psci_modules")

(define-derived-mode inferior-purescript-mode comint-mode "Inf-PureScript"
  "Major mode for interacting with an inferior PureScript process."
  :group 'inferior-purescript
  (set (make-local-variable 'comint-prompt-regexp)
       inferior-purescript-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t))

(defun inferior-purescript-string-to-strings (string)
  "Split the STRING into a list of strings."
  (let ((i (string-match "[\"]" string)))
    (if (null i) (split-string string) ; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i)))
              (let ((rfs (read-from-string string i)))
                (cons (car rfs)
                      (inferior-purescript-string-to-strings
                       (substring string (cdr rfs)))))))))

(defun inferior-purescript-command (arg)
  (inferior-purescript-string-to-strings
   (if (null arg)
       purescript-program-name
     (read-string "Command to run purescript: " purescript-program-name))))

(defvar inferior-purescript-buffer nil
  "The buffer in which the inferior process is running.")

(defun inferior-purescript-start-process (command)
  "Start an inferior purescript process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `purescript-program-name'.
It runs the hook `inferior-purescript-hook' after starting the process and
setting up the inferior-purescript buffer."
  (interactive (list (inferior-purescript-command current-prefix-arg)))
  (setq inferior-purescript-buffer
        (apply 'make-comint "purescript" (car command) nil (cdr command)))
  (with-current-buffer inferior-purescript-buffer
    (inferior-purescript-mode)
    (run-hooks 'inferior-purescript-hook)))

(defun inferior-purescript-process (&optional arg)
  (or (if (buffer-live-p inferior-purescript-buffer)
          (get-buffer-process inferior-purescript-buffer))
      (progn
        (let ((current-prefix-arg arg))
          (call-interactively #'inferior-purescript-start-process))
        (inferior-purescript-process arg))))

;;;###autoload
(defalias 'run-purescript #'switch-to-purescript)

;;;###autoload
(defun switch-to-purescript (&optional arg)
  "Show the inferior-purescript buffer.  Start the process if needed."
  (interactive "P")
  (let ((proc (inferior-purescript-process arg)))
    (pop-to-buffer (process-buffer proc))))

(provide 'inf-purescript)

;;; inf-purescript.el ends here
