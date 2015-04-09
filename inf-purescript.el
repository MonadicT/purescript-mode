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

;;; Code:

(require 'comint)
(require 'purescript-mode)
(require 'purescript-font-lock)

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

(defcustom inferior-purescript-nodejs-executable "node"
  "Executable of NodeJS."
  :group 'inferior-purescript
  :type 'string)

(defcustom inferior-purescript-buffer-name "Psci"
  "Default buffer name for psci interpreter."
  :type 'string
  :group 'inferior-purescript
  :safe 'stringp)

(defvar inferior-purescript-buffer nil
  "The buffer in which the inferior process is running.")

(defvar inferior-purescript-module-history nil)
(defconst inferior-purescript-psci-user-config ".psci")
(defconst inferior-purescript-psci-modules-dir ".psci_modules")


(define-derived-mode inferior-purescript-mode comint-mode "Inferior PureScript"
  "Major mode for interacting with an inferior PureScript process."
  :group 'inferior-purescript
  (set (make-local-variable 'comint-prompt-regexp)
       inferior-purescript-prompt-regexp)
  ;; (define-key inferior-purescript-mode-map "\t" #'completion-at-point)
  ;;(compilation-shell-minor-mode 1)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'font-lock-defaults) '(purescript-font-lock-keywords))
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-use-syntax) t))


;;; Helpers:
(defun inferior-purescript-locate-base-directory (&optional directory)
  "Locate a directory which will be used as base directory."
  (let ((dir (or directory default-directory)))
    (or (locate-dominating-file dir inferior-purescript-psci-user-config)
        (locate-dominating-file dir inferior-purescript-psci-modules-dir))))


(defun inferior-purescript-get-buffer ()
  "Get inferior psci buffer."
  (if (derived-mode-p 'inferior-purescript-mode)
      (current-buffer)
    (let* ((global-proc-name (format "*%s*" inferior-purescript-buffer-name))
           (global-proc-running (comint-check-proc global-proc-name)))
      (when global-proc-running
        global-proc-name))))

(defun inferior-purescript-get-process-or-error ()
  (get-buffer-process (inferior-purescript-get-buffer)))

(defun inferior-purescript-send-string (string &optional process)
  "Send STRING to inferior purescript process"
  (interactive
   (list (read-string "PureScript command: ") nil))
  (let ((process (or process (inferior-purescript-get-process-or-error))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

;;; For debugging purposes. *Will be deleted*
(defun inferior-purescript-find-module-file (module)
  "Find the compiled file of a PureScript MODULE."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Module: " (inferior-purescript-find-modname))
                       (inferior-purescript-find-modname))))
  (let* ((base-dir (inferior-purescript-locate-base-directory))
         (psci-path (and base-dir (expand-file-name inferior-purescript-psci-modules-dir base-dir)))
         (node-path (and psci-path (expand-file-name "node_modules" psci-path)))
         (node-envvar (format "NODE_PATH=%s" node-path))
         (node-script (format "process.stdout.write(require.resolve('%s'))" module))
         (process-environment (if node-envvar (cons node-envvar process-environment) process-environment)))
    (find-file-other-window (car (process-lines inferior-purescript-nodejs-executable "-e" node-script)))))


;;; psci commands
(defun inferior-purescript-load-file (file-name &optional process)
  "Load FILE-NAME to psci PROCESS."
  (interactive (list
                (if current-prefix-arg
                    (read-file-name "File to load: " nil buffer-file-name)
                  buffer-file-name)))
  (let ((process (or process (inferior-purescript-get-process-or-error))))
    (when file-name
      (comint-send-string process (format ":m %s" file-name))
      (comint-send-string process "\n"))))

(defun inferior-purescript-find-modname (&optional buffer)
  "Find modname from BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^module[ \t]+\\(\\(?:\\sw\\|[.]\\)+\\)" nil t)
        (match-string 1)))))

(defun inferior-purescript-import-module (modname &optional process)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Module: " (inferior-purescript-find-modname))
                       (inferior-purescript-find-modname))))
  (let ((process (or process (inferior-purescript-get-process-or-error))))
    (comint-send-string process (format ":i %s" modname))
    (comint-send-string process "\n")))

(defun inferior-purescript-make-comint (command proc-name &optional directory)
  "Create a psci comint buffer."
  (let* ((cmdlist (split-string-and-unquote command))
         (interpreter (car cmdlist))
         (args (cdr cmdlist))
         (proc-buffer-name (format "*%s*" proc-name))
         (default-directory (or directory default-directory))
         (buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                        interpreter nil args)))
    (with-current-buffer buffer
      (inferior-purescript-mode))
    (display-buffer buffer)
    proc-buffer-name))

(defun inferior-purescript-send-region (start end)
  (interactive
   (list (region-beginning) (region-end)))
  (let* ((string (buffer-substring-no-properties start end))
         (process (inferior-purescript-get-process-or-error)))
    (inferior-purescript-send-string string process)))

;;;###autoload
(defalias 'switch-to-purescript #'run-purescript)

;;;###autoload
(defun run-purescript (&optional command directory)
  "Run PureScript interpreter with COMMAND."
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run psci: " purescript-program-name)
        (read-from-minibuffer "Start from directory: " (or (inferior-purescript-locate-base-directory) default-directory)))
     (list purescript-program-name (or (inferior-purescript-locate-base-directory) default-directory))))
  (get-buffer-process
   (inferior-purescript-make-comint
    (or command purescript-program-name)
    inferior-purescript-buffer-name
    directory)))


;;; minor mode
(defvar inf-purescript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" #'inferior-purescript-load-file)
    (define-key map "\C-c\C-j" #'inferior-purescript-import-module)
    (define-key map "\C-c\C-f" #'inferior-purescript-find-module-file)
    map)
  "Keymap for using inf-purescript.")

;;;###autoload
(define-minor-mode inf-purescript-mode
  "Minor mode for enabling inf-purescript process interaction."
  :lighter " Inf-PureScript"
  :keymap inf-purescript-mode-map)

(provide 'inf-purescript)

;;; inf-purescript.el ends here
