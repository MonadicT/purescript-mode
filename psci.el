;;; psci.el --- Interaction with an inferior PureScript process  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/purescript-mode
;; Keywords: PureScript
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

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
(eval-when-compile
  (require 'cl-lib)
  (defvar compilation-error-regexp-alist))

(require 'json)
(require 'comint)
(require 'em-glob)
(require 'purescript-mode)
(require 'purescript-font-lock)

(defgroup psci nil
  "Settings for REPL interaction via `inferior-purescript-mode'"
  :prefix "psci-"
  :group 'purescript)

(defcustom psci-executable "psci"
  "The name of the command to start the inferior psci."
  :type 'string
  :group 'psci)

(defcustom psci-prompt-regexp "^> "
  "Regular expression matching top level input prompt of psci shell."
  :type 'string
  :group 'psci)

(defcustom psci-buffer-name "*psci*"
  "Default buffer name for psci interpreter."
  :type 'string
  :safe #'stringp
  :group 'psci)

(defconst psci-dotpsci-file-name ".psci"
  "Psci script file name.")

(define-derived-mode psci-mode comint-mode "Psci"
  "Major mode for interacting with an inferior PureScript process."
  :group 'psci
  (set (make-local-variable 'comint-prompt-regexp)
       psci-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'compilation-error-regexp-alist)
       purescript-mode-compilation-regex-alist)
  (compilation-shell-minor-mode 1))


;;;###autoload
(defalias 'dotpsci #'psci-write-dotpsci)

;;;###autoload
(defun psci-write-dotpsci (&optional directory)
  "Write .psci file collecting sources and ffis and from DIRECTORY recursively."
  (interactive (psci-read-project-root))
  (let* ((default-directory (file-name-as-directory (expand-file-name (or directory default-directory))))
         (dotpsci (expand-file-name psci-dotpsci-file-name))
         (bower-purs (psci-bower-directory-purescript-glob))
         (sources (append (psci-collect-purescript-sources "src/")
                          (psci-collect-purescript-sources bower-purs)))
         (ffis (append (psci-collect-purescript-ffis "src/")
                       (psci-collect-purescript-ffis bower-purs))))
    (message "Writing %s" dotpsci)
    (with-temp-file dotpsci
      (dolist (source sources)
        (insert (format ":load %s\n" source)))
      (dolist (ffi ffis)
        (insert (format ":foreign %s\n" ffi))))))

(defun psci-collect-purescript-sources (directory)
  "Collect recursively the PureScript sources from a DIRECTORY."
  (psci-extend-glob (concat (file-name-as-directory directory) "**/*.purs")))

(defun psci-collect-purescript-ffis (directory)
  "Collect recursively PureScript ffi from DIRECTORY."
  (psci-extend-glob (concat (file-name-as-directory directory) "**/*.js")))

(defun psci-bower-directory-purescript-glob (&optional directory)
  "Return a glob for PureScript bower sources in DIRECTORY."
  (let ((bowerdir (psci-read-bowerrc-directory directory)))
    (concat (file-name-as-directory bowerdir) "purescript-*/src/")))

(defun psci-read-bowerrc-directory (&optional directory)
  "Read directories defined in DIRECTORY."
  (let ((bowerrc (expand-file-name ".bowerrc" directory)))
    (or (assoc-default 'directory (ignore-errors (json-read-file bowerrc))) "bower_components")))

(defun psci-read-project-root ()
  "Read PureScript project root."
  (let ((project-root (purescript-project-root)))
    (list (or (and (not current-prefix-arg) project-root)
              (read-directory-name "Project root: " project-root)))))

(defvar eshell-glob-matches)
(defun psci-extend-glob (glob)
  "Locate recursively files from GLOB.

Based on `eshell-extended-glob'"
  (let ((paths (eshell-split-path glob))
        eshell-glob-matches)
    (unwind-protect
        (if (and (cdr paths)
                 (file-name-absolute-p (car paths)))
            (eshell-glob-entries (file-name-as-directory (car paths))
                                 (cdr paths))
          (eshell-glob-entries (file-name-as-directory ".") paths)))
    (and eshell-glob-matches (sort eshell-glob-matches #'string<))))


;;;###autoload
(defun psci-repl-send-string (string &optional process)
  "Send STRING to node.js repl PROCESS."
  (interactive (list (read-string "PureScript command: ") nil))
  (let ((process (or process (psci-get-process-or-error))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

(defun psci-get-process-or-error ()
  "Return psci process or error."
  (or (get-buffer-process psci-buffer-name) (error "No psci process running")))

;;;###autoload
(defalias 'run-psci #'psci)

;;;###autoload
(defun psci (directory)
  "Run psci interpreter inside DIRECTORY."
  (interactive (psci-read-project-root))
  (let* ((default-directory (file-name-as-directory directory))
         (buffer (make-comint-in-buffer "psci" psci-buffer-name psci-executable)))
    (with-current-buffer buffer
      (psci-mode))
    (switch-to-buffer psci-buffer-name)))


;;; psci-script mode
(defvar psci-script-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?-  ". 123" table)
    (modify-syntax-entry ?\n ">"     table)
    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "!#$%&*+./:<=>?@^|~,;\\")
    table)
  "Syntax table for psci script mode.")

(defvar psci-script-font-lock-keywords
  `((,(rx line-start (* space) (group  (not (any "\t:"))) ":")
     (1 font-lock-keyword-face))
    (,(rx line-start (* space) (group ":" (+ (any alnum))) (? (+ space) (group (one-or-more not-newline))))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)))
  "Font lock keywords for `psci-script-mode'.")

(defvar psci-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'psci-write-dotpsci)
    map)
  "Keymap for `psci-script-mode'.")

;;;###autoload
(define-derived-mode psci-script-mode text-mode "Psci-Script"
  "Major mode for working with .psci files.

\\{psci-script-mode-map}"
  :syntax-table psci-script-mode-syntax-table
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'font-lock-defaults)
       '(psci-script-font-lock-keywords t t nil nil))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (and (boundp 'electric-indent-inhibit)
       (setq electric-indent-inhibit t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.psci\\'" . psci-script-mode))

(provide 'psci)

;;; psci.el ends here
