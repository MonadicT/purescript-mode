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
  :group 'psci)

(defcustom psci-executable "psci"
  "The name of the command to start the inferior psci."
  :type 'string
  :group 'psci)

(defcustom psci-prompt-regexp "^> "
  "Regular expression matching top level input prompt of psci shell."
  :type 'string
  :group 'psci)

(defcustom psci-find-project-root t
  "If non-nil, try and find the project root directory of this file."
  :type 'boolean
  :safe 'booleanp
  :group 'psci)

(defcustom psci-node-executable "node"
  "Executable of NodeJS."
  :type 'string
  :group 'psci)

(defcustom psci-buffer-name "*psci*"
  "Default buffer name for psci interpreter."
  :type 'string
  :safe 'stringp
  :group 'psci)

(defcustom psci-project-base-files '(".psci"
                                     ".psci_modules"
                                     "bower.json"
                                     "package.json"
                                     "bower_components"
                                     ".bowerrc" ; XXX: At the end because could be found in $HOME
                                     )
  "List of files which be considered to locate the project root."
  :type '(repeat string)
  :group 'psci)

(defvar psci-module-history nil)
(defvar psci-bower-extra-dirs-glob nil)

;;;###autoload
(defconst psci-config-file ".psci")
;;;###autoload
(defconst psci-modules-directory ".psci_modules")

(define-derived-mode psci-mode comint-mode "Psci"
  "Major mode for interacting with an inferior PureScript process."
  :group 'psci
  (set (make-local-variable 'comint-prompt-regexp)
       psci-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'font-lock-defaults) '(purescript-font-lock-keywords))
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'compilation-error-regexp-alist)
       purescript-mode-compilation-regex-alist))


(defun psci-locate-base-directory (&optional directory throw-error)
  "Locate a project root DIRECTORY for a purescript project.

If THROW-ERROR is non-nil throws an signal error if project root
couldn't be found."
  (let ((directory (or directory default-directory)))
    (cl-loop for file in psci-project-base-files
             for project-root-dir = (locate-dominating-file directory file)
             when project-root-dir
             return project-root-dir
             finally (and throw-error (error "Project root not found")))))

(defun psci-find-module-name ()
  "Find PureScript module name from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward"^module[ \t]+\\(\\(?:\\sw\\|[.]\\)+\\)" nil t)
         (match-string-no-properties 1))))

;;;###autoload
(defun psci-create-dotpsci (directory)
  "Create a .psci in DIRECTORY."
  (interactive (list (read-directory-name "Directory: " (psci-locate-base-directory))))
  (let* ((default-directory (file-name-as-directory directory))
         (dotpsci (expand-file-name psci-config-file))
         (bower-dirs-glob (cons (or (psci-read-bowerrc-directory) "bower_components") psci-bower-extra-dirs-glob))
         (globs (append (list "src/")
                        bower-dirs-glob
                        ;; (list "test/") ; FIXME: Globbing purescript packages mess with package test/
                        )))
    (with-temp-file dotpsci
      (dolist (src (psci-collect-purescript-sources globs))
        (insert (format ":load %s\n" src)))
      (dolist (ffi (psci-collect-purescript-ffi globs))
        (insert (format ":foreign %s\n" ffi))))))

(defun psci-collect-purescript-sources (globs)
  "Collect recursively PureScript sources from directories GLOBS."
  (cl-loop for glob in globs
           for srcs = (psci-extend-glob (concat (file-name-as-directory glob) "**/*.purs"))
           append srcs into src-files
           finally return src-files))

(defun psci-collect-purescript-ffi (globs)
  "Collect recursively PureScript ffi from directories GLOBS."
  (cl-loop for glob in globs
           for ffis = (psci-extend-glob (concat (file-name-as-directory glob) "**/*.js"))
           append ffis into ffi-files
           finally return ffi-files))

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

(defun psci-read-bowerrc-directory (&optional directory)
  "Read directories defined in DIRECTORY."
  (let ((bowerrc (expand-file-name ".bowerrc" directory)))
    (and (file-readable-p bowerrc) (assoc-default 'directory (json-read-file bowerrc)))))


;; XXX: For debugging purposes, don't rely much on this.
;;;###autoload
(defun psci-locate-compiled-file (module)
  "Find the compiled file of a PureScript MODULE."
  (interactive (list (read-string "Module: " (psci-find-module-name) 'psci-module-history)))
  (let* ((base-dir (psci-locate-base-directory nil t))
         (node-path (expand-file-name (format "%s/node_modules" psci-modules-directory) base-dir))
         (process-environment (append (list (format "NODE_PATH=%s" node-path))
                                      process-environment)))
    (find-file-other-window (car (process-lines psci-node-executable "-e" (format "process.stdout.write(require.resolve('%s'))" module))))))

;;;###autoload
(defalias 'run-psci #'psci)

;;;###autoload
(defun psci ()
  "Run psci interpreter with COMMAND inside DIRECTORY."
  (interactive)
  (let* ((default-directory (if psci-find-project-root
                                (psci-locate-base-directory nil t)
                              default-directory))
         (buffer (make-comint-in-buffer "psci" psci-buffer-name psci-executable)))
    (with-current-buffer buffer
      (psci-mode))
    (pop-to-buffer buffer t)))

(provide 'psci)

;;; psci.el ends here
