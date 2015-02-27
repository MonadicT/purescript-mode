;;; purescript-mode.el --- Major mode for editing Purescript -*- lexical-binding: t -*-

;; Copyright © 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/purescript-mode
;; Keywords: convenience
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

;; Emacs major mode for PureScript.

;;; Code:

;; XXX:
(defvar purescript-literate nil)
(make-variable-buffer-local 'purescript-literate)
(put 'purescript-literate 'safe-local-variable 'symbolp)

(defcustom purescript-mode-hook nil
  "Hook run after entering `purescript-mode'."
  :type 'hook
  :group 'purescript)

(define-derived-mode purescript-mode prog-mode "PureScript"
  "Major mode for PureScript
     \\{purescript-mode-map}
Invokes `purescript-mode-hook'."
  (set (make-local-variable 'font-lock-defaults)
       '(purescript-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . purescript-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . purescript-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))

(provide 'purescript-mode)

;;; purescript-mode.el ends here
