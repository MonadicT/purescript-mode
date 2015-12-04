;;; psc-ide.el --- psc-ide integration               -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/purescript-mode
;; Keywords: PureScript
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (let-alist "1.0.1") (queue "0.1.1"))

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
;;
;; psc-ide protocol see: https://github.com/kRITZCREEK/psc-ide/blob/84139f7/PROTOCOL.md

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

(require 'json)
(require 'queue)

(defgroup psc-ide nil
  "Settings for REPL interaction via `inferior-purescript-mode'"
  :prefix "psc-ide-"
  :group 'purescript)

(defcustom psc-ide-server-executable "psc-ide-server"
  "Path to psc-ide-server executable."
  :type 'string
  :group 'psc-ide)

(defcustom psc-ide-server-host "127.0.0.1"
  "Host of psc-ide-server."
  :type 'string
  :group 'psc-ide)

(defcustom psc-ide-server-port 4242
  "Port of psc-ide-server."
  :type 'integer
  :group 'psc-ide)

(defcustom psc-ide-server-debug nil
  "Whether to use --debug flag."
  :type 'boolean
  :group 'psc-ide)

(defcustom psc-ide-server-directory nil
  "Directory where start of psc-ide-server."
  :type 'string
  :safe #'file-directory-p
  :group 'psc-ide)


;;; psc ide server
(defvar psc-ide-server-buffer-name "*psc-ide-server*")

;;;###autoload
(defun psc-ide-server-start ()
  "Get or start psc server process."
  (interactive)
  (or (get-buffer-process psc-ide-server-buffer-name)
      (let* ((directory (expand-file-name (or psc-ide-server-directory default-directory)))
             (process (start-process "psc-ide-server"
                                     psc-ide-server-buffer-name
                                     psc-ide-server-executable
                                     "--debug"
                                     "--port" (number-to-string psc-ide-server-port)
                                     "--directory" directory)))
        (set-process-filter process #'psc-ide-server-filter)
        (set-process-query-on-exit-flag process nil)
        process)))

(defun psc-ide-server-filter (process message)
  "Process filter for psc-ide server PROCESS and MESSAGE."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "<< RECV [%S]\n" message))))


;;; psc-ide client
(defvar psc-ide-client-buffer-name "*psc-ide-client*")

(define-error 'psc-ide-client-error "Unknown psc ide client error")
(define-error 'psc-ide-client-end-response "End of response" 'psc-ide-client-error)

(defvar psc-ide-client-latest-response nil)
(defvar psc-ide-client-callback-queue (make-queue)
  "Queue which holds the psc-ide-client callbacks.")

(defun psc-ide-client-get-or-create ()
  "Return empd server connection."
  (or (get-buffer-process psc-ide-client-buffer-name)
      (make-network-process :name "psc-ide-client"
                            :buffer psc-ide-client-buffer-name
                            :host psc-ide-server-host
                            :service psc-ide-server-port
                            :coding 'utf-8
                            :noquery t
                            :filter #'psc-ide-client-filter-function
                            :keepalive t)))

(defun psc-ide-client-filter-function (process message)
  "Filter function for psc-ide client PROCESS with MESSAGE."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "<< RECV [%S]" message))
    (condition-case err
        (progn
          (push message psc-ide-client-latest-response)
          (when (string-match "\n\\'" message)
            (let ((response (apply 'concat (reverse psc-ide-client-latest-response))))
              (setq psc-ide-client-latest-response nil)
              (signal 'psc-ide-client-end-response (list response)))))
      (psc-ide-client-end-response
       (let ((callback (queue-dequeue psc-ide-client-callback-queue)))
         (and (functionp callback) (funcall callback (json-read-from-string (cadr err))))))
      (error
       (queue-dequeue psc-ide-client-callback-queue)
       (setq psc-ide-client-latest-response nil)
       (signal (car err) (cdr err))))))

(cl-defun psci-ide-client-send (object &key callback)
  "Send OBJECT to psc-client CONNECTION and call CALLBACK with the response."
  (psc-ide-server-start)
  (let ((connection (psc-ide-client-get-or-create)))
    (queue-enqueue psc-ide-client-callback-queue callback)
    (process-send-string connection (json-encode object))
    (process-send-eof connection)))

;;;###autoload
(defun psc-ide-command-pursuit (query type &optional callback)
  "Call psc-ide client pursuit command with QUERY and TYPE.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "pursuit"
                              :params (list
                                       :query query
                                       :type type))
                        :callback callback))

;;;###autoload
(cl-defun psc-ide-command-load (&key
                                (modules [])
                                (dependencies [])
                                callback)
  "Call psc-ide client load command with MODULES and DEPENDENCIES.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "load"
                              :params (list :modules modules
                                            :dependencies dependencies))
                        :callback callback))

;;;###autoload
(cl-defun psc-ide-command-type (&key
                                (search "")
                                (filters [])
                                callback)
  "Call psc-ide client type command with SEARCH and FILTERS.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "type"
                              :params (list :search search
                                            :filters filters))
                        :callback callback))

;;;###autoload
(cl-defun psc-ide-command-complete (&key
                                    (filters [])
                                    (matcher "flex")
                                    callback)
  "Call psc-ide client complete command with FILTERS and MATCHER.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "complete"
                              :params (list :filters filters
                                            :matcher matcher))
                        :callback callback))

;;;###autoload
(cl-defun psc-ide-command-list (&key callback)
  "Call psc-ide client list command.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "list"
                              :params (list :type "loadedModules"))
                        :callback callback))

;;;###autoload
(cl-defun psc-ide-command-cwd (&key callback)
  "Call psc-ide client cwd command.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "cwd") :callback callback))

;;;###autoload
(cl-defun psc-ide-command-quit (&key callback)
  "Call psc-ide client quit command.

Calls CALLBACK with the response."
  (interactive)
  (psci-ide-client-send (list :command "quit") :callback callback))

(provide 'psc-ide)
;;; psc-ide.el ends here
