;;; org-reminders-cli-dao.el --- CLI Data Access for Org Reminders.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org-reminders-model)

(defvar org-reminders-cli-dao--list-names nil)
(defvar org-reminders-cli-dao--items nil)
(defvar org-reminders-cli-dao--items-hashtable nil)


(defun org-reminders-cli-dao--build-items-hashtable ()
  (setq org-reminders-cli-dao--items-hashtable
        (org-reminders--build-items-hashtable org-reminders-cli-dao--items)))

(defun org-reminders-cli-dao-get (external-id)
  (gethash external-id org-reminders-cli-dao--items-hashtable))

(defvar org-reminders-cli-dao--commands
  (list
   :add '("reminders add '{:list-name}' '{:title}'"
          " --due-date '{:due-date}'"
          " --priority '{:priority}'"
          " --notes '{:notes}'"
          " --url '{:url}'"
          " --format json")
   :complete '("reminders complete '{:list-name}' '{:external-id}' --format json")
   :uncomplete '("reminders uncomplete '{:list-name}' '{:external-id}' --format json")
   :delete '("reminders delete '{:list-name}' '{:external-id}'")
   :edit '("reminders edit '{:list-name}' '{:external-id}' '{:title}'"
           " --notes '{:notes}'"
           " --url '{:url}'"
           " --format json")
   :show '("reminders show '{:list-name}' --include-completed -f json")
   :show-all '("reminders show-all --include-completed -f json")
   :show-lists '("reminders show-lists -f json")
   :new-list '("reminders new-list '{:list-name}'")))


(defun org-reminders-cli-dao--run-command (command-key &rest args)
  "Run reminders cli command."
  (let* ((lst (plist-get org-reminders-cli-dao--commands command-key))
         (command-str (apply #'org-reminders--expand-list lst args)))
    (message command-str)
    (shell-command-to-string command-str)))

(defun org-reminders-cli-dao-all-items ()
  "Refresh list names data."
  (setq org-reminders-cli-dao--items
        (org-reminders-model-parse-json-str
         (org-reminders-cli-dao--run-command :show-all))))

(defun org-reminders-cli-dao-all-list-names ()
  "Refresh list names data."
  (setq org-reminders-cli-dao--list-names
        (org-reminders-model-parse-json-str
         (org-reminders-cli-dao--run-command :show-lists))))

(defun org-reminders-cli-dao-all-elements ()
  "Refresh list names data."
  (org-reminders-cli-dao-all-list-names)
  (org-reminders-cli-dao-all-items)
  (org-reminders-cli-dao--build-items-hashtable))


(defun org-reminders-cli-dao-add-list (list-name)
  "Refresh list names data."
  (org-reminders-cli-dao--run-command :new-list
                                      :list-name list-name))

(defun org-reminders-cli-dao-add-item (model)
  "Refresh list names data."
  (org-reminders-model-parse-json-str
   (apply #'org-reminders-cli-dao--run-command
          :add
          (org-reminders-model-to-plist model))))

(defun org-reminders-cli-dao-complete (model)
  "Refresh list names data."
  (org-reminders-model-parse-json-str
   (apply #'org-reminders-cli-dao--run-command
          :complete
          (org-reminders-model-to-plist model))))

(defun org-reminders-cli-dao-toggle-completed (model)
  (if (org-reminders-model-completed model)
      (org-reminders-cli-dao-uncomplete model)
    (org-reminders-cli-dao-complete model)))

(defun org-reminders-cli-dao-uncomplete (model)
  "Refresh list names data."
  (org-reminders-model-parse-json-str
   (apply #'org-reminders-cli-dao--run-command
          :uncomplete
          (org-reminders-model-to-plist model))))

(defun org-reminders-cli-dao-delete (model)
  "Refresh list names data."
  (apply #'org-reminders-cli-dao--run-command
         :delete
         (org-reminders-model-to-plist model)))

(defun org-reminders-cli-dao-edit (model)
  "Refresh list names data."
  (org-reminders-model-parse-json-str
   (apply #'org-reminders-cli-dao--run-command
          :edit
          (org-reminders-model-to-plist model))))

(provide 'org-reminders-cli-dao)
;;; org-reminders-cli-dao.el ends here
