;;; org-reminders.el --- An Emacs plugin for interacting between macOS Reminders and org mode.  -*- lexical-binding: t; -*-

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

(require 'org)
(require 'cl-seq)
(require 'org-reminders-cli-dao)
(require 'org-reminders-org-dao)


(defvar org-reminders--match-items nil)

(defvar org-reminders--new-reminders nil)

(defvar org-reminders--deleted-reminders nil)

(defvar org-reminders--deleted-todos nil)

(defcustom org-reminders-org-sync-file (file-name-concat
                                        user-emacs-directory
                                        "Reminders.org")
  "The org file.")

(defun org-reminders-sync ()
  (interactive)
  (org-reminders-org-dao-all-elements)
  (org-reminders-cli-dao-all-elements)
  (org-reminders--sync-list-names
   org-reminders-cli-dao--list-names
   org-reminders-org-dao--list-names)
  (org-reminders--sync-items
   org-reminders-cli-dao--items
   org-reminders-org-dao--items))


(defun org-reminders--sync-list-names (cli-list-names org-list-names)
  (dolist (list-name (append cli-list-names org-list-names))
    (unless (member list-name cli-list-names)
      (org-reminders-cli-dao-add-list list-name))
    (unless (member list-name org-list-names)
      (org-reminders-org-dao-add-list list-name))))

(defun org-reminders--sync-items (cli-models org-models)
  ;; Iterate through all Reminders.
  (dolist (cli-model cli-models)
    (let* ((external-id (org-reminders-model-external-id cli-model))
           (org-model (org-reminders-org-dao-get external-id)))
      (if org-model
          ;; If the external ID matches, synchronize the item in both Reminders and Org mode.
          (org-reminders--sync-item cli-model org-model)
        ;; otherwise, add the item to Org mode.
        (org-reminders-org-dao-add-item cli-model))))

  ;; Iterate through all org mode todos.
  (dolist (org-model org-models)
    (let* ((external-id (org-reminders-model-external-id org-model))
           (cli-model (org-reminders-cli-dao-get external-id))
           (id (org-reminders-model-id org-model))
           (new-model))
      (if (not external-id)
          ;; No external ID found in Org item; adding to Reminders.
          (progn
            (setq new-model (org-reminders-cli-dao-add-item org-model))
            (eieio-oset new-model 'id id)
            (org-reminders-org-dao-edit
             new-model))
        (unless cli-model
          ;; 1. External ID found in Org item.
          ;; 2. No matching item found in Reminders,
          ;; which indicates that the item may have been deleted.
          ;; Delete the item in the org mode file.
          (org-reminders-org-dao-delete org-model))))))

(defun org-reminders--sync-item (cli-model org-model)
  (let ((deleted (org-reminders-model-deleted org-model))
        (id (org-reminders-model-id org-model))
        (cli-hash (org-reminders-model-hash cli-model))
        (org-hash (org-reminders-model-hash org-model))
        (cli-last-modified (org-reminders-model-last-modified cli-model))
        (org-last-modified (org-reminders-model-last-modified org-model))
        (cli-completed (org-reminders-model-completed cli-model))
        (org-completed (org-reminders-model-completed org-model))
        (cli-title (org-reminders-model-title cli-model))
        (org-title (org-reminders-model-title org-model))
        (cli-notes (org-reminders-model-notes cli-model))
        (org-notes (org-reminders-model-notes org-model))
        (cli-priority (org-reminders-model-priority cli-model))
        (org-priority (org-reminders-model-priority org-model)))
    ;; When an org mode todo is manually marked DELETED,
    ;; delete the corresponding Reminders item.
    (when deleted
      (org-reminders-cli-dao-delete cli-model)
      (org-reminders-org-dao-delete org-model))
    (unless (equal cli-hash org-hash)
      (unless (string> cli-last-modified org-last-modified)
        (unless (equal cli-completed org-completed)
          (setq cli-model (org-reminders-cli-dao-toggle-completed cli-model)))
        (unless (and (equal cli-title org-title)
                     (equal cli-notes org-notes))
          (setq cli-model (org-reminders-cli-dao-edit org-model))))
      (eieio-oset cli-model 'id id)
      (org-reminders-org-dao-edit cli-model))))

(provide 'org-reminders)
;;; org-reminders.el ends here
