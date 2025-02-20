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
(require 'org-reminders-dao)

(defcustom org-reminders-org-sync-file (file-name-concat
                                        user-emacs-directory
                                        "Reminders.org")
  "The org file.")

(defun org-reminders-sync ()
  "Synchronize Reminders with Org files."
  (interactive)
  (org-reminders-dao-all-elements 'cli)
  (org-reminders-dao-all-elements 'org)
  (org-reminders-sync-list-names
   (org-reminders-dao-list-names 'cli)
   (org-reminders-dao-list-names 'org))
  (org-reminders-sync-items
   (org-reminders-dao-items 'cli)
   (org-reminders-dao-items 'org)))


(defun org-reminders-sync-list-names (cli-list-names org-list-names)
  "Synchronize Reminders list names with Org files."
  (dolist (list-name (append cli-list-names org-list-names))
    (unless (member list-name cli-list-names)
      (org-reminders-dao-add-list 'cli list-name))
    (unless (member list-name org-list-names)
      (org-reminders-dao-add-list 'org list-name))))

(defun org-reminders-sync-items (cli-models org-models)
  "Synchronize Reminders items with Org files."
  ;; Iterate through all Reminders.
  (dolist (cli-model cli-models)
    (let* ((external-id (org-reminders-model-external-id cli-model))
           (org-model (org-reminders-org-dao-get external-id)))
      (if org-model
          ;; If the external ID matches, synchronize the item in both Reminders and Org mode.
          (org-reminders-sync-item cli-model org-model)
        ;; otherwise, add the item to Org mode.
        (org-reminders-dao-add-item 'org cli-model))))

  ;; Iterate through all org mode todos.
  (dolist (org-model org-models)
    (let* ((external-id (org-reminders-model-external-id org-model))
           (cli-model (org-reminders-cli-dao-get external-id))
           (id (org-reminders-model-id org-model))
           (new-model))
      (if (not external-id)
          ;; No external ID found in Org item; adding to Reminders.
          (progn
            (setq new-model (org-reminders-dao-add-item 'cli org-model))
            (eieio-oset new-model 'id id)
            (org-reminders-dao-edit 'org
                                    new-model))
        (unless cli-model
          ;; 1. External ID found in Org item.
          ;; 2. No matching item found in Reminders,
          ;; which indicates that the item may have been deleted.
          ;; Delete the item in the org mode file.
          (org-reminders-dao-delete 'org org-model))))))

(defun org-reminders-sync-item (cli-model org-model)
  "Synchronize Reminders an item with Org files."
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
      (org-reminders-dao-delete 'cli cli-model)
      (org-reminders-dao-delete 'org org-model))
    (unless (equal cli-hash org-hash)
      (unless (string> cli-last-modified org-last-modified)
        (unless (equal cli-completed org-completed)
          (setq cli-model (org-reminders-dao-toggle-completed 'cli cli-model)))
        (unless (and (equal cli-title org-title)
                     (equal cli-notes org-notes))
          (setq cli-model (org-reminders-dao-edit 'cli org-model))))
      (eieio-oset cli-model 'id id)
      (org-reminders-dao-edit 'org cli-model))))

(provide 'org-reminders)
;;; org-reminders.el ends here
