;;; org-reminders-org-dao.el --- Data Access for Org reminders org file.  -*- lexical-binding: t; -*-

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

(require 'org-element)
(require 'org-reminders-model)
(require 'org-reminders-utils)

(defvar org-reminders-org-dao--list-names nil)
(defvar org-reminders-org-dao--items nil)
(defvar org-reminders-org-dao--items-hashtable nil)

(defun org-reminders-org-dao--build-items-hashtable ()
  (setq org-reminders-org-dao--items-hashtable
        (org-reminders--build-items-hashtable org-reminders-org-dao--items)))

(defun org-reminders-org-dao-get (external-id)
  (gethash external-id org-reminders-org-dao--items-hashtable))

(defvar org-reminders-org-dao--commands
  '(
    :goto-list org-reminders-org-dao-goto-list
    :goto-id org-reminders-org-dao-goto-id
    :get-id org-reminders-org-dao-get-id
    :get-model org-reminders-org-dao-get-model
    :goto-external-id org-reminders-org-dao-goto-external-id
    :new-item org-reminders-org-dao-new-item
    :edit-item org-reminders-org-dao-edit-item
    :add-notes org-reminders-org-dao-add-notes
    :delete-item org-reminders-org-dao-delete-item
    :edit-list-name org-reminders-org-dao-edit-list-name
    :set-todo org-reminders-org-dao-set-todo
    :set-id org-reminders-org-dao-set-id
    :set-external-id org-reminders-org-dao-set-external-id
    :set-last-modified org-reminders-org-dao-set-last-modified
    :set-priority org-reminders-org-dao-set-priority
    :set-close-date org-reminders-org-dao-set-close-date
    :set-hash org-reminders-org-dao-set-hash
    :set-schedule org-reminders-org-dao-set-schedule))

(defvar org-reminders-org-dao--edit-keys
  '(
    :set-external-id
    :set-last-modified
    :set-todo
    :set-priority
    :set-close-date
    :set-hash
    :set-schedule
    :add-notes))



(defun org-reminders-org-dao--run-command (key model)
  (funcall (plist-get org-reminders-org-dao--commands key) model))

(defun org-reminders-org-dao-get-model (model)
  (org-reminders-org-dao--parse-headline (org-element-at-point)))

(defun org-reminders-org-dao-goto-external-id (model)
  (org-reminders-with-model-fields
   model
   (goto-char (point-min))
   (search-forward-regexp
    (format ":EXTERNAL-ID:\\s-*%s" external-id))))

(defun org-reminders-org-dao-get-id (model)
  (org-reminders-with-model-fields
   model
   (org-id-get)))

(defun org-reminders-org-dao-goto-id (model)
  (org-reminders-with-model-fields
   model
   (goto-char (point-min))
   (search-forward-regexp
    (format ":ID:\\s-*%s" id))))

(defun org-reminders-org-dao-delete-item (model)
  (org-reminders-with-model-fields
   model
   (org-cut-subtree 1)))

(defun org-reminders-org-dao-set-id (model)
  (org-reminders-with-model-fields
   model
   (org-id-get-create)))

(defun org-reminders-org-dao-goto-list (model)
  (org-reminders-with-model-fields
   model
   (when list-name
     (let ((position (org-find-exact-headline-in-buffer list-name nil t)))
       (when position
         (goto-char position))))))


(defun org-reminders-org-dao-new-item (model)
  (org-reminders-with-model-fields
   model
   (when title
     (progn
       (end-of-line 1)
       (org-insert-subheading 0)
       (org-edit-headline title)))))

(defun org-reminders-org-dao-add-notes (model)
  (org-reminders-with-model-fields
   model
   (when notes
     (org-end-of-meta-data)
     (insert notes))))



(defun org-reminders-org-dao-edit-item (model)
  (org-reminders-with-model-fields
   model
   (when title
     (progn
       (org-edit-headline title)))))

(defun org-reminders-org-dao-edit-list-name (model)
  (org-reminders-with-model-fields
   model
   (when list-name
     (progn (org-insert-heading 0)
            (org-edit-headline (format "%s [/]" list-name))))))

(defun org-reminders-org-dao-set-todo (model)
  (org-reminders-with-model-fields
   model
   (org-todo (org-reminders-org-dao--convert-completed completed))))



(defun org-reminders-org-dao-set-last-modified (model)
  (org-reminders-with-model-fields
   model
   (when last-modified (org-set-property
                        "LAST-MODIFIED"
                        (org-reminders--local-timezone-readable last-modified)))))


(defun org-reminders-org-dao-set-external-id (model)
  (org-reminders-with-model-fields
   model
   (when external-id (org-set-property "EXTERNAL-ID" external-id))))

(defun org-reminders-org-dao-set-priority (model)
  (org-reminders-with-model-fields
   model
   (when (org-reminders--priority-convert 'model 'org priority)
     (org-priority (org-reminders--priority-convert 'model 'org priority)))))

(defun org-reminders-org-dao-set-schedule (model)
  (org-reminders-with-model-fields
   model
   (when due-date
     (org-add-planning-info
      'scheduled
      (org-reminders--local-timezone-readable due-date)))))

(defun org-reminders-org-dao-set-hash (model)
  (org-reminders-with-model-fields
   model
   (when hash
     (org-set-property "HASH" hash))))

(defun org-reminders-org-dao-set-close-date (model)
  (org-reminders-with-model-fields
   model
   (when completion-date
     (org-add-planning-info
      'closed
      (org-reminders--local-timezone-readable completion-date)))))

(defun org-reminders-org-dao--run-commands (command-keys model)
  (let (result)
    (dolist (key command-keys)
      (setq result (org-reminders-org-dao--run-command key model)))
    result))

(defun org-reminders-org-dao--convert-completed (completed)
  (if completed
      "DONE"
    "TODO"))


(defun org-reminders-org-dao--headline-text (headline)
  (string-trim (replace-regexp-in-string
                " +\\[.*?\\]$" ""
                (org-element-property :raw-value headline))))

(defun org-reminders-org-dao--get-or-create-id (headline)
  (let ((id (org-id-get headline)))
    (unless id
      (save-excursion
        (goto-char (org-element-property :begin headline))
        (setq id (org-id-get-create))))
    id))

(defun org-reminders-org-dao--get-content (headline)
  (save-excursion
    (let ((content-begin)
          (content-end)
          (content))
      (goto-char (org-element-property :begin headline))
      (org-narrow-to-subtree)
      (setq content-begin (progn (org-end-of-meta-data) (point)))
      (setq content-end (point-max))
      (setq section-content
            (string-trim (buffer-substring-no-properties content-begin content-end)))
      (setq section-content (unless (string-empty-p section-content) section-content))
      (widen)
      section-content)))

(defun org-reminders-org-dao--parse-headline (headline)
  (let* ((level (org-element-property :level headline))
         (title (org-reminders-org-dao--headline-text headline)))
    (when (= 1 level) ;; List Name
      (setq org-reminders-org-dao--list-names
            (append org-reminders-org-dao--list-names
                    (list title))))
    (when (= 2 level) ;; TODO Item
      (let* ((parent (org-element-property :parent headline))
             (list-name (org-reminders-org-dao--headline-text parent))
             (external-id (org-entry-get headline "EXTERNAL-ID"))
             (last-modified (org-reminders--to-time-utc
                             (org-entry-get headline "LAST-MODIFIED")))
             (due-date (org-reminders--time-utc (org-get-scheduled-time headline)))
             (hash (org-entry-get headline "HASH"))
             (priority (org-element-property :priority headline))
             (deleted (if (member "DELETED" (org-element-property :tags headline)) t nil))
             (heading-todo (org-element-property :todo-keyword headline))
             (id (org-reminders-org-dao--get-or-create-id headline))
             (reminders-mode-item (make-org-reminders-model)))
        (eieio-oset reminders-mode-item 'id id)
        (eieio-oset reminders-mode-item 'title title)
        (eieio-oset reminders-mode-item 'list-name list-name)
        (eieio-oset reminders-mode-item 'completed (equal "DONE" heading-todo))
        (eieio-oset reminders-mode-item 'priority
                    (org-reminders--priority-convert 'org 'model priority))
        (eieio-oset reminders-mode-item 'external-id external-id)
        (eieio-oset reminders-mode-item 'last-modified last-modified)
        (eieio-oset reminders-mode-item 'deleted deleted)
        (eieio-oset reminders-mode-item 'notes
                    (org-reminders-org-dao--get-content headline))
        (eieio-oset reminders-mode-item 'hash hash)
        (eieio-oset reminders-mode-item 'due-date due-date)
        (setq org-reminders-org-dao--items
              (append org-reminders-org-dao--items
                      (list (org-reminders-model-check-hash
                             reminders-mode-item))))))
    org-reminders-org-dao--items))

(defun org-reminders-org-dao-all-elements ()
  (setq org-reminders-org-dao--list-names nil)
  (setq org-reminders-org-dao--items nil)
  (org-reminders-org-dao-with-sync-file
   (let ((ast (org-element-parse-buffer)))
     (org-element-map ast 'headline
       #'org-reminders-org-dao--parse-headline)))
  (org-reminders-org-dao--build-items-hashtable))


(defun org-reminders-org-dao-add-list (list-name)
  (let ((model (make-org-reminders-model
                :list-name list-name)))
    (org-reminders-org-dao-with-sync-file
     (goto-char (point-max))
     (org-reminders-org-dao--run-commands
      '(:edit-list-name)
      model))))


(defmacro org-reminders-org-dao-with-sync-file (&rest body)
  `(with-current-buffer (find-file-noselect ,org-reminders-org-sync-file)
     (save-excursion
       (goto-char (point-min))
       ,@body
       (org-update-statistics-cookies t))))


(defun org-reminders-org-dao-add-item (model)
  "Refresh list names data."
  (org-reminders-org-dao-with-sync-file
   (org-reminders-org-dao--run-commands
    (append '(:goto-list :new-item :set-id)
            org-reminders-org-dao--edit-keys)
    model)))

(defun org-reminders-org-dao-edit (model)
  "Refresh list names data."
  (org-reminders-org-dao-with-sync-file
   (org-reminders-org-dao--run-commands
    (append '(:goto-id :edit-item)
            org-reminders-org-dao--edit-keys)
    model)))

(defun org-reminders-org-dao-delete (model)
  "Refresh list names data."
  (org-reminders-org-dao-with-sync-file
   (org-reminders-org-dao--run-commands
    '(:goto-external-id :delete-item)
    model)))

(provide 'org-reminders-org-dao)
;;; org-reminders-org-dao.el ends here
