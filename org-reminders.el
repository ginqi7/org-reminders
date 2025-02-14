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

(defvar org-reminders--buffer-name "*reminders*")

(defvar org-reminders--groups nil)

(defvar org-reminders--lists nil)

(defvar org-reminders--priorities
  '((low ?C 9)
    (medium ?B 5)
    (high ?A 1)))

(defvar org-reminders-commands
  (list
   :add '("reminders add '{:list-name}' '{:title}'"
          " --due-date '{:due-date}'"
          " --priority '{:priority}'"
          " --notes '{:notes}'")
   :complete '("reminders complete '{:list-name}' '{:external-id}'")
   :uncomplete '("reminders uncomplete '{:list-name}' '{:external-id}'")
   :delete '("reminders delete '{:list-name}' '{:external-id}'")
   :edit '("reminders edit '{:list-name}' '{:external-id}' '{:title}'"
           " --notes '{:notes}'")
   :show '("reminders show '{:list-name}' --include-completed -f json")
   :show-all '("reminders show-all --include-completed -f json")
   :show-lists '("reminders show-lists -f json")
   :new-list '("reminders new-list '{:list-name}'")))

(defvar org-reminders-org-template
  '("\n** {:state}"  " {:priority}" " {:title}"
    "\nCLOSED: {:date}"
    "\n:PROPERTIES:"
    "\n:EXTERNAL-ID: {:external-id}"
    "\n:END:"
    "\n{:notes}"))

(defun org-reminders--expand-str (template &rest args)
  "Expand template str by args."
  (let* ((start 0)
         (str template)
         key value match-str break)
    (while (and
            (null break)
            (string-match "{\\(:.*?\\)}" template start))
      (setq start (match-end 0))
      (setq match-str (match-string 0 template))
      (setq key (intern (match-string 1 template)))
      (setq value (plist-get args key))
      (unless value
        (setq break t))
      (setq value (format "%s" value))
      (setq str (string-replace match-str value str)))
    (if break
        ""
      str)))

(defun org-reminders (&optional position)
  "Render reminders list by org-mode."
  (interactive)
  (org-reminders--refresh-data)
  (switch-to-buffer org-reminders--buffer-name)
  (org-reminders--insert-data)
  (unless position
    (setq position (point-min)))
  (goto-char position)
  (org-reminders-mode))

(defun org-reminders--add (reminder)
  "Add REMINDER to reminders."
  (let* ((list-name (gethash "list" reminder))
         (title (gethash "title" reminder))
         (notes (gethash "notes" reminder))
         (priority (gethash "priority" reminder)))

    (setq priority
          (car
           (find-if (lambda (item) (equal priority (nth 2 item)))
                    org-reminders--priorities)))
    (org-reminders-add-reminder
     :list-name list-name
     :title title
     :notes notes
     :priority priority)))


(defun org-reminders--convert-date (date-str)
  "Convert reminders DATE-STR to org date string."
  (when date-str
    (concat "["
            (string-replace "Z" "" (string-replace "T" " " date-str))
            "]")))

(defun org-reminders--expand-list (lst &rest args)
  "Expand template list by args."
  (string-join
   (mapcar
    (lambda (str) (apply #'org-reminders--expand-str str args))
    lst)))

(defun org-reminders--delete (reminder)
  "Delete REMINDER."
  (let* ((list-name (gethash "list" reminder))
         (external-id (gethash "externalId" reminder)))
    (org-reminders-run-command :delete
                               :list-name list-name
                               :external-id external-id)))

(defun org-reminders--edit (reminder original-reminder)
  (let* ((list-name (gethash "list" reminder))
         (external-id (gethash "externalId" reminder))
         (new-title (gethash "title" reminder))
         (new-notes (gethash "notes" reminder))
         (origin-title (gethash "title" original-reminder))
         (origin-notes (gethash "notes" original-reminder)))
    (unless (and (equal new-title origin-title)
                 (equal new-notes origin-notes))
      (org-reminders-run-command :edit
                                 :list-name list-name
                                 :external-id external-id
                                 :title new-title
                                 :notes new-notes))))

(defun org-reminders--find-original-reminder (reminder)
  "Find original reminder by ID."
  (when (and reminder
             org-reminders--groups)
    (when-let* ((list-name (gethash "list" reminder))
                (external-id (gethash "externalId" reminder))
                (reminders (gethash list-name org-reminders--groups)))
      (gethash external-id reminders))))

(defun org-reminders--get-list (heading)
  "Get list name from HEADING."
  (replace-regexp-in-string
   " +\\[[^]]+\\]" ""
   (org-element-property :title heading)))

(defun org-reminders--get-parent-heading (heading)
  "Get parent heading by HEADING."
  (let* ((level (org-element-property :level heading))
         (parent-level (1- level))
         (parent nil)
         (save-excursion
           (while (and (not parent)
                       (org-up-heading-safe))
             (let ((current (org-element-at-point)))
               (when (= (org-element-property :level current) parent-level)
                 (setq parent current))))))
    parent))

(defun org-reminders--toggle-priority-type (priority)
  "Toggle priority type between Org mode and Reminders."
  (when (and priority
             (not (equal priority 0)))
    (let ((input-index)
          (output-index))
      (if (< priority 10) ;; Reminders uses a priority scale of 1, 5, and 9.
          (setq input-index 2
                output-index 1)
        (setq input-index 1
              output-index 2))
      (nth output-index (find-if (lambda (item) (equal priority (nth input-index item))) org-reminders--priorities)))))

(defun org-reminders--get-reminder (heading)
  "Get reminder at current pointer."
  (let ((reminder (make-hash-table :test #'equal)))
    (save-excursion
      (org-narrow-to-subtree)
      (let* ((parent-heading)
             (heading-title (org-element-property :raw-value heading))
             (heading-todo (org-element-property :todo-keyword heading))
             (heading-properties (org-entry-properties))
             (external-id (alist-get "EXTERNAL-ID" heading-properties nil nil #'string=))
             (priority (org-element-property :priority heading))
             (content-begin (progn (org-end-of-meta-data) (point)))
             (content-end (point-max))
             (section-content (buffer-substring-no-properties content-begin content-end))
             (section-content (unless (string-empty-p section-content) section-content)))
        (widen)
        (setq parent-heading (org-reminders--get-parent-heading heading))
        ;; (print parent-heading)
        (puthash "externalId" external-id reminder)
        (puthash "priority" (org-reminders--toggle-priority-type priority) reminder)
        (puthash "notes" section-content reminder)
        (puthash "title" heading-title reminder)
        (puthash "list" (org-reminders--get-list parent-heading) reminder)
        (puthash "isCompleted" (string= "DONE" heading-todo)
                 reminder)
        (puthash "deleted" (string= "DELETED" heading-todo)
                 reminder)
        reminder))))

(defun org-reminders--get-element ()
  "Get Element in current pointer."
  (save-excursion
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (let* ((heading (org-element-at-point))
           (heading-level (org-element-property :level heading)))
      (widen)
      (if (= 1 heading-level)
          (cons 'list (org-reminders--get-list heading))
        (cons 'reminder (org-reminders--get-reminder heading))))))

(defun org-reminders--get-reminder-in-list (list-name)
  "Get items in list by LIST-NAME."
  (org-reminders-run-command :show
                             :list-name list-name
                             :parse-json t))

(defun org-reminders-insert-reminder (reminder)
  "Insert reminder."
  (insert (org-reminders--expand-list
           org-reminders-org-template
           :state (if (gethash "isCompleted" reminder) "DONE" "TODO")
           :title (gethash "title" reminder)
           :date (org-reminders--convert-date (gethash "completionDate" reminder))
           :external-id  (gethash "externalId" reminder)
           :priority (if (org-reminders--toggle-priority-type
                          (gethash "priority" reminder))
                         (format
                          "[#%s]"
                          (char-to-string
                           (org-reminders--toggle-priority-type
                            (gethash "priority" reminder))))
                       "")
           :notes (gethash "notes" reminder))))

(defun org-reminders--insert-list (list-name)
  "Insert List."
  (let ((reminders (hash-table-values (gethash list-name org-reminders--groups))))
    (insert (format "\n* %s [/]\n" list-name))
    (seq-do #'org-reminders-insert-reminder reminders)))

(defun org-reminders--insert-data ()
  "Insert reminders data into buffer."
  (let ((buffer-read-only))
    (erase-buffer)
    (insert "#+TITLE: Reminders\n")
    (seq-do #'org-reminders--insert-list org-reminders--lists)
    (org-reminders-mode)))

(defun org-reminders-run-command (command-key &rest args)
  "Run reminders cli command."
  (let ((lst (plist-get org-reminders-commands command-key))
        (command-str)
        (result-str)
        (result))
    (setq command-str (apply #'org-reminders--expand-list lst args))
    (message command-str)
    (setq result-str (shell-command-to-string command-str))
    (condition-case err
        (when (plist-get args :parse-json)
          (setq result (json-parse-string result-str
                                          :array-type 'list
                                          :false-object nil)))
      (json-parse-error
       (message "An error occurred: %s" result-str)))
    result))

(defun org-reminders--refresh-lists-data ()
  "Refresh list names data."
  (setq org-reminders--lists (org-reminders-run-command :show-lists :parse-json t)))

(defun org-reminders--refresh-data ()
  "Refresh data."
  (org-reminders--refresh-lists-data)
  (setq org-reminders--groups (make-hash-table :test #'equal))
  (dolist (list-name org-reminders--lists)
    (puthash list-name (make-hash-table :test #'equal) org-reminders--groups))
  (dolist (reminder (org-reminders-run-command :show-all :parse-json t))
    (let* ((external-id (gethash "externalId" reminder))
           (list-name  (gethash "list" reminder))
           (reminders (gethash list-name org-reminders--groups)))
      (when reminders
        (puthash external-id reminder reminders)
        (puthash list-name reminders org-reminders--groups)))))

(defun org-reminders--toggle-state (reminder original-reminder)
  "Toggle reminder state."
  (let* ((new-completed (gethash "isCompleted" reminder))
         (origin-completed (gethash "isCompleted" original-reminder))
         (list-name (gethash "list" reminder))
         (external-id (gethash "externalId" reminder))
         (command-key (if new-completed :complete :uncomplete)))
    (unless (equal new-completed origin-completed)
      (org-reminders-run-command command-key
                                 :list-name list-name
                                 :external-id external-id))))

(defun org-reminders-delete-reminder (reminder)
  "Delete REMINDER."
  (let ((external-id (gethash "externalId" reminder))
        (list-name (gethash "list" reminder)))
    (org-reminders-run-command :delete
                               :external-id external-id
                               :list-name list-name)))

(defun org-reminders-delete-element ()
  "Delete Element in current pointer."
  (interactive)
  (let ((element (org-reminders--get-element))
        (position (point)))
    (pcase (car element)
      ('reminder (org-reminders-delete-reminder (cdr element))))))

(defun org-reminders-sync-list (list-name)
  "Sync list."
  (unless (member list-name org-reminders--lists)
    (org-reminders-add-list list-name)))

(defun org-reminders-sync-reminder (reminder)
  "Sync reminders reminder."
  (let* ((original-reminder (org-reminders--find-original-reminder reminder))
         (point (point)))
    (if (gethash "deleted" reminder)
        (org-reminders--delete reminder)
      (if original-reminder
          (progn
            (org-reminders--toggle-state reminder original-reminder)
            (org-reminders--edit reminder original-reminder))
        (org-reminders--add reminder)))))

(defun org-reminders-sync-element ()
  "Sync Element in current pointer."
  (let ((element (org-reminders--get-element))
        (position (point)))
    (pcase (car element)
      ('reminder (org-reminders-sync-reminder (cdr element)))
      ('list (org-reminders-sync-list (cdr element))))))

(defun org-reminders-sync ()
  "Sync Element in current buffer."
  (interactive)
  (let ((position (point)))
    (save-excursion
      (org-fold-show-all)
      (goto-char (point-min))
      (while (= 0 (org-next-visible-heading 1))
        (org-reminders-sync-element))
      (org-reminders position))))

(defun org-reminders-add-list (&optional list-name)
  "Add List."
  (interactive)
  (unless list-name
    (setq list-name (read-string "Please input a list name: ")))
  (org-reminders-run-command :new-list
                             :list-name list-name))

(defun org-reminders-add-reminder (&rest args)
  "Add reminder.
  :list-name list name.
  :title title
  :priority priority
  :notes notes"
  (interactive)
  (let ((list-name (plist-get args :list-name))
        (title (plist-get args :title))
        (priority (plist-get args :priority))
        (notes (plist-get args :notes)))
    (org-reminders--refresh-lists-data)
    (unless list-name
      (setq list-name (completing-read "Please select a list name: "
                                       org-reminders--lists)))
    (when (string-empty-p list-name)
      (error "list name is required."))
    (unless title (setq title (read-string "Please input a title: ")))
    (when (string-empty-p title)
      (error "title must is required."))
    (unless priority (setq priority (completing-read "Please select a priority: "
                                                     org-reminders--priorities)))
    (unless notes (setq notes (read-string "Please input a notes: ")))
    (when (string-empty-p priority)
      (setq priority nil))
    (when (string-empty-p notes)
      (setq notes nil))

    (org-reminders-run-command :add
                               :list-name list-name
                               :title title
                               :notes notes
                               :priority priority)))


(transient-define-prefix org-reminders-add-element ()
  "Add Reminders Element."
  ["Add Reminders Element"
   ("l" "Add List" org-reminders-add-list)
   ("r" "Add Reminder" org-reminders-add-reminder)])

(define-derived-mode org-reminders-mode org-mode "Org-Reminders"
  "Major mode for managing reminders in Org mode."
  (defface org-deleted
    '((t (:foreground "gray" :slant italic :strike-through t)))
    "Face for DELETED headlines in Org mode.")
  ;;Set task status
  (setq org-todo-keywords '((sequence "TODO" "DONE" "DELETED")))
  ;; Set status color
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("DONE" . org-done)
          ("DELETED" . org-deleted)))

  (org-fold-hide-sublevels 1)
  (org-update-statistics-cookies t)
  (org-cycle))

(transient-define-prefix org-reminders-prefix ()
  "Prefix for Org Reminders."
  ["Org Reminders Commands"
   ("r" "Show Reminders" org-reminders)
   ("s" "Sync Reminders" org-reminders-sync)
   ("a" "Add Element" org-reminders-add-element)
   ("d" "Delete Reminders" org-reminders-delete-element)])


(provide 'org-reminders)
;;; org-reminders.el ends here
