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

(defvar org-reminders-org-dao--list-names nil)
(defvar org-reminders-org-dao--items nil)

(defcustom org-reminders-org-sync-file "~/Downloads/Hello.org"
  "The org file.")


(defmacro org-reminders-org-dao--commands (key)
  `(pcase ,key
     (:goto-list
      (when list-name
        (goto-char (org-find-exact-headline-in-buffer list-name nil t)))
      (:insert-item
       (when title (progn (org-insert-subheading 0) (insert title))))
      (:insert-list-name (when title (insert list-name)))
      (:set-todo-default (org-todo "TODO"))
      (:set-todo (when completed
                   (org-todo (org-reminders-org-dao--convert-completed completed))))
      (:set-external-id (when (org-set-property "EXTERNAL-ID" external-id)))
      (:set-priority (when priorite (org-priority (org-reminders-org-dao--convert-priority priorite))))
      (:set-close (when completion-date
                    (org-add-planning-info 'closed completion-date))))))


(defun org-reminders-org-dao--run-commands (command-keys obj)
  (dolist (key command-keys)
    (org-reminders--run-in-obj
     (org-reminders-org-dao--commands key obj))))


(defvar org-reminders-org-dao--list-name-template
  '("\n* {:list-name} [/]"))

(defun org-reminders-org-dao--convert-completed (completed)
  (if completed
      "DONE"
    "TODO"))

(defun org-reminders-org-dao--convert-priority (priority)
  (pcase priority
    (0 nil)
    (1 ?C)
    (5 ?B)
    (9 ?A)
    (?A 9)
    (?B 5)
    (?C 1)))


(defun org-reminders-org-dao--headline-text (headline)
  (string-trim (replace-regexp-in-string
                " +\\[[^]]+\\]" ""
                (org-element-property :raw-value headline))))

(defun org-reminders-org-dao--parse-headline (headline)
  (let* ((level (org-element-property :level headline))
         (title (org-reminders-org-dao--headline-text headline)))
    (when (= 1 level) ;; List Name
      (setq org-reminders-org-dao--list-names
            (append org-reminders-org-dao--list-names
                    (list title))))
    (when (= 2 level)
      (let* ((parent (org-element-property :parent headline))
             (list-name (org-reminders-org-dao--headline-text parent))
             (heading-properties (org-entry-properties))
             (external-id (alist-get "EXTERNAL-ID" heading-properties nil nil #'string=))
             (priority (org-element-property :priority headline))
             (deleted (if (member "DELETED" (org-element-property :tags headline)) t nil))
             (heading-todo (org-element-property :todo-keyword headline))
             (reminders-mode-item))
        (setq reminders-mode-item (make-org-reminders-model))
        (setq org-reminders-org-dao--items
              (append org-reminders-org-dao--items
                      (list reminders-mode-item)))
        (eieio-oset reminders-mode-item 'title title)
        (eieio-oset reminders-mode-item 'list-name list-name)
        (eieio-oset reminders-mode-item 'completed heading-todo)
        (eieio-oset reminders-mode-item 'priority priority)
        (org-element-map headline 'paragraph
          (lambda (paragraph)
            (eieio-oset reminders-mode-item 'notes (substring-no-properties (nth 2 paragraph)))))))
    org-reminders-org-dao--items))

(defun org-reminders-org-dao-all-elements ()
  (setq org-reminders-org-dao--list-names nil)
  (setq org-reminders-org-dao--items nil)
  (let ((ast (org-element-parse-buffer)))
    (org-element-map ast 'headline
      #'org-reminders-org-dao--parse-headline)))


(defmacro with-struct-fields (struct &rest body)
  "Bind each field of STRUCT to a local variable and execute BODY.
  STRUCT should be a struct instance."
  (let* ((struct-name (symbol-name struct))
         (fields (mapcar #'intern (cl-struct-slot-info (intern struct-name)))))
    `(let ,(mapcar (lambda (field)
                     `(,field (,(intern (concat struct-name "-" (symbol-name field))) ,struct)))
                   fields)
       ,@body)))

(defmacro org-reminders-org-dao-run (&rest body)
  `(with-current-buffer (find-file-noselect ,org-reminders-org-sync-file)
     (save-excursion
       ,@body)
     (org-update-statistics-cookies t)))

(defun org-reminders-org-dao-add-list (list-name)
  (org-reminders-org-dao-run
   (goto-char (point-max))
   (insert
    (org-reminders--expand-list
     org-reminders-org-dao--list-name-template
     :list-name  list-name))))


(defun org-reminders-org-dao-add-item (model)
  "Refresh list names data."
  (with-current-buffer (find-file-noselect org-reminders-org-sync-file)
    (save-excursion
      (goto-char (org-find-exact-headline-in-buffer
                  (org-reminders-model-list-name model)
                  nil t))
      (apply #'org-reminders-org-dao--run-commands
             '(:goto-list :insert-item :set-todo-default :set-todo :set-priority :set-close)
             (org-reminders-model-to-plist model)))
    (org-update-statistics-cookies t)))

(org-reminders-org-dao-add-item
 (make-org-reminders-model
  :list-name "hello"
  :title "西瓜"
  :priority 9))



(defun org-reminders-org-dao-complete (model)
  "Refresh list names data."
  (org-reminders-org-dao--run-command
   :complete
   (org-reminders-model-to-plist model)))

(defun org-reminders-org-dao-uncomplete (model)
  "Refresh list names data."
  (org-reminders-org-dao--run-command
   :uncomplete
   (org-reminders-model-to-plist model)))

(defun org-reminders-org-dao-delete (model)
  "Refresh list names data."
  (org-reminders-org-dao--run-command
   :delete
   (org-reminders-model-to-plist model)))

(defun org-reminders-org-dao-edit (model)
  "Refresh list names data."
  (org-reminders-org-dao--run-command
   :delete
   (org-reminders-model-to-plist model)))


(provide 'org-reminders-org-dao)
;;; org-reminders-org-dao.el ends here
