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
(require 'transient)

(cl-defstruct org-reminders-item
  title
  external-id
  priority
  completed
  deleted
  closed
  scheduled
  last-modified
  org-list
  notes
  hash)

(cl-defstruct org-reminders-list
  title
  id)

(cl-defstruct org-reminders-log
  time
  target
  type
  action
  id
  data)

(defmacro org-reminders-with-subtree (&rest body)
  `(progn
     (org-narrow-to-subtree)
     ,@body
     (widen)))

(defmacro org-reminders-with-sync-file (&rest body)
  `(with-current-buffer (find-file-noselect ,org-reminders-sync-file)
     (save-excursion
       (goto-char (point-min))
       ,@body
       (save-buffer))))

(defcustom org-reminders-cli-command (executable-find "org-reminders")
  "")

(defcustom org-reminders-include-completed t
  "Show completed reminders?")

(defcustom org-reminders-sync-file (expand-file-name "~/.emacs.d/Reminders.org")
  "")

(defvar org-reminders--buffer-name "*reminders*")

(defvar org-reminders--cli-process nil)

(defvar org-reminders--cli-process-buffer "*org-reminders-cli*")

(defvar org-reminders--groups nil)

(defvar org-reminders--lists nil)

(defvar org-reminders--log-str-buffer nil)

(defvar org-reminders--priorities
  '((low ?C 9)
    (medium ?B 5)
    (high ?A 1)))

(defvar org-reminders-keymaps
  '("externalId" external-id
    "isCompleted" completed
    "lastModified" last-modified
    "dueDate" scheduled
    "list" org-list
    "completionDate" closed))

(defun org-reminders-handle-list (action data)
  (let ((obj (org-reminders-parse-list-data data)))
    (funcall (intern (format "org-reminders--list-%s" (downcase action))) obj)))

(defun org-reminders-parse-item-data (data)
  (let* ((obj (org-reminders-parse-data data #'make-org-reminders-item))
         (json (json-encode (eieio-oref obj 'org-list))))
    (eieio-oset obj 'org-list (org-reminders-parse-list-data json))
    obj))

(defun org-reminders--filter (process output)
  (setq org-reminders--log-str-buffer (concat org-reminders--log-str-buffer output))
  (let ((match-log (org-reminders--parse-log org-reminders--log-str-buffer)))
    (while match-log
      (org-reminders-reaction (cdr match-log))
      (setq org-reminders--log-str-buffer (substring org-reminders--log-str-buffer
                                                     (car match-log)))
      (setq match-log (org-reminders--parse-log org-reminders--log-str-buffer))))
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)))

(defun org-reminders--item-update-detail (obj)
  (let ((hash (org-reminders-item-hash obj))
        (title (org-reminders-item-title obj))
        (notes (org-reminders-item-notes obj))
        (last-modified (org-reminders-item-last-modified obj))
        (external-id (org-reminders-item-external-id obj))
        (closed (org-reminders-item-closed obj))
        (scheduled (org-reminders-item-scheduled obj))
        (completed (org-reminders-item-completed obj)))
    (when hash (org-set-property "HASH" hash))
    (when last-modified (org-set-property "LAST-MODIFIED" last-modified))
    (when external-id (org-set-property "EXTERNAL-ID" external-id))
    (when title (org-edit-headline title))
    (when closed (org-add-planning-info 'closed closed))
    (when scheduled (org-add-planning-info 'scheduled scheduled))
    (org-todo (if (equal :false completed) 'todo 'done))))

(defun org-reminders--insert-item-str (obj)
  (let((title (org-reminders-item-title obj)))
    (insert "\n** " title)
    (org-reminders--item-update-detail obj)))

(defun org-reminders--item-add (obj)
  (let ((org-list (org-reminders-item-org-list obj)))
    (org-reminders-with-sync-file
     (when (org-reminders--list-locate-by-id org-list)
       (org-reminders-with-subtree
        (goto-char (point-max))
        (org-reminders--insert-item-str obj))))))

(defun org-reminders--item-locate-by-id (obj)
  (let ((id (org-reminders-item-external-id obj)))
    (goto-char (point-min))
    (search-forward-regexp (format ":EXTERNAL-ID:\s*%s" id) nil t)))

(defun org-reminders--item-delete (obj)
  (org-reminders-with-sync-file
   (when (org-reminders--item-locate-by-id obj)
     (org-reminders-with-subtree
      (delete-region (point-min) (point-max))))))

(defun org-reminders--locate-headline-by-name-and-level (name level)
  "Locate an Org headline by NAME and LEVEL using org-element API.
NAME is the headline's name, and LEVEL is its level (e.g., 1 for '*', 2 for '**', etc.)."
  (let ((tree (org-element-parse-buffer 'headline))
        found)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (and (string= (org-element-property :raw-value hl) name)
                   (= (org-element-property :level hl) level))
          (setq found hl)))
      nil 'first-match (= level 1))
    (if found
        (goto-char (org-element-property :begin found)))))

(defun org-reminders--item-locate-by-title (obj)
  (let ((title (org-reminders-item-title obj)))
    (goto-char (point-min))
    (print title)
    (org-reminders--locate-headline-by-name-and-level
     title 2)))

(defun org-reminders--item-update (obj)
  (org-reminders-with-sync-file
   (when (or (org-reminders--item-locate-by-id obj)
             (org-reminders--item-locate-by-title obj))
     (org-reminders-with-subtree
      (org-reminders--item-update-detail obj)))))

(defun org-reminders--list-add (obj)
  (let ((id (org-reminders-list-id obj))
        (title (org-reminders-list-title obj)))
    (org-reminders-with-sync-file
     (goto-char (point-max))
     (insert (format "\n* %s [0/0]" title))
     (when id (org-set-property "LIST-ID" id)))))

(defun org-reminders--list-locate-by-id (obj)
  (let ((id (org-reminders-list-id obj)))
    (goto-char (point-min))
    (search-forward-regexp (format ":LIST-ID:\s*%s" id) nil t)))

(defun org-reminders--list-delete (obj)
  (org-reminders-with-sync-file
   (when (org-reminders--list-locate-by-id obj)
     (org-reminders-with-subtree
      (delete-region (point-min) (point-max))))))

(defun org-reminders--list-locate-by-title (obj)
  (let ((title (org-reminders-list-title obj)))
    (goto-char (point-min))
    (org-reminders--locate-headline-by-name-and-level
     title 1)))

(defun org-reminders--list-update (obj)
  (let ((id (org-reminders-list-id obj))
        (title (org-reminders-list-title obj)))
    (org-reminders-with-sync-file
     (when (or (org-reminders--list-locate-by-id obj)
               (org-reminders--list-locate-by-title obj))
       (org-reminders-with-subtree
        (when title (org-edit-headline (format "%s [0/0]" title)))
        (when id (org-set-property "LIST-ID" id)))))))

(defun org-reminders--parse-log (output)
  (when (string-match (concat "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]\n")

                      output)
    (cons (match-end 0)
          (make-org-reminders-log
           :time (match-string 1 output)
           :target (match-string 2 output)
           :type (match-string 3 output)
           :action (match-string 4 output)
           :id (match-string 5 output)
           :data (base64-decode-string (match-string 6 output))))))

(defun org-reminders-parse-data (data type)
  (let* ((htable (json-parse-string data))
         (keys (hash-table-keys htable))
         (property)
         (obj (funcall type)))
    (dolist (key keys)
      (setq property (plist-get org-reminders-keymaps key #'equal))
      (unless property
        (setq property (intern key)))
      (eieio-oset obj property (gethash key htable)))
    obj))

(defun org-reminders-handle-item (action data)
  (let ((obj (org-reminders-parse-item-data data)))
    (funcall (intern (format "org-reminders--item-%s" (downcase action))) obj)))

(defun org-reminders-reaction (log)
  (when (string= "Org Mode" (org-reminders-log-target log))
    (let ((action (org-reminders-log-action log))
          (type (org-reminders-log-type log))
          (data (org-reminders-log-data log)))
      (pcase type
        ("CommonList" (org-reminders-handle-list action data))
        ("CommonReminder" (org-reminders-handle-item action data)))))

  (defun org-reminders-parse-list-data (data)
    (org-reminders-parse-data data #'make-org-reminders-list)))

(defun org-reminders-start-auto-sync ()
  (if org-reminders--cli-process
      (message "org-reminders-auto-sync has already started.")
    (setq org-reminders--cli-process
          (start-process "org-reminders-cli"
                         org-reminders--cli-process-buffer
                         org-reminders-cli-command
                         "sync"
                         org-reminders-sync-file
                         "-t"
                         "auto")))
  (set-process-filter org-reminders--cli-process #'org-reminders--filter))

(defun org-reminders-stop-auto-sync ()
  (interactive)
  (when (and org-reminders--cli-process
             (process-live-p org-reminders--cli-process))
    (kill-process org-reminders--cli-process))
  (with-current-buffer (get-buffer-create org-reminders--cli-process-buffer)
    (erase-buffer))
  (setq org-reminders--cli-process nil)
  (setq org-reminders--log-str-buffer nil))

(defun org-reminders-restart-auto-sync ()
  (interactive)
  (org-reminders-stop-auto-sync)
  (org-reminders-start-auto-sync))

(defun org-reminders-sync-all ()
  (interactive)
  (start-process "org-reminders-cli"
                 org-reminders--cli-process-buffer
                 org-reminders-cli-command
                 "sync"
                 org-reminders-sync-file
                 "-t"
                 "all"))



(provide 'org-reminders)
;;; org-reminders.el ends here
