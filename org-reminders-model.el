;;; org-reminders-model.el --- Model for Org-reminders.  -*- lexical-binding: t; -*-

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

(require 'cl-lib)


(cl-defstruct org-reminders-model
  id
  title
  priority
  list-name
  completed
  external-id
  completion-date
  notes)

(defvar org-reminders-model-keyword-mapping
  '((id . "id")
    (title . "title")
    (priority . "priority")
    (list-name . "list")
    (completed . "isCompleted")
    (external-id . "externalId")
    (completion-date . "completionDate")))

(defun org-reminders-model-parse-json-str (json-str)
  (let ((json-obj (json-parse-string json-str
                                     :array-type 'list
                                     :false-object nil)))
    (org-reminders-model-parse-json-obj json-obj)))

(defun org-reminders-model-parse-json-obj (json-obj)
  (pcase (type-of json-obj)
    ('cons (mapcar #'org-reminders-model-parse-json-obj json-obj))
    ('hash-table (org-reminders-model-parse-hash-table json-obj))
    ('string json-obj)))

(defun org-reminders-model-parse-hash-table (htable)
  (let ((item (make-org-reminders-model)))
    (dolist (pair org-reminders-model-keyword-mapping)
      (eieio-oset item (car pair) (gethash (cdr pair) htable)))
    item))


(defun org-reminders-model-to-plist (obj)
  (let* ((slots (cl-struct-slot-info 'org-reminders-model))
         (properties (mapcar #'car (cdr slots))))
    (mapcan
     (lambda (property)
       (when (eieio-oref obj property)
         (list (intern (format ":%s" property))
               (eieio-oref obj property))))
     properties)))


(org-reminders-model-to-plist (make-org-reminders-model
                               :id 100
                               :external-id 10))

(provide 'org-reminders-model)
;;; org-reminders-model.el ends here
