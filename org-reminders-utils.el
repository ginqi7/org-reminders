;;; org-reminders-utils.el --- Utils for Org Reminders.  -*- lexical-binding: t; -*-

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

(defun org-reminders--expand-list (lst &rest args)
  "Expand template list by args."
  (string-join
   (mapcar
    (lambda (str) (apply #'org-reminders--expand-str str args))
    lst)))

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

(defun org-reminders--build-items-hashtable (items)
  (let ((hashtable (make-hash-table :test #'equal))
        id external-id)
    (dolist (item items)
      (setq external-id (org-reminders-model-external-id item))
      (setq id (org-reminders-model-id item))
      (when id (puthash id item hashtable))
      (when external-id (puthash external-id item hashtable))
      (when (and id external-id)
        (puthash (format "%s:%s" id external-id) item hashtable)))
    hashtable))



(defmacro org-reminders-with-model-fields (model &rest body)
  (let* ((type 'org-reminders-model)
         (slot-info (cl-struct-slot-info type))
         (fields (mapcar #'car (cdr slot-info))))
    `(let ,(mapcar
            (lambda (field)
              `(,field
                (,(intern (format "%s-%s" type field)) ,model)))
            fields)
       ,@body)))


(defun org-reminders--current-time-utc ()
  "Return the current time in UTC format (ISO 8601)."
  (org-reminders--time-utc (current-time)))

(defun org-reminders--time-utc (time)
  "Return time in UTC format (ISO 8601)."
  (when time
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)))

(defun org-reminders--to-time-utc (str)
  "Return time in UTC format (ISO 8601)."
  (when str
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" (date-to-time str) t)))

(defun org-reminders--local-timezone-readable (utc-time)
  "Convert an ISO8601 time string to the current system timezone."
  (when utc-time
    (let ((time (date-to-time utc-time)))
      (format-time-string "%Y-%m-%d %H:%M:%S" time))))


(defun org-reminders--md5 (obj)
  (let ((contains-attributes
         '(title
           priority
           list-name
           completed
           external-id
           due-date notes)))
    (md5
     (format
      "%s"
      (mapcar
       (lambda (key)
         (eieio-oref obj key))
       contains-attributes)))))


(defun org-reminders--priority-convert (in-type out-type priority)
  (let ((priorities '((nil 0 nil)
                      (low 9 ?C)
                      (medium 5 ?B)
                      (high 1 ?A)))
        (in-index)
        (out-index))
    (pcase in-type
      ('cli (setq in-index 0))
      ('model (setq in-index 1))
      ('org (setq in-index 2)))
    (pcase out-type
      ('cli (setq out-index 0))
      ('model (setq out-index 1))
      ('org (setq out-index 2)))
    (nth out-index
         (cl-find-if (lambda (item)
                       (equal (nth in-index item)
                              priority))
                     priorities))))



(provide 'org-reminders-utils)
;;; org-reminders-utils.el ends here
