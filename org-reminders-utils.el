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

(defmacro org-reminders--in-obj (obj &rest body)
  (let* ((type 'org-reminders-model)
         (slot-info (cl-struct-slot-info type))
         (fields (mapcar #'car (cdr slot-info))))
    `(let ,(mapcar
            (lambda (field)
              `(,field
                (,(intern (format "%s-%s" type field)) ,obj)))
            fields)
       ,@body)))

(provide 'org-reminders-utils)
;;; org-reminders-utils.el ends here
