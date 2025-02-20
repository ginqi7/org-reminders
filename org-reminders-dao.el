;;; org-reminders-dao.el --- Data Access for Org Reminders.  -*- lexical-binding: t; -*-

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

(require 'org-reminders-cli-dao)
(require 'org-reminders-org-dao)

(defun org-reminders-dao-list-names (type)
  (pcase type
    ('org org-reminders-org-dao--list-names)
    ('cli org-reminders-cli-dao--list-names)))

(defun org-reminders-dao-items (type)
  (pcase type
    ('org org-reminders-org-dao--items)
    ('cli org-reminders-cli-dao--items)))

(defun org-reminders-dao-add-item (type model)
  (pcase type
    ('org (org-reminders-org-dao-add-item model))
    ('cli (org-reminders-cli-dao-add-item model))))

(defun org-reminders-dao-add-list (type list-name)
  (pcase type
    ('org (org-reminders-org-dao-add-list list-name))
    ('cli (org-reminders-cli-dao-add-list list-name))))

(defun org-reminders-dao-all-elements (type)
  (pcase type
    ('org (org-reminders-org-dao-all-elements))
    ('cli (org-reminders-cli-dao-all-elements))))

(defun org-reminders-dao-delete (type model)
  (pcase type
    ('org (org-reminders-org-dao-delete model))
    ('cli (org-reminders-cli-dao-delete model))))

(defun org-reminders-dao-edit (type model)
  (pcase type
    ('org (org-reminders-org-dao-edit model))
    ('cli (org-reminders-cli-dao-edit model))))

(defun org-reminders-dao-toggle-completed (type model)
  (pcase type
    ('org (org-reminders-org-dao-edit model))
    ('cli (org-reminders-cli-dao-toggle-completed model))))

(provide 'org-reminders-dao)
;;; org-reminders-dao.el ends here
