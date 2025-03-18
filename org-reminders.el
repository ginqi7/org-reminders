;;; org-reminders.el --- An Emacs plugin for interacting between macOS Reminders and org mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: org, reminders, macOS

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

;; This package provides integration between macOS Reminders and Org mode.

;;; Code:

(require 'org)
(require 'cl-seq)
(require 'json)
(require 'transient)

;; Structures
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

;; Custom Variables
(defcustom org-reminders-sync-file (expand-file-name "~/.emacs.d/Reminders.org")
  "The path of sync file.")
(defcustom org-reminders-cli-command (executable-find "org-reminders")
  "The path of org-reminders cli.")
(defcustom org-reminders-include-completed t
  "Show completed reminders?")
(defcustom org-reminders-sync-frequency 1
  "Synchronization frequency indicates how many times files are saved before synchronizing.")
(defcustom org-reminders-log-level "info"
  "info or debug")
(defcustom org-reminders-display-options "all"
  "all or or incomplete or complete")

;; Macros
(defmacro org-reminders-with-subtree (&rest body)
  "Run BODY in current subtree."
  `(progn
     (org-narrow-to-subtree)
     ,@body
     (widen)))

(defmacro org-reminders-with-sync-file (&rest body)
  "Run BODY in org-reminders-sync-file."
  `(with-current-buffer (find-file-noselect ,org-reminders-sync-file)
     (save-excursion
       (goto-char (point-min))
       ,@body
       (save-buffer))))


;; Internal Variables
(defvar org-reminders--cli-process nil
  "The org-reminders-cli-process.")

(defvar org-reminders--cli-process-buffer "*org-reminders-cli*"
  "The org-reminders-cli-process buffer.")

(defvar org-reminders--log-string nil
  "The waited to process the log string.")

(defvar org-reminders--sync-once-running nil)

(defvar org-reminders--priorities
  '(9 ?C
      5 ?B
      1 ?A
      0 32))

(defvar org-reminders-keymaps
  '("externalId" external-id
    "isCompleted" completed
    "lastModified" last-modified
    "dueDate" scheduled
    "list" org-list
    "completionDate" closed)
  "The keymaps for converting JSON keys to struct fields.")

(defun org-reminders--run-cil (type
                               &optional
                               process-buffer
                               process-sentinel
                               process-filter)
  "Run org-reminders cli."
  (let ((process (start-process (format "org-reminders-cli-%s" type)
                                process-buffer
                                org-reminders-cli-command
                                "sync"
                                org-reminders-sync-file
                                "-t"
                                type
                                "-l"
                                org-reminders-log-level
                                "-f"
                                (number-to-string org-reminders-sync-frequency)
                                "-d"
                                org-reminders-display-options)))
    (when process-sentinel (set-process-sentinel process process-sentinel))
    (when process-filter (set-process-filter process process-filter))
    process))

(defun org-reminders-handle-list (action data)
  "Handle Reminders List by ACTION and DATA."
  (let ((obj (org-reminders-parse-list-data data)))
    (funcall (intern (format "org-reminders--list-%s" (downcase action))) obj)))

(defun org-reminders-parse-item-data (data)
  "Handle Reminders Item by DATA."
  (let* ((obj (org-reminders-parse-data data #'make-org-reminders-item))
         (json (json-encode (eieio-oref obj 'org-list))))
    (eieio-oset obj 'org-list (org-reminders-parse-list-data json))
    obj))

(defun org-reminders--log-append (str)
  "Append STR to log."
  (setq org-reminders--log-string (concat org-reminders--log-string str)))

(defun org-reminders--log-pop ()
  "Pop macthed log"
  (when-let ((matched-log (org-reminders--parse-log org-reminders--log-string)))
    (setq org-reminders--log-string
          (substring org-reminders--log-string (car matched-log)))
    matched-log))

(defun org-reminders--filter (process output)
  "org-reminders-cli output filter."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output))
  ;; Since the logs may not be printed in complete lines, they need to be saved first and then parsed gradually.
  (org-reminders--log-append output)
  (while-let ((matched-log (org-reminders--log-pop)))
    (org-reminders-reaction (cdr matched-log))))

(defun org-reminders--item-update-detail (obj)
  "Update a Reminders item in the sync file with details from OBJ.

This function updates the properties and content of a Reminders item in the Org
mode sync file based on the data provided in the OBJ structure.

Parameters:
- OBJ: A structure or object containing the details of the Reminders item.

Steps:
1. Extract the following properties from OBJ:
   - HASH: A unique identifier for the item.
   - TITLE: The title or headline of the item.
   - NOTES: Additional notes or description of the item.
   - LAST-MODIFIED: Timestamp of the last modification.
   - EXTERNAL-ID: An external identifier for the item.
   - CLOSED: Timestamp when the item was closed.
   - SCHEDULED: Timestamp for the scheduled date.
   - COMPLETED: A boolean indicating whether the item is completed.
2. Update the corresponding properties in the Org mode entry.
3. Update the headline, planning info, and notes as needed.
4. Set the TODO state based on the completion status."
  (let ((hash (org-reminders-item-hash obj))
        (title (org-reminders-item-title obj))
        (notes (org-reminders-item-notes obj))
        (last-modified (org-reminders-item-last-modified obj))
        (external-id (org-reminders-item-external-id obj))
        (closed (org-reminders-item-closed obj))
        (scheduled (org-reminders-item-scheduled obj))
        (completed (org-reminders-item-completed obj))
        (priority (org-reminders-item-priority obj)))
    (when hash (org-set-property "HASH" hash))
    (when last-modified (org-set-property "LAST-MODIFIED" last-modified))
    (when external-id (org-set-property "EXTERNAL-ID" external-id))
    (when title (org-edit-headline title))
    (when closed (org-add-planning-info 'closed closed))
    (when scheduled (org-add-planning-info 'scheduled scheduled))
    (org-todo (if (equal :false completed) 'todo 'done))
    (when (not (= 0 priority)) (org-priority (plist-get org-reminders--priorities priority)))
    ;; Update the notes section if notes are provided
    (when notes
      (org-end-of-meta-data)
      (delete-region (point) (point-max))
      ;; if char-before is not '\n' insert it
      (insert (if (= 10 (char-before)) "" "\n") notes))))

(defun org-reminders--insert-item-str (obj)
  "Insert a new Reminders item into the current buffer based on the details in OBJ."
  (let((title (org-reminders-item-title obj)))
    (insert "\n** " title)
    (org-reminders--item-update-detail obj)))

(defun org-reminders--item-add (obj)
  "Add a new Reminders item to the appropriate list in the sync file."
  (let ((org-list (org-reminders-item-org-list obj)))
    (org-reminders-with-sync-file
     (when (org-reminders--list-locate-by-id org-list)
       (org-reminders-with-subtree
        (goto-char (point-max))
        (org-reminders--insert-item-str obj))))))

(defun org-reminders--item-locate-by-id (obj)
  "Locate a Reminders item in the current buffer by its external ID."
  (let ((id (org-reminders-item-external-id obj)))
    (goto-char (point-min))
    (search-forward-regexp (format ":EXTERNAL-ID:\s*%s" id) nil t)))

(defun org-reminders--item-delete (obj)
  "Delete a Reminders item from the sync file by its external ID."
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
  "Locate a Reminders item in the current buffer by its external title."
  (let ((title (org-reminders-item-title obj)))
    (goto-char (point-min))
    (org-reminders--locate-headline-by-name-and-level
     title 2)))

(defun org-reminders--item-update (obj)
  "Update an existing Reminders item in the sync file."
  (org-reminders-with-sync-file
   (when (or (org-reminders--item-locate-by-id obj)
             (org-reminders--item-locate-by-title obj))
     (org-reminders-with-subtree
      (org-reminders--item-update-detail obj)))))

(defun org-reminders--list-add (obj)
  "Add a new Reminders list to the sync file."
  (let ((id (org-reminders-list-id obj))
        (title (org-reminders-list-title obj)))
    (org-reminders-with-sync-file
     (goto-char (point-max))
     (insert (format "\n* %s [0/0]" title))
     (when id (org-set-property "LIST-ID" id)))))

(defun org-reminders--list-locate-by-id (obj)
  "Locate a Reminders List in the current buffer by its LIST ID."
  (let ((id (org-reminders-list-id obj)))
    (goto-char (point-min))
    (search-forward-regexp (format ":LIST-ID:\s*%s" id) nil t)))

(defun org-reminders--list-delete (obj)
  "Delete a Reminders list from the sync file by its list ID."
  (org-reminders-with-sync-file
   (when (org-reminders--list-locate-by-id obj)
     (org-reminders-with-subtree
      (delete-region (point-min) (point-max))))))

(defun org-reminders--list-locate-by-title (obj)
  "Locate a Reminders List in the current buffer by its LIST title."
  (let ((title (org-reminders-list-title obj)))
    (goto-char (point-min))
    (org-reminders--locate-headline-by-name-and-level
     title 1)))

(defun org-reminders--list-update (obj)
  "Update an existing Reminders List in the sync file."
  (let ((id (org-reminders-list-id obj))
        (title (org-reminders-list-title obj)))
    (org-reminders-with-sync-file
     (when (or (org-reminders--list-locate-by-id obj)
               (org-reminders--list-locate-by-title obj))
       (org-reminders-with-subtree
        (when title (org-edit-headline (format "%s [0/0]" title)))
        (when id (org-set-property "LIST-ID" id)))))))

(defun org-reminders--list-sync (obj)
  (unless org-reminders--sync-once-running
    (org-reminders--run-cil "once" org-reminders--cli-process-buffer #'org-reminders--once-sentinel #'org-reminders--filter)
    (setq org-reminders--sync-once-running t)))

(defun org-reminders--once-sentinel (process event)
  "Sentinel function for org-reminders process.
PROCESS is the process object.
EVENT is the event describing the process state change."
  (cond
   ((string-match-p "finished" event)
    (setq org-reminders--sync-once-running nil))
   ((string-match-p "exited" event)
    (setq org-reminders--sync-once-running nil))
   ((string-match-p "killed" event)
    (setq org-reminders--sync-once-running nil))))


(defun org-reminders--parse-log (log-string)
  "Parse a log entry from the log-string string and extract its components.

This function extracts structured data from a log entry string by matching a
specific pattern. The log entry is expected to contain multiple fields enclosed
in square brackets, including time, target, type, action, ID, and base64-encoded
data. The extracted data is returned as a structured object.

Parameters:
- LOG: A string containing the log entry to be parsed.

Steps:
1. Use `string-match` to match the log entry pattern, which consists of six
   fields enclosed in square brackets:
   - Time
   - Target
   - Type
   - Action
   - ID
   - Base64-encoded data
2. If the pattern matches, extract each field using `match-string`.
3. Decode the base64-encoded data using `base64-decode-string`.
4. Construct a structured object using `make-org-reminders-log` with the extracted
   fields.
5. Return a cons cell where the car is the end position of the match and the cdr
   is the structured log object.

Returns:
A cons cell where:
- The car is the position in the log-string string after the matched log entry.
- The cdr is a structured object containing the parsed log data.

This function is typically used to process log entries during synchronization or
debugging."
  (when (string-match (concat "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]"
                              "\\[\\(.*?\\)\\]\n")

                      log-string)
    (cons (match-end 0)
          (make-org-reminders-log
           :time (match-string 1 log-string)
           :target (match-string 2 log-string)
           :type (match-string 3 log-string)
           :action (match-string 4 log-string)
           :id (match-string 5 log-string)
           :data (base64-decode-string (match-string 6 log-string))))))

(defun org-reminders-parse-data (data type)
  "Parse JSON data and map it to a structured object of the specified type.

This function takes a JSON string and converts it into a structured object of the
specified TYPE. The JSON keys are mapped to object properties using a predefined
keymap (`org-reminders-keymaps`). If a key is not found in the keymap, it is used
directly as the property name.

Parameters:
- DATA: A JSON string containing the data to be parsed.
- TYPE: A function or constructor that creates an instance of the target object type.

Steps:
1. Parse the JSON string into a hash table using `json-parse-string`.
2. Extract the keys from the hash table using `hash-table-keys`.
3. Create an instance of the target object type using the provided TYPE function.
4. Iterate over the keys:
   - Look up the key in `org-reminders-keymaps` to determine the corresponding
     property name.
   - If the key is not found in the keymap, use the key directly as the property name.
   - Set the property value in the object using `eieio-oset`.
5. Return the populated object.

Returns:
An instance of the specified TYPE with properties set according to the parsed JSON data.

This function is typically used to convert JSON data from an external source into
a structured object for further processing."
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
  "Handle an item action by parsing the data and invoking the appropriate handler."
  (let ((obj (org-reminders-parse-item-data data)))
    (funcall (intern (format "org-reminders--item-%s" (downcase action))) obj)))

(defun org-reminders-reaction (log)
  "React to a log entry by performing the appropriate action based on its type and target.

This function processes a log entry by checking its target and type, and then
invoking the corresponding handler function for the specified action.

Parameters:
- LOG: A log entry object containing details about the action, type, and data.

Steps:
1. Check if the log target is 'Org Mode'. If not, exit early.
2. Extract the action, type, and data from the log entry.
3. Use `pcase` to match the type:
   - If the type is 'CommonList', call `org-reminders-handle-list` with the action and data.
   - If the type is 'CommonReminder', call `org-reminders-handle-item` with the action and data.

Returns:
The result of the invoked handler function, or nil if the target is not 'Org Mode'.

This function is typically used to process log entries and trigger appropriate
actions in the context of Org Mode."
  (when (string= "Org Mode" (org-reminders-log-target log))
    (let ((action (org-reminders-log-action log))
          (type (org-reminders-log-type log))
          (data (org-reminders-log-data log)))
      (pcase type
        ("CommonList" (org-reminders-handle-list action data))
        ("CommonReminder" (org-reminders-handle-item action data))))))

(defun org-reminders-parse-list-data (data)
  "Parse JSON data into a structured `org-reminders-list` object."
  (org-reminders-parse-data data #'make-org-reminders-list))

(defun org-reminders-start-auto-sync ()
  "Start auto-sync process."
  (interactive)
  (with-current-buffer (get-buffer-create org-reminders--cli-process-buffer)
    (erase-buffer))
  (if org-reminders--cli-process
      (message "org-reminders-auto-sync has already started.")
    (setq org-reminders--cli-process
          (org-reminders--run-cil "auto" org-reminders--cli-process-buffer nil #'org-reminders--filter))))

(defun org-reminders-stop-auto-sync ()
  "Stop auto-sync process."
  (interactive)
  (when (and org-reminders--cli-process
             (process-live-p org-reminders--cli-process))
    (kill-process org-reminders--cli-process))
  (setq org-reminders--cli-process nil)
  (setq org-reminders--log-string nil))

(defun org-reminders-restart-auto-sync ()
  "Restart auto-sync process."
  (interactive)
  (org-reminders-stop-auto-sync)
  (org-reminders-start-auto-sync))

(defun org-reminders-sync-all ()
  "Synchronize all reminders and lists with the external system."
  (interactive)
  (org-reminders--run-cil "all"))

(defun org-reminders-delete-item ()
  "Mark the current Reminders item as deleted by adding the 'DELETED' tag."
  (interactive)
  (org-reminders-with-subtree
   (goto-char (point-min))
   (org-set-tags "DELETED")))

(transient-define-prefix org-reminders-prefix ()
  "Prefix for Org Reminders."
  ["Org Reminders Commands"
   ("A" "Sync All" org-reminders-sync-all)
   ("a" "Auto Sync" org-reminders-start-auto-sync)
   ("s" "Stop Auto Sync" org-reminders-stop-auto-sync)
   ("r" "Restart Auto Sync" org-reminders-restart-auto-sync)])

(provide 'org-reminders)
;;; org-reminders.el ends here
