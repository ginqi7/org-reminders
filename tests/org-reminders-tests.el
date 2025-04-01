(defun read-file-lines (file)
  "Read lines from FILE and return them as a list."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (split-string (buffer-string) "\n" t))))

(defun load-dependencies-path ()
  "Add all dependencies to load-path."
  (let* ((file-name (file-name-concat
                     (or load-file-name
                         (buffer-file-name))
                     ".."
                     ".."
                     "dependencies.txt"))
         (urls (read-file-lines file-name))
         (names (mapcar (lambda (url) (car (last (split-string url "/")))) urls)))
    (dolist (name names)
      (print (format "Add `~/.emacs.d/lisp/%s` to load-path" name))
      (add-to-list 'load-path (format "~/.emacs.d/lisp/%s" name)))))

(load-dependencies-path)

(require 'org-reminders)
