;;;###autoload
(defun dired-create-dir-or-file (path)
  "Use `dired-create-directory' or `dired-create-empty-file' based on PATH.
   If PATH has an extension, create an empty file.
   If it has no extension, create a directory.
   If PATH already exists, report with a message and stop."
  (interactive "FCreate (dir or file): ")
  (if (file-exists-p path)
      (message "Error: '%s' already exists." path)
    (if (file-name-extension path)
        (dired-create-empty-file path)
      (dired-create-directory path))))

;;;###autoload
(defun dired-goto-dir-or-file (path)
  "Open PATH in Dired.
   If PATH is a directory, open it.
   If PATH is a file, open its parent directory and move point to the file."
  (interactive "fGoto (dir or file): ")
  (let* ((expanded (expand-file-name path))
         (dir (if (file-directory-p expanded)
                  expanded
                (file-name-directory expanded))))
    (dired dir)
    (when (file-exists-p expanded)
      (dired-goto-file expanded))))
