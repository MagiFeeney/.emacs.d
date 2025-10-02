;;;###autoload
(defun denote-dailies-capture (n &optional goto)
  "Create (or visit) the Denote daily note N days from today.
N can be negative (yesterday = -1)."
  (interactive "p")
  (let* ((time (time-add (current-time) (days-to-time n)))
         (date-id (format-time-string "%Y%m%d" time))
         (pattern (concat "^" date-id "T[0-9]\\{6\\}.*__daily\\.org$"))
         (files (directory-files (denote-directory) t pattern))
         (file (car files)))
    (if file
        (find-file file)
      (let ((denote-use-date time)
            (denote-use-keywords '("daily")))
        (denote "" '("daily"))))))

;;;###autoload
(defun denote--prefix-to-number (arg default)
  "Turn ARG (raw prefix or a number) into a numeric value, else DEFAULT.
Handles programmatic calls where ARG may already be a number."
  (cond ((numberp arg) arg)
        (arg (prefix-numeric-value arg))
        (t default)))

;;;###autoload
(defun denote-dailies-goto-today (&optional arg)
  "Go to (or create) today's daily note.
With numeric prefix ARG go N days from today (default 0)."
  (interactive "P")
  (let ((n (denote--prefix-to-number arg 0)))
    (denote-dailies-capture n t)))

;;;###autoload
(defun denote-dailies-goto-tomorrow (&optional arg)
  "Go to (or create) tomorrow's daily note.
With numeric prefix ARG go N days from today (default 1)."
  (interactive "P")
  (let ((n (denote--prefix-to-number arg 1)))
    (denote-dailies-capture n t)))

;;;###autoload
(defun denote-dailies-goto-yesterday (&optional arg)
  "Go to (or create) yesterday's daily note.
With numeric prefix ARG go N days in the past (default -1)."
  (interactive "P")
  (let ((n (denote--prefix-to-number arg -1)))
    (denote-dailies-capture n t)))
