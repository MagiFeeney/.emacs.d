;; -*- lexical-binding: t; -*-

;;;##autoload
(defun my/get-garmin-deadline ()
  (let* ((tomorrow (time-add (current-time) (days-to-time 1)))
         (day-of-week (string-to-number (format-time-string "%u" tomorrow)))
         (is-weekend (>= day-of-week 6))
         (report-time (if is-weekend "10:00" "09:00")))
    (format-time-string (concat "<%Y-%m-%d %a " report-time ">") tomorrow)))

;;;##autoload
(defun my/push-to-garmin-morning-report ()
  (interactive)
  (org-copy-subtree)
  (save-window-excursion
    (with-current-buffer (find-file-noselect "/db:/garmin/morning_report.org")
      (goto-char (point-min))
      (when (re-search-forward "^\\*" nil t)
        (goto-char (match-beginning 0)))
      (org-paste-subtree)
      (org-deadline nil (substring (my/get-garmin-deadline) 1 -1))
      (save-buffer)))
  (message "Task safely copied to the top of Garmin Morning Report!"))

(global-set-key (kbd "C-c g p") 'my/push-to-garmin-morning-report)
