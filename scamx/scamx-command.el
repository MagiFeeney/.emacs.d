;;;###autoload
(defun scamx-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 40))

;;;###autoload
(defun scamx-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 40))

;;;###autoload
(defun scamx-scroll-down-command (&optional arg)
  (interactive "p")
  (scroll-down-command (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-up-command (&optional arg)
  (interactive "p")
  (scroll-up-command (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-other-window-down (&optional arg)
  (interactive "p")
  (scroll-other-window-down (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-other-window (&optional arg)
  (interactive "p")
  (scroll-other-window (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-kill-line (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg
          (kill-line (prefix-numeric-value arg))
        (kill-line)))))

;;;###autoload
(defun scamx-kill-sentence (&optional arg)
  "Kill sentence if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg
          (kill-sentence (prefix-numeric-value arg))
        (kill-sentence)))))

;;;###autoload
(defun scamx-kill-paragraph (arg)
  "Kill paragraph if no region is selected, otherwise kill the region."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg
          (kill-paragraph arg)
        (kill-paragraph)))))

;;;###autoload
(defun scamx-delete-char (arg &optional killp)
  "delete char if no region is selected, otherwise, delete region without storing to killring.

  With a prefix ARG, delete ARG characters. If KILLP is non-nil, also kill
the deleted text (similar to `kill-region`)."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (if killp
	      (delete-char arg killp)
	    (delete-char arg))
	(delete-char)))))

;;;###autoload
(defun scamx-backward-delete-char (arg &optional killp)
  "backward delete char if no region is selected, otherwise, delete region without storing to killring.

  With a prefix ARG, delete ARG characters. If KILLP is non-nil, also kill
the deleted text (similar to `kill-region`)."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (if killp
	      (backward-delete-char-untabify arg killp)
	    (backward-delete-char-untabify arg))
	(backward-delete-char-untabify)))))

;;;###autoload
(defun scamx-kill-word (arg)
  "kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (kill-word arg)
	(kill-word)))))

;;;###autoload
(defun scamx-backward-kill-word (arg)
  "backward kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (backward-kill-word arg)
	(backward-kill-word)))))

;;;###autoload
(defun scamx-forward-paragraph (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (if (minibufferp)
      (if arg
	  (next-history-element arg)
	(next-history-element 1))
    (forward-paragraph arg)))

;;;###autoload
(defun scamx-backward-paragraph (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (if (minibufferp)
      (if arg
	  (previous-history-element arg)
	(previous-history-element 1))
    (backward-paragraph arg)))

;;;###autoload
(defun read-ssh-connections-from-file (file)
  "Read SSH connections from FILE, returning them as a list."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun scamx-tramp-find-file ()
  "Prompt to choose an SSH connection from a list and connect to it."
  (interactive)
  (let* ((file "~/.emacs.d/ssh-connections")
         (connections (read-ssh-connections-from-file file))
         (chosen-connection nil))
    (setq chosen-connection (completing-read "Please choose SSH connection: " connections))
    (find-file chosen-connection)))

;;;###autoload
(defun scamx-suspend (&optional arg)
  (interactive "P")
  (when (meow-convert-mode-p)
    (meow--switch-state 'normal)
    (let ((key (read-key-sequence "Suspend to execute a command in Normal mode: ")))
      (if (not (equal (key-binding key) 'undefined))
	  (execute-kbd-macro key arg)
	(message "%s is undefined" key)))
    (meow--switch-state 'convert)))

;; ;;;###autoload
;; (defun scamx-suspend (&optional arg)
;;   (interactive "P")
;;   (when (meow-convert-mode-p)
;;     (meow--switch-state 'normal)
;;     (let ((key (read-key-sequence "Suspend to execute a command in Normal mode: ")))
;;       (if (not (equal (key-binding key) 'undefined))
;; 	  (execute-kbd-macro key arg)
;; 	(message "%s is undefined" key)))
;;     (meow--switch-state 'convert)))

;;;###autoload
(defun scamx-branching (cmd1 cmd2)
  (interactive)
  (if (use-region-p)
      (cmd2)
    (when (fboundp cmd1)
      (cmd1))))

;;;###autoload
(defun scamx-find-file ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file)))

;;;###autoload
(defun scamx-mark-inside-pairs (&optional arg)
  "Move up one list level, then mark the sexp inside."
  (interactive "p")
  (scamx-mark-outside-pairs arg)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  (exchange-point-and-mark))

;;;###autoload
(defun scamx-mark-outside-pairs (&optional arg)
  "Move up one list level, then mark the sexp outside."
  (interactive "p")
  (backward-up-list arg (point) (point))
  (mark-sexp))

(autoload 'hi-lock--regexps-at-point "hi-lock" nil t)

;;;###autoload
(defun scamx-space-command ()
  "Set mark on single space, highlight symbol on double space."
  (interactive)
  (set-mark-command nil)  ; Set mark first
  (let ((key (read-key "Mark set. Press SPC again to highlight symbol.")))
    (if (eq key ?\s)
        (progn
          (deactivate-mark) ; Cancel the mark if the next read is also SPC
	  (if (hi-lock--regexps-at-point)
	      (unhighlight-regexp t)
	    (progn
	      (unhighlight-regexp t) ; Remove all previous highlights
	      (message "Highlighting symbol ...")
	      (highlight-symbol-at-point)))
	  )
      ;; If another key was pressed, execute that key
      (setq unread-command-events (list key)))))

;;;###autoload
(defun scamx-insert-space (&optional arg)
  "Insert ARG number of spaces at point. Defaults to 1 if no prefix is provided."
  (interactive "p")
  (insert (make-string (or arg 1) ?\s)))

(provide 'scamx-command)
