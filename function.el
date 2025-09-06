;; move line around
;;;###autoload
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

;; create buffer
;;;###autoload
(defun create-buffer ()
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
  (switch-to-buffer (get-buffer-create bufname))
  (if (= n 1) initial-major-mode)))

(defvar my-password-file "~/.emacs.d/password"
  "File path to store the password.")

(defvar my-password nil
  "Variable to store the password.")

(defvar user-name "MagiFeeney")

;;;###autoload
(defun insert-password ()
  "Inserts the stored password at the current cursor position."
  (interactive)
  (if (not my-password)
      (progn
        (message "Password is null, please renew it.")
        (set-password t))
    (insert my-password)))

(global-set-key (kbd "C-c x p") #'insert-password)
(global-set-key (kbd "C-c x n") (lambda () (interactive) (insert user-name)))

;;;###autoload
(defun set-password (isRenew)
  "Interactively sets the value of the variable my-password.
If isRenew is non-nil, it indicates that my-password already has a value."
  (interactive "P")
  (if (not isRenew)
      (setq my-password (read-string "Enter password (overwrite): "))
    (setq my-password (read-string "Enter password (write): ")))
  (save-password))

(global-set-key (kbd "C-c x s") #'set-password)

;;;###autoload
(defun save-password ()
  "Saves the value of my-password to a file."
  (with-temp-file my-password-file
    (insert (or my-password ""))))

(defun load-password ()
  "Loads the value of my-password from the file."
  (when (file-exists-p my-password-file)
    (setq my-password (with-temp-buffer
                        (insert-file-contents my-password-file)
                        (buffer-string)))))

(load-password)

;; checkbox intermediate state
;;;###autoload
(defun toggle-checkbox ()
  "Toggles the state of the checkbox at the current cursor position."
  (interactive)insert-password
  (if (org-at-item-checkbox-p)
      (if (string= (match-string 0) "[-] ")
          (replace-match "[X]")
        (replace-match "[-] "))))

;;;###autoload
(defun consult-org-roam-ripgrep()
  (interactive)
  (unless (featurep 'org-roam)
    (require 'org-roam))
  (let ((consult-ripgrep "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

;;;###autoload
(defun delete-tramp-buffers ()
  (interactive)
  (buffer-menu)

  (if (yes-or-no-p "Delete ssh & tramp?")
      (let ((continue t))
	(while continue
          (if (search-forward "/ssh" nil t)
	      (Buffer-menu-delete)
            (setq continue nil))))
    (when (search-forward "*tramp" nil t)
      (Buffer-menu-delete)))
  
  (Buffer-menu-execute))

(global-set-key (kbd "C-c C-d") 'delete-tramp-buffers)

;;;###autoload
(defun my/org-insert-total-effort ()
  "Calculate total Effort in the buffer and insert or update #+TOTAL_EFFORT: after #+TITLE:."
  (interactive)
  (let ((total-minutes 0)
        (effort-line "")
        (case-fold-search t))  ;; Make search case-insensitive for #+TITLE:
    ;; Calculate total effort
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (let ((effort (org-element-property :EFFORT hl)))
          (when effort
            (setq total-minutes (+ total-minutes (org-duration-to-minutes effort)))))))

    (setq effort-line (format "#+TOTAL_EFFORT: %s\n" (org-duration-from-minutes total-minutes)))

    (save-excursion
      (goto-char (point-min))
      (let ((title-found (re-search-forward "^#\\+TITLE:.*$" nil t)))
        (if title-found
            ;; If #+TITLE: found, go to next line
            (forward-line)
          ;; If no title, go to top
          (goto-char (point-min)))
        ;; If there's already a #+TOTAL_EFFORT: on this line, replace it
        (if (looking-at "^#\\+TOTAL_EFFORT:.*$")
            (replace-match effort-line)
          ;; Otherwise, insert a new line
          (insert effort-line))))))

;;;###autoload
(defun print-in-SoC (filename)
  (interactive "fPrint file: ")
  (let ((cmd (format "smbclient //nts27.comp.nus.edu.sg/psf501 -A ~/.smbcredentials -c 'print \%s\'" (expand-file-name filename))))
    (shell-command cmd)))
