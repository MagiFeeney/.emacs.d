;; require packages from melpa
(use-package package
  :ensure t
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . auto-revert-mode)
   (dired-mode . hl-line-mode))
  :bind (:map dired-mode-map
              (";" . dired-up-directory)
              ("+" . dired-create-dir-or-file)
	      ("j" . dired-goto-dir-or-file)
	      ("v" . vterm)
	      ("/" . scamx-tramp-find-file)
	      ("z" . magit-status))
  :custom
  (dired-free-space nil)
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (load-file "~/.emacs.d/setup/dired/function.el") ; useful functions
  (put 'dired-find-alternate-file 'disabled nil))

;; mc
(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  (setq mc/always-run-for-all t))

;; define a nested org map for scamx
(defvar-keymap scamx-org-map
  :doc "access org mode via scamx")

;; define a nested org map for scamx
(defvar-keymap scamx-denote-map
  :doc "access denote mode via scamx")

;; global org directory
(setq org-directory (file-truename "~/Documents/Brain"))

;; org mode setup
(use-package org
  :ensure t
  :defer t
  :hook
  ((org-mode . (lambda () (setq truncate-lines nil)))
   (org-mode . variable-pitch-mode))
  :custom
  (org-agenda-files (concat org-directory "/agenda.org"))
  (org-default-notes-file (concat org-directory "/Capture"))
  (org-capture-templates
   '(("t" "TODO" entry (file+headline "~/Documents/Brain/Capture/notes.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree "~/Documents/Brain/Capture/journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("p" "Prioritized" entry (file+headline "~/Documents/Brain/Capture/priority.org" "Papers")
      "* TODO %?\n  %i\n  %a")))
  :bind (:map scamx-org-map
	      (("a" . org-agenda)
	       ("s" . org-capture)
	       ("l" . org-capture-goto-last-stored)
	       ("e" . #'my/org-insert-total-effort)))
  :config
  (setq org-edit-src-content-indentation 0
	org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
	org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	;; Org styling, hide markup etc.
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-tags-column 0
	org-ellipsis "…") ;newline when creating a TODO

  ;; (add-hook 'org-mode-hook #'scamx-motion-org-mode)
  )

;; org-roam
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (concat org-directory "/Roam"))
  :bind (:map scamx-org-map
	      (("f" . org-roam-node-find)
               ("i" . org-roam-node-insert)
               ("c" . org-roam-capture)
               ;; Dailies
               ("j" . org-roam-dailies-capture-today)
               ("," . org-roam-dailies-goto-yesterday)
	       ("." . org-roam-dailies-goto-today)
               ("/" . org-roam-dailies-goto-tomorrow)
               (";" . org-roam-dailies-goto-previous-note)
               ("'" . org-roam-dailies-goto-next-note)
               ("r" . consult-org-roam-ripgrep)))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

;; denote preview
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (:map scamx-denote-map
	      (("d" . denote)
	       ("f" . denote-open-or-create)
	       ("," . denote-dailies-goto-yesterday)
	       ("." . denote-dailies-goto-today)
	       ("/" . denote-dailies-goto-tomorrow)
	       ("r" . denote-rename-file)
	       ("l" . denote-link)
	       ("b" . denote-backlinks)
	       ("j" . denote-dired)
	       ("s" . denote-grep)))
  :config
  (setq denote-directory (concat org-directory "/Denote"))

  (load-file "~/.emacs.d/setup/denote/function.el") ; useful functions
  (denote-rename-buffer-mode 1))

;; LaTeX
(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-electric-math '("$" . "$"))
  (blink-matching-paren nil)
  (LaTeX-electric-left-right-brace t)
  (TeX-electric-sub-and-superscript t)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (reftex-plug-into-AUCTeX t)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (LaTeX-math-mode)
                  (pdf-tools-install)))
  :config
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)

  ;; (add-hook 'LaTeX-mode-hook #'scamx-motion-latex-mode)

  (load-file "~/.emacs.d/setup/latex/function.el") ; useful functions
  (load-file "~/.emacs.d/setup/latex/reference.el") ; reference management
  )

;; pdf tools
(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ((pdf-view-mode . delete-other-windows)
   (pdf-view-mode . pdf-view-roll-minor-mode))
  :bind (:map pdf-view-mode-map
	      ("[" . pdf-view-previous-page-command)
	      ("]" . pdf-view-next-page-command))
  :config
  (pdf-tools-install :no-query)
  ;; (add-hook 'pdf-view-after-change-page-hook #'pdf-view-fit-height-to-window)
  )

(use-package yasnippet
  :ensure t
  :hook
  ((python-mode . yas-minor-mode)
   (LaTeX-mode . yas-minor-mode)
   (org-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package vterm
  :ensure t
  :defer t
  :hook
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind
  (:map vterm-mode-map
        ("C-c q" . (lambda () (interactive)
                     (vterm-send-key "x" nil nil t)  ; send C-x
                     (vterm-send-key "q" nil nil t)))) ; send C-q
  :config
  (setq vterm-max-scrollback 100000
	vterm-timer-delay 0.01))

(use-package docker
  :ensure t
  :defer t
  :bind
  (("C-c d s" . (lambda () (interactive) (shell-command "systemctl --user start docker-desktop")))
   ("C-c d x" . (lambda () (interactive) (shell-command "systemctl --user stop docker-desktop")))))

(use-package magit
  :ensure t
  :defer t
  :bind (:map magit-mode-map
	      ("q" . magit-kill-this-buffer)))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package abbrev
  :ensure nil
  :custom
  (save-abbrevs nil)
  (abbrev-file-name "~/.emacs.d/snippets/abbrev.el")
  :config
  (defun emacs-solo/abbrev--replace-placeholders ()
	"Replace placeholders ###1###, ###2###, ### with minibuffer input.
If ###@### is found, remove it and place point there at the end."
	(let ((cursor-pos nil))
	  (save-excursion
		(goto-char (point-min))
		(let ((loop 0)
			  (values (make-hash-table :test 'equal)))
		  (while (re-search-forward "###\\([0-9]+\\|@\\)###" nil t)
			(setq loop (1+ loop))
			(let* ((index (match-string 1))
				   (start (match-beginning 0))
				   (end (match-end 0)))
			  (cond
			   ((string= index "@")
				(setq cursor-pos start)
				(delete-region start end))
			   (t
				(let* ((key (format "###%s###" index))
					   (val (or (gethash key values)
								(let ((input (read-string (format "Value for %s: " key))))
								  (puthash key input values)
								  input))))
				  (goto-char start)
				  (delete-region start end)
				  (insert val)
				  (goto-char (+ start (length val))))))))))
	  (when cursor-pos
		(goto-char cursor-pos)))))

(use-package dumb-jump
  :ensure t
  :defer t
  :hook
  ((prog-mode . dumb-jump-mode))
  :custom
  (dumb-jump-aggressive nil)
  :config
  (defun dumb-jump-check-searcher ()
    (when (and (not (file-remote-p default-directory))
               (executable-find "rg"))
      (setq-local dumb-jump-prefer-searcher 'rg
		  dumb-jump-force-searcher 'rg)))

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'dumb-jump-mode-hook #'dumb-jump-check-searcher))

;; tramp speedup suggested by
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(use-package tramp
  :ensure nil
  :init
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        tramp-copy-size-limit (* 1024 1024)
        tramp-default-method "rsync"
        remote-file-name-inhibit-auto-save-visited t)
  :custom
  ;; Direct async
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protcol "scp")
   'remote-direct-async-process)

  (connection-local-set-profiles
   '(:application tramp :protocol "rsync")
   'remote-direct-async-process)

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)

  (magit-tramp-pipe-stty-settings 'pty)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))))

;; Speedup tramp
(use-package tramp-hlo
  :ensure t
  :after tramp
  :config
  (tramp-hlo-setup))

;; Center screen
(use-package olivetti
  :ensure t
  :hook ((text-mode . olivetti-mode)
	 (prog-mode . olivetti-mode)
	 (dired-mode . olivetti-mode)
	 ;; (pdf-view-mode . olivetti-mode)
	 )
  :init
  (fringe-mode 0)
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-style 'variable))

;; Beautifying org mode
(use-package org-modern
  :ensure t
  :defer t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("●" "○" "•" "◦"))
  (org-modern-list '((?- . "❯")
                     (?+ . "➤")
                     (?* . "➥")))
  (org-modern-todo nil))

;; Better keybindings for indentation
(use-package indent
  :ensure nil
  :bind (:map indent-rigidly-map
	      ("b" . indent-rigidly-left)
	      ("f" . indent-rigidly-right)
	      ("a" . indent-rigidly-left-to-tab-stop)
	      ("e" . indent-rigidly-right-to-tab-stop)))

;; lsp
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("~/miniconda3/bin/pylsp"))))
