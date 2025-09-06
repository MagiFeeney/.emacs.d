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
   (dired-mode . hl-line-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t))

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
(use-package org
  :ensure t
  :defer t
  :custom
  (org-agenda-files (concat org-directory "/agenda.org"))
  (org-default-notes-file (concat org-directory "/Capture"))
  (org-capture-templates
   '(("t" "TODO" entry (file+headline "~/Documents/Brain/Capture/notes.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree "~/Documents/Brain/Capture/journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")))
  :bind (:map scamx-org-map
	      (("a" . org-agenda)
	       ("s" . org-capture)
	       ("e" . #'my/org-insert-total-effort)))
  :config
  (setq org-edit-src-content-indentation 0)
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  ;; (add-hook 'org-mode-hook #'scamx-motion-org-mode)
  )

;; org-roam
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (concat org-directory "/Roam"))
  :bind (:map scamx-org-map
	      (("l" . org-roam-buffer-toggle)
               ("f" . org-roam-node-find)
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
	       ("." . denote-date)
	       ("r" . denote-rename-file)
	       ("l" . denote-link)
	       ("b" . denote-backlinks)
	       ("j" . denote-dired)
	       ("s" . denote-grep)))
  :config
  (setq denote-directory (concat org-directory "/Denote"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
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
  :config
  (pdf-tools-install :no-query))

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

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))
