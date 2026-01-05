
(use-package modus-themes
  :ensure nil
  :demand t
  :init (require-theme 'modus-themes)
  :custom-face
  (minibuffer-nonselected ((t (:inverse-video t))))
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)

  (modus-themes-headings
   '((1 . (ultrabold 1.5))
     (2 . (ultrabold 1.3))
     (agenda-date . (regular 1.1))
     (agenda-structure . (light 1.3))
     (t . (1.1))))

  (modus-vivendi-palette-overrides
   '(
     (bg-main        "#000000")
     (bg-dim         "#0a0f0a")
     (bg-active      "#111c17")
     (bg-inactive    "#1b2a24")

     (fg-main        "#ffffff")
     (fg-dim         "#b4aeae")
     (fg-heading-1   "#ab82ff")
     (fg-heading-2   "#fab387")

     (prose-tag      "#ffe")
     (mail-subject   "#6ae4b9")

     (cursor "#5fd7af") (bg-completion "#248f6c") (bg-hl-line "#142f2b") ;;seagreen

     (bg-region bg-completion)
     (fg-region unspecified)

     (bg-tab-bar     bg-main)
     (bg-tab-current bg-active)
     (bg-tab-other   bg-dim)

     (bg-mode-line-active     bg-dim)
     (bg-line-number-active   bg-main)
     (bg-line-number-inactive bg-main)

     (fg-line-number-active   fg-dim)
     (fg-line-number-inactive border)

     (border-mode-line-active   unspecified)
     (border-mode-line-inactive unspecified)
     (fringe unspecified)))
  :config
  (load-theme 'modus-vivendi t))

(defface meow-normal-face
  '((t (:foreground "#89b4fa" :weight bold)))  ;; blue
  "Face for Meow NORMAL mode.")

(defface meow-insert-face
  '((t (:foreground "#a6e3a1" :weight bold)))  ;; green
  "Face for Meow INSERT mode.")

(defface meow-convert-face
  '((t (:foreground "#fab387" :weight bold)))  ;; peach
  "Face for Meow CONVERT mode.")

(defface meow-visit-face
  '((t (:foreground "#cba6f7" :weight bold)))  ;; purple
  "Face for Meow VISIT mode.")

(defface meow-isearch-face
  '((t (:foreground "#f9e2af" :weight bold)))  ;; yellow
  "Face for Meow ISEARCH mode.")

(defface meow-motion-face
  '((t (:foreground "#f38ba8" :weight bold)))  ;; red
  "Face for Meow MOTION mode.")

(defun my/meow-state-face ()
  (pcase (meow--current-state)
    ('normal 'meow-normal-face)
    ('insert 'meow-insert-face)
    ('convert 'meow-convert-face)
    ('visit 'meow-visit-face)
    ('isearch 'meow-isearch-face)
    ('motion 'meow-motion-face)
    (_ 'mode-line)))   ;; fallback

(defun my/meow-indicator-colored ()
  (propertize (format "⦿%s" (meow-indicator))
              'face (my/meow-state-face)))

(defun my/line-progress-indicator ()
  "Return a block indicating buffer position proportionally."
  (let* ((total (max 1 (count-lines (point-min) (point-max))))
         (current (line-number-at-pos))
         (blocks ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]) ; 8 levels
         (index (floor (* 7 (/ (float current) total))))) ; scale 0–7
    (aref blocks index)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foundry "JB" :family "JetBrains Mono" :weight bold :height 180 :width normal))))
 '(isearch ((t (:foreground "pink" :background "black" :weight bold :underline t))))
 '(lazy-highlight ((t (:foreground "#67B7A4" :background "#0d0d0d"))))
 '(window-divider ((t (:foreground "#b1bf88")))))
