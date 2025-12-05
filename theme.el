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
