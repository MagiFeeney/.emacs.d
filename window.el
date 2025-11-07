(setopt display-buffer-alist
        ;; Message, Info, and Man buffers
        '(((lambda (buf &rest _)
             (with-current-buffer buf
               (memq major-mode
                     '(messages-buffer-mode
                       Info-mode
                       Man-mode))))
           (display-buffer-reuse-window
            display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . (messages-buffer-mode
                    Info-mode
                    Man-mode))
           (side . right)
           (window-width . 80))
          ;; Help buffers and Variable-value edit buffers
          ((lambda (buf &rest _)
             (with-current-buffer buf
               (memq major-mode
                     '(help-mode
                       help-fns--edit-value-mode))))
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . (help-mode help-fns--edit-value-mode))
           (side . right)
           (window-width . fit-window-to-buffer))
          ;; Custom theme-selection buffers
          ("\*Custom Themes\*"
           (display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . left)
           (window-width . 66))
          ;; Custom settings buffers
          ((lambda (buf &rest _)
             (with-current-buffer buf
               (eq major-mode 'Custom-mode)))
           (display-buffer-reuse-mode-window
            display-buffer-in-direction)
           (direction . right)
           (mode Custom-mode)
           (window-width . 80))
          ;; Dired, Ibuffer, & Buffer-menu
          ((lambda (buf &rest _)
             (with-current-buffer buf
               (memq major-mode '(dired-mode
                                  ibuffer-mode
                                  Buffer-menu-mode))))
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (side . left)
           (mode . (dired-mode ibuffer-mode Buffer-menu-mode))
           (window-width . 37)
           (window-parameters (no-delete-other-windows . t)))
          ;; Bookmark menu
          ("\*Bookmark List\*"
           (display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . left)
           (window-width . 51))
          ;; Pages-directory and Occur buffers
          ("\*Directory for: .*\\|\*Occur\*"
           (display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . right))
          ;; Scratch buffer
          ("\*scratch\*"
           (display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . right)
           (window-width . 80))
          (".*"
           (display-buffer-reuse-window
            display-buffer-use-some-window))))
 
;; `help-fns-edit-mode-done' kills its buffer before redisplaying the
;; `help-mode' buffer.  Thus, `display-buffer-reuse-mode-window'
;; doesn't work to get the *Help* window to reuse that window again.
;; This advice forces it to use the same window instead of creating a
;; new one.
(defun my/help-fns-edit-mode-done-advice (oldfunc &rest args)
  "Advice for the `help-fns-edit-mode-done' command."
  (let ((display-buffer-alist '((t display-buffer-same-window))))
    (apply oldfunc args)))
 
(advice-add 'help-fns-edit-mode-done :around
            'my/help-fns-edit-mode-done-advice)
 
;; If you visit another buffer in a `help-mode' window and then return
;; to the "*Help*" buffer, "q" won't delete the window.  This fixes that.
(with-eval-after-load 'help-mode
  (keymap-set help-mode-map "q" 'delete-window))
 
(defun my/dired-open-advice (oldfunc &rest args)
  "Advice for Dired's opening commands.
If opening a directory from Dired, be sure to use the existing Dired
window for that and not a new window.  Otherwise, use some other window."
  (let ((display-buffer-alist
         '(((lambda (buf &rest _) (with-current-buffer buf
                                    (eq major-mode 'dired-mode)))
            display-buffer-reuse-mode-window)
           (t display-buffer-use-some-window))))
    (apply oldfunc args)))
 
(advice-add 'dired-find-file-other-window :around 'my/dired-open-advice)
(advice-add 'dired-display-file :around 'my/dired-open-advice)
(advice-add 'dired-find-file :around 'my/dired-open-advice)
 
;; View-echo-area-messages advice
(defun my/select-messages-window (&rest _)
  "Select the window showing the `messages-buffer'.
This should be used as `:after' advice with `view-echo-area-messages'."
  (select-window (get-buffer-window (messages-buffer))))
 
(advice-add 'view-echo-area-messages :after 'my/select-messages-window)
 
;; Man advice
(defun my/select-man-window (man-buffer)
  "Select the window displaying `man-buffer'."
  (select-window (get-buffer-window man-buffer)))
 
(advice-add 'Man-notify-when-ready :after 'my/select-man-window)
