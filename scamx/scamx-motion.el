(defun meow-motion-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-motion-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-motion-exit))
   ((meow-motion-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-motion ()
  "Move to the start of selection, switch to MOTION state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'motion)))

(define-key meow-motion-state-keymap [escape] nil)

(defmacro scamx-motion-define-key (&rest configs)
  "A unified interface to define meow motion keys globally and per-mode.
Use the :keep keyword as the first item in a mode block to prevent
the macro from wiping the base keybindings (useful for e.g. dired/magit)."
  (let ((forms nil))
    (dolist (raw-config configs)
      (let ((config (if (eq (car-safe raw-config) 'quote) (cadr raw-config) raw-config)))
        (cond
         ;; Global binding
         ((stringp (car config))
          (push `(define-key meow-motion-state-keymap (kbd ,(car config)) ',(cdr config)) forms))

         ;; Mode-specific bindings
         ((symbolp (car config))
          (let* ((scope (car config))
                 (body (cdr config))
                 ;; Check :keep, strip out bindings if true
                 (keep-base (eq (car body) :keep))
                 (raw-bindings (if keep-base (cdr body) body))

                 (map-sym (intern (format "%s-map" scope)))
                 (hook-sym (intern (format "%s-hook" scope)))
                 (func-sym (intern (format "scamx-setup-motion-%s" scope))))

            (push `(defun ,func-sym ()
                     ;; Create blank slate only if :keep was not provided
                     ,@(when (not keep-base)
                         `((dolist (char (number-sequence ?\s ?~))
                             (define-key ,map-sym (vector char)
                               '(menu-item "" undefined
                                           :filter (lambda (c) (when (meow-motion-mode-p) c)))))))

                     ;; Apply overrides
                     ,@(mapcar (lambda (raw-b)
                                 (let ((b (if (eq (car-safe raw-b) 'quote) (cadr raw-b) raw-b)))
                                   `(define-key ,map-sym (kbd ,(car b))
                                      '(menu-item "" ,(cdr b)
                                                  :filter (lambda (c) (when (meow-motion-mode-p) c))))))
                               raw-bindings))
                  forms)
            (push `(add-hook ',hook-sym ',func-sym) forms)))

         (t (error "Invalid meow motion configuration: %s" config)))))

    `(progn ,@(nreverse forms))))

(defun scamx-motion-overwrite-define-key (keymap &rest bindings)
  "Bind keys in KEYMAP that only activate when `meow-motion-mode' is active.
BINDINGS is a list of (KEY . COMMAND) cons cells."
  (dolist (binding bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key keymap (kbd key)
        `(menu-item "" ,cmd
                    :filter ,(lambda (c)
                               ;; Only return the command if we are in motion state.
                               ;; Otherwise, return nil so Emacs ignores this binding.
                               (when (meow-motion-mode-p) c)))))))

;; ;; LaTeX Mode
;; (with-eval-after-load 'latex
;;   ;; Note: AUCTeX usually uses LaTeX-mode-map or TeX-mode-map
;;   (scamx-motion-overwrite-define-key LaTeX-mode-map
;;     '("g" . meow-motion-exit)
;;     '("h" . LaTeX-mark-environment)
;;     '("=" . LaTeX-mark-section)
;;     '("n" . LaTeX-find-matching-end)
;;     '("p" . LaTeX-find-matching-begin)
;;     '("e" . LaTeX-environment)
;;     '("s" . LaTeX-section)
;;     '("[" . reftex-citation)
;;     '("]" . LaTeX-close-environment)
;;     '("(" . reftex-label)
;;     '(")" . reftex-reference)
;;     '("f" . TeX-font)))

;; ;; Org Mode
;; (with-eval-after-load 'org
;;   (scamx-motion-overwrite-define-key org-mode-map
;;     '("g" . meow-motion-exit)
;;     '("h" . backward-delete-char)
;;     '("p" . org-previous-visible-heading)
;;     '("n" . org-next-visible-heading)))

;; ;; Vterm Mode
;; (with-eval-after-load 'vterm
;;   (scamx-motion-overwrite-define-key vterm-mode-map
;;     '("g" . meow-motion-exit)
;;     ;; Add any other vterm-specific motion keys here
;;     ))

;; (defun scamx-motion-latex-mode ()
;;   (meow-motion-overwrite-define-key
;;    '("g" . meow-motion-exit)
;;    '("h" . LaTeX-mark-environment)
;;    '("=" . LaTeX-mark-section)
;;    '("n" . LaTeX-find-matching-end)
;;    '("p" . LaTeX-find-matching-begin)
;;    '("e" . LaTeX-environment)
;;    '("s" . LaTeX-section)
;;    '("[" . reftex-citation)
;;    '("]" . LaTeX-close-environment)
;;    '("(" . reftex-label)
;;    '(")" . reftex-reference)
;;    '("f" . TeX-font)
;;    ))

;; (defun scamx-motion-org-mode ()
;;   (meow-motion-overwrite-define-key
;;    '("g" . meow-motion-exit)
;;    '("h" . backward-delete-char)
;;    '("p" . previous-line)
;;    ))

(provide 'scamx-motion)
