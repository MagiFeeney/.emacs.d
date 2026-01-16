;;; scamx-multiple-cursors.el --- Advanced multiple cursor with movement-based placement -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Magi Feeney
;; Keywords: editing, multiple-cursors
;; Package-Requires: ((emacs "24.3") (multiple-cursors "1.4.0"))

;;; Commentary:
;;
;; This package provides a more flexible multiple-cursor workflow that allows
;; you to add cursors as you navigate, with smart pattern detection and
;; cursor management features.
;;
;; Key features:
;; - Add cursors while moving (@ key triggers mode)
;; - Smart pattern detection and suggestions
;; - Flexible cursor removal by line or region
;; - Seamless integration with multiple-cursors package
;;
;; Usage:
;; 1. Press @ to enter cursor-placement mode
;; 2. Move around and cursors are added at each position
;; 3. Press @ again to start editing with all cursors
;; 4. Use @ l to remove cursors by line, @ r to remove by region
;;

;;; Code:

(require 'multiple-cursors-core)

;;; Customization

(defgroup scamx-multiple-cursors nil
  "Advanced multiple cursor placement and management."
  :group 'editing
  :prefix "scamx-mc-")

(defcustom scamx-mc-prefix-key "C-c @"
  "Prefix key for smart cursor commands."
  :type 'string
  :group 'scamx-multiple-cursors)

(defcustom scamx-mc-suggestion-threshold 3
  "Number of movements before suggesting pattern repetition."
  :type 'integer
  :group 'scamx-multiple-cursors)

(defcustom scamx-mc-highlight-face 'highlight
  "Face used to highlight cursor positions in placement mode."
  :type 'face
  :group 'scamx-multiple-cursors)

;;; Internal variables

(defvar scamx-mc--placement-mode nil
  "Non-nil when in cursor placement mode.")

(defvar scamx-mc--cursor-positions nil
  "List of cursor positions added during placement mode.")

(defvar scamx-mc--movement-history nil
  "History of movements during placement mode.
Each entry is (command . args).")

(defvar scamx-mc--overlays nil
  "List of overlays marking cursor positions.")

(defvar scamx-mc--last-suggestion nil
  "Last suggested pattern for repetition.")

(defvar scamx-mc--activating nil
  "Guard to prevent recursive activation.")

;;; Overlay management

(defun scamx-mc--add-cursor-overlay (pos)
  "Add a visual overlay at POS to mark cursor position."
  (let ((ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'face scamx-mc-highlight-face)
    (overlay-put ov 'scamx-mc-cursor t)
    (push ov scamx-mc--overlays)
    ov))

(defun scamx-mc--remove-all-overlays ()
  "Remove all cursor position overlays."
  (mapc #'delete-overlay scamx-mc--overlays)
  (setq scamx-mc--overlays nil))

(defun scamx-mc--remove-overlays-in-region (start end)
  "Remove cursor overlays between START and END."
  (setq scamx-mc--overlays
        (cl-remove-if
         (lambda (ov)
           (let ((pos (overlay-start ov)))
             (when (and (>= pos start) (<= pos end))
               (delete-overlay ov)
               t)))
         scamx-mc--overlays))
  (setq scamx-mc--cursor-positions
        (cl-remove-if
         (lambda (pos)
           (and (>= pos start) (<= pos end)))
         scamx-mc--cursor-positions)))

(defun scamx-mc--remove-overlays-on-line (line-num)
  "Remove cursor overlays on LINE-NUM."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-num))
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (scamx-mc--remove-overlays-in-region start end))))

;;; Movement tracking

(defun scamx-mc--record-movement (command &rest args)
  "Record a movement COMMAND with ARGS."
  (push (cons command args) scamx-mc--movement-history))

(defun scamx-mc--detect-pattern ()
  "Detect repeating pattern in movement history.
Returns (command . args) if pattern detected, nil otherwise."
  (when (>= (length scamx-mc--movement-history) scamx-mc-suggestion-threshold)
    (let* ((recent (cl-subseq scamx-mc--movement-history 0
                             (min scamx-mc-suggestion-threshold
                                  (length scamx-mc--movement-history))))
           (first-move (car recent)))
      ;; Simple pattern: same command repeated
      (when (cl-every (lambda (move) (equal (car move) (car first-move))) recent)
        first-move))))

(defun scamx-mc--suggest-pattern ()
  "Suggest pattern repetition if detected."
  (let ((pattern (scamx-mc--detect-pattern)))
    (when (and pattern (not (equal pattern scamx-mc--last-suggestion)))
      (setq scamx-mc--last-suggestion pattern)
      (message "Pattern detected: %s. Press C-r to repeat (move+add cursor), or C-u N C-r for N times"
               (car pattern)))))

;;; Cursor placement

(defun scamx-mc--add-cursor-at-point ()
  "Add a cursor at the current point."
  (interactive)
  (let ((pos (point)))
    (unless (member pos scamx-mc--cursor-positions)
      (push pos scamx-mc--cursor-positions)
      (scamx-mc--add-cursor-overlay pos)
      (message "Cursor %d added at position %d"
               (length scamx-mc--cursor-positions) pos))))

(defun scamx-mc--placement-post-command ()
  "Post-command hook for cursor placement mode."
  (when scamx-mc--placement-mode
    (let ((cmd this-command))
      ;; Only record movement commands for pattern detection
      ;; Do NOT auto-add cursors
      (when (and (symbolp cmd)
                 (or (memq cmd '(forward-char backward-char
                                next-line previous-line
                                forward-word backward-word
                                forward-sexp backward-sexp
                                end-of-line beginning-of-line
                                forward-sentence backward-sentence
                                forward-paragraph backward-paragraph
                                scroll-up-command scroll-down-command
                                goto-line))
                     (string-match-p "forward\\|backward\\|next\\|previous\\|goto"
                                   (symbol-name cmd))))
        (scamx-mc--record-movement cmd)
        (scamx-mc--suggest-pattern)))))

;;; Pattern repetition

(defun scamx-mc-repeat-last-pattern (n)
  "Repeat the last detected movement pattern N times, adding cursor each time."
  (interactive "p")
  (if-let ((pattern scamx-mc--last-suggestion))
      (dotimes (_ n)
        (call-interactively (car pattern))
        (scamx-mc--add-cursor-at-point))
    (message "No pattern detected yet. Make 3+ identical movements first!")))

;;; Cursor removal

(defun scamx-mc-remove-cursor-at-point ()
  "Remove cursor at current point if it exists."
  (interactive)
  (let ((pos (point)))
    (when (member pos scamx-mc--cursor-positions)
      (setq scamx-mc--cursor-positions (delete pos scamx-mc--cursor-positions))
      (dolist (ov scamx-mc--overlays)
        (when (= (overlay-start ov) pos)
          (delete-overlay ov)
          (setq scamx-mc--overlays (delete ov scamx-mc--overlays))))
      (message "Cursor removed. %d cursors remaining."
               (length scamx-mc--cursor-positions)))))

(defun scamx-mc-remove-cursors-on-line ()
  "Remove all cursors on the current line."
  (interactive)
  (let ((line (line-number-at-pos)))
    (scamx-mc--remove-overlays-on-line line)
    (message "Removed cursors on line %d. %d cursors remaining."
             line (length scamx-mc--cursor-positions))))

(defun scamx-mc-remove-cursors-in-region (start end)
  "Remove all cursors between START and END."
  (interactive "r")
  (if (use-region-p)
      (progn
        (scamx-mc--remove-overlays-in-region start end)
        (message "Removed cursors in region. %d cursors remaining."
                 (length scamx-mc--cursor-positions)))
    (message "No active region. Select a region first.")))

(defun scamx-mc-clear-all-cursors ()
  "Remove all cursors and exit placement mode."
  (interactive)
  (scamx-mc--remove-all-overlays)
  (setq scamx-mc--cursor-positions nil
        scamx-mc--movement-history nil
        scamx-mc--last-suggestion nil)
  (when scamx-mc--placement-mode
    (scamx-mc-toggle-placement-mode))
  (message "All cursors cleared."))

;;; Mode activation

(defun scamx-mc--create-fake-cursors ()
  "Create multiple-cursors fake cursors at all marked positions."
  (when scamx-mc--cursor-positions
    ;; Sort and deduplicate positions
    (setq scamx-mc--cursor-positions (sort (delete-dups scamx-mc--cursor-positions) #'<))
    ;; Create fake cursors at all positions EXCEPT current point
    (let ((current-pos (point)))
      (mc/save-excursion
        (dolist (pos scamx-mc--cursor-positions)
          (unless (= pos current-pos)
            (goto-char pos)
            (mc/create-fake-cursor-at-point)))))))

(defun scamx-mc-activate-cursors ()
  "Activate all placed cursors and start editing."
  (interactive)
  (when (and (not scamx-mc--activating) scamx-mc--cursor-positions)
    (let ((scamx-mc--activating t))  ; Set guard
      ;; Exit placement mode
      (when scamx-mc--placement-mode
        (scamx-mc-placement-mode -1))
      ;; Remove overlays
      (scamx-mc--remove-all-overlays)
      ;; Create fake cursors
      (scamx-mc--create-fake-cursors)
      ;; Clear positions NOW to prevent reuse
      (setq scamx-mc--cursor-positions nil)
      ;; Activate mc mode
      (multiple-cursors-mode 1))))

;; Clean up when exiting multiple-cursors mode
(defun scamx-mc--cleanup-after-mc ()
  "Clean up state after multiple-cursors mode exits."
  (setq scamx-mc--cursor-positions nil
        scamx-mc--activating nil)
  (scamx-mc--remove-all-overlays))

(add-hook 'multiple-cursors-mode-disabled-hook #'scamx-mc--cleanup-after-mc)

;;; Placement mode

(defvar scamx-mc-placement-mode-map
  (let ((map (make-sparse-keymap)))
    ;; MAIN: Add cursor at current point
    (define-key map "." #'scamx-mc--add-cursor-at-point)
    ;; Activation
    (define-key map (kbd "RET") #'scamx-mc-activate-cursors)
    ;; Pattern repetition (move + add cursor)
    (define-key map (kbd "C-r") #'scamx-mc-repeat-last-pattern)
    ;; Cursor removal
    (define-key map (kbd "l") #'scamx-mc-remove-cursors-on-line)
    (define-key map (kbd "r") #'scamx-mc-remove-cursors-in-region)
    (define-key map (kbd "d") #'scamx-mc-remove-cursor-at-point)
    (define-key map (kbd "c") #'scamx-mc-clear-all-cursors)
    ;; Quick exit
    (define-key map (kbd "q") #'scamx-mc-toggle-placement-mode)
    map)
  "Keymap for smart cursor placement mode.")

(defvar scamx-mc-command-map
  (let ((map (make-sparse-keymap)))
    ;; Start/stop placement
    (define-key map (kbd "m") #'scamx-mc-start-placement)
    (define-key map (kbd "a") #'scamx-mc-activate-cursors)
    ;; Quick cursor addition without entering mode
    (define-key map (kbd ".") #'scamx-mc-add-cursor-here)
    ;; Integration with mc
    (define-key map (kbd ">") #'scamx-mc-mark-next-like-this)
    (define-key map (kbd "<") #'scamx-mc-mark-previous-like-this)
    (define-key map (kbd "*") #'scamx-mc-mark-all-like-this)
    map)
  "Keymap for smart cursor commands accessible via prefix key.")

(define-minor-mode scamx-mc-placement-mode
  "Minor mode for placing multiple cursors."
  :lighter " SMC"
  :keymap scamx-mc-placement-mode-map
  (if scamx-mc-placement-mode
      (progn
        (setq scamx-mc--placement-mode t)
        (add-hook 'post-command-hook #'scamx-mc--placement-post-command nil t)
        (message "Cursor placement mode active. Move around, press . to add cursor. RET to edit, q to exit."))
    (setq scamx-mc--placement-mode nil)
    (remove-hook 'post-command-hook #'scamx-mc--placement-post-command t)))

;;;###autoload
(defun scamx-mc-start-placement ()
  "Start smart cursor placement mode."
  (interactive)
  ;; ALWAYS reset state when starting, even if already in mode
  (setq scamx-mc--cursor-positions nil
        scamx-mc--movement-history nil
        scamx-mc--last-suggestion nil
        scamx-mc--activating nil)
  (scamx-mc--remove-all-overlays)
  (unless scamx-mc-placement-mode
    (scamx-mc-placement-mode 1))
  (message "Cursor placement started fresh. Move around, press . to add cursor."))

;;;###autoload
(defun scamx-mc-toggle-placement-mode ()
  "Toggle smart cursor placement mode."
  (interactive)
  (if scamx-mc-placement-mode
      (scamx-mc-placement-mode -1)
    (scamx-mc-start-placement)))

;;;###autoload
(defun scamx-mc-add-cursor-here ()
  "Add a cursor at point without entering placement mode."
  (interactive)
  (let ((pos (point)))
    (unless (member pos scamx-mc--cursor-positions)
      (push pos scamx-mc--cursor-positions)
      (scamx-mc--add-cursor-overlay pos)
      (message "Cursor added at position %d (%d total). Use %s a to activate."
               pos (length scamx-mc--cursor-positions) scamx-mc-prefix-key))))

;;; Integration with standard multiple-cursors

;;;###autoload
(defun scamx-mc-mark-all-like-this ()
  "Mark all occurrences like this using standard mc."
  (interactive)
  (when scamx-mc--placement-mode
    (scamx-mc-toggle-placement-mode))
  (call-interactively #'mc/mark-all-like-this))

;;;###autoload
(defun scamx-mc-mark-next-like-this (arg)
  "Mark next like this, adding to current cursor positions if in placement mode."
  (interactive "p")
  (if scamx-mc--placement-mode
      (dotimes (_ arg)
        (mc/mark-next-like-this 1)
        (scamx-mc--add-cursor-at-point))
    (mc/mark-next-like-this arg)))

;;;###autoload
(defun scamx-mc-mark-previous-like-this (arg)
  "Mark previous like this, adding to current cursor positions if in placement mode."
  (interactive "p")
  (if scamx-mc--placement-mode
      (dotimes (_ arg)
        (mc/mark-previous-like-this 1)
        (scamx-mc--add-cursor-at-point))
    (mc/mark-previous-like-this arg)))

;;; Setup

;;;###autoload
(defun scamx-multiple-cursors-setup ()
  "Setup smart multiple cursors with recommended bindings."
  (interactive)
  ;; Set up prefix key map
  (global-set-key (kbd scamx-mc-prefix-key) scamx-mc-command-map)
  ;; Also bind the placement mode map when in that mode
  (when scamx-mc-placement-mode
    (local-set-key (kbd scamx-mc-prefix-key) scamx-mc-placement-mode-map))
  (message "Smart multiple cursors setup complete.
%s m - Start placement mode
%s . - Add cursor at point
%s a - Activate cursors
%s > - Mark next like this
%s < - Mark previous like this
%s * - Mark all like this"
           scamx-mc-prefix-key scamx-mc-prefix-key scamx-mc-prefix-key
           scamx-mc-prefix-key scamx-mc-prefix-key scamx-mc-prefix-key))

;; Make sure placement mode map is active when the mode is on
(add-hook 'scamx-mc-placement-mode-hook
          (lambda ()
            (when scamx-mc-placement-mode
              (local-set-key (kbd scamx-mc-prefix-key) scamx-mc-placement-mode-map))))

(provide 'scamx-multiple-cursors)

;;; scamx-multiple-cursors.el ends here
