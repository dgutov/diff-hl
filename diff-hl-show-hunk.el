;;; diff-hl-show-hunk.el --- Integrate popup/posframe and diff-hl-diff-goto-hunk -*- lexical-binding: t -*-


;;; Commentary:

;; `diff-hl-show-hunk' shows a posframe/popup with the modified hunk at point.
;; `diff-hl-show-hunk-function' contains the backend used to show the hunk.  Its
;; default value is `diff-hl-show-hunk-function-default', which tries to use
;; `diff-hl-show-hunk-posframe' (GUI), and then `diff-hl-show-hunk-popup'.
;; Other backends (for example pos-tip or phantom overlays) could be
;; implemented.
;;
;; `diff-hl-show-hunk-mode' shows the posframe/popup when clicking
;; in the margin or the fringe.
;;
;; To use it in all buffers:
;;
;;    ```
;;      (global-diff-hl-show-hunk-mode)
;;    ```
;;

;;; Code:


(require 'diff-hl)
(require 'diff-hl-show-hunk-posframe)
(require 'diff-hl-show-hunk-popup)



(defvar diff-hl-show-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "C-x v *") #'diff-hl-show-hunk)
    (define-key map (kbd "C-x v {") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "C-x v }") #'diff-hl-show-hunk-next)
    map)
  "Keymap for command `diff-hl-show-hunk-mode'.")

(defvar diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*" "Name of the posframe used by diff-hl-show-hunk.")
(defvar diff-hl-show-hunk--original-window nil "The vc window of which the hunk is shown.")
(defvar diff-hl-show-hunk--original-buffer nil "The vc buffer of which the hunk is shown.")

(defgroup diff-hl-show-hunk-group nil
  "Show vc diffs in a posframe or popup."
  :group 'convenience)

(defcustom diff-hl-show-hunk-boundary "^@@.*@@"
  "Regex that marks the boundary of a hunk in *vc-diff* buffer."
  :type 'string)

(defcustom diff-hl-show-hunk-narrow t
  "Narrow the differences to the current hunk.")

(defcustom diff-hl-show-hunk-posframe-show-head-line t
  "Show some useful buttons at the top of the diff-hl posframe."
  :type 'boolean)


(defcustom diff-hl-show-hunk-function 'diff-hl-show-hunk-function-default
  "The function used to reder the hunk.
The function receives as first parameter a buffer with the
contents of the hunk, and as second parameter the line number
corresponding to the clicked line in the original buffer.  The
function should return t if the hunk is show, or nil if not.
There are some built in funcions:
`diff-hl-show-hunk-function-default', `diff-hl-show-hunk-popup'
and `diff-hl-show-hunk-posframe'"
  :type 'function)

(defvar diff-hl-show-hunk--hide-function nil "Function to call to close the shown hunk.")

(defun diff-hl-show-hunk-hide ()
  "Hide the current shown hunk, using the function in `diff-hl-show-hunk--hide-function'."
  (interactive)
  (if diff-hl-show-hunk--original-window
      (select-window diff-hl-show-hunk--original-window))
  (setq diff-hl-show-hunk--original-window nil)
  (if diff-hl-show-hunk--original-buffer
      (switch-to-buffer diff-hl-show-hunk--original-buffer))
  (setq diff-hl-show-hunk--original-buffer nil)
  (with-current-buffer diff-hl-show-hunk-buffer-name
    (read-only-mode -1)
    (erase-buffer))
  (when diff-hl-show-hunk--hide-function
    (funcall diff-hl-show-hunk--hide-function)))

(defface diff-hl-show-hunk-clicked-line-face
  '((t (:inverse-video t)))
  "Face for the clicked line in the diff output.")

(defface diff-hl-show-hunk-face
  '((t (:height 0.8)))
  "Face for the posframe.")

(defun diff-hl-show-hunk-ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing a posframe or a popup."
  (member command '(ignore diff-hl-show-hunk handle-switch-frame diff-hl-show-hunk--click)))


(defun diff-hl-show-hunk-buffer ()
  "Create the buffer with the contents of the hunk at point.
The buffer has the point in the corresponding line of the hunk.
Returns a list with the buffer and the line number of the clicked line."

  (let ((content)
        (point-in-buffer)
        (line)
        (line-overlay)
        (inhibit-redisplay t) ;;https://emacs.stackexchange.com/questions/35680/stop-emacs-from-updating-display
        (buffer (get-buffer-create diff-hl-show-hunk-buffer-name)))
    

    ;; Get differences
    (save-window-excursion
      (save-excursion
        (diff-hl-diff-goto-hunk)
        (with-current-buffer "*vc-diff*"
          (setq content (buffer-substring-no-properties (point-min) (point-max)))
          (setq point-in-buffer (point)))))

    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert content)
      
      ;; Highlight the clicked line
      (goto-char point-in-buffer)
      (setq line-overlay (make-overlay (point-at-bol) (min (point-max) (1+ (point-at-eol)))))
      (overlay-put line-overlay 'face 'diff-hl-show-hunk-clicked-line-face)
      
      ;; diff-mode, highlight hunks boundaries
      (diff-mode)
      (highlight-regexp diff-hl-show-hunk-boundary)
      (read-only-mode 1)

      

      ;; Change face size
      (buffer-face-set 'diff-hl-show-hunk-face)

      ;;  Find the hunk and narrow to it
      (when diff-hl-show-hunk-narrow
        (re-search-backward diff-hl-show-hunk-boundary nil 1)
        (forward-line 1)
        (let* ((start (point)))
          (re-search-forward diff-hl-show-hunk-boundary nil 1)
          (move-beginning-of-line nil)
          (narrow-to-region start (point)))
        ;; Come back to the clicked line
        (goto-char (overlay-start line-overlay)))
      

      (setq line (line-number-at-pos)))
    
    (list buffer line)))




(defun diff-hl-show-hunk--click (event)
  "Called when user clicks on margins.  EVENT is click information."
  (interactive "event")

  ;; Go to clicked spot
  (posn-set-point (event-start event))
  (diff-hl-show-hunk))

(defun diff-hl-show-hunk--previousp (buffer)
  "Decide if the is a previous hunk/change in BUFFER."
  (ignore-errors
    (with-current-buffer buffer
      (save-excursion
        (diff-hl-previous-hunk)))))

(defun diff-hl-show-hunk--nextp (buffer)
  "Decide if the is a next hunk/change in BUFFER."
  (ignore-errors
    (with-current-buffer buffer
      (save-excursion
        (diff-hl-next-hunk)))))

(defun diff-hl-show-hunk-previous ()
  "Go to previous hunk/change and show it."
  (interactive)
  (if (not (diff-hl-show-hunk--previousp (or diff-hl-show-hunk--original-buffer (current-buffer))))
      (progn
        (message "There is no previous change")
        (if (display-graphic-p)
            (tooltip-show "There is no previous change")))
    (progn
      (diff-hl-show-hunk-hide)
      (diff-hl-previous-hunk)
      (run-with-timer 0 nil #'diff-hl-show-hunk))))

(defun diff-hl-show-hunk-next ()
  "Go to next hunk/change and show it."
  (interactive)
  (if (not (diff-hl-show-hunk--nextp (or diff-hl-show-hunk--original-buffer (current-buffer))))
      (progn
        (message "There is no next change")
        (if (display-graphic-p)
            (tooltip-show "There is no next change")))
    (progn
      (diff-hl-show-hunk-hide)
      (diff-hl-next-hunk)
      (run-with-timer 0 nil #'diff-hl-show-hunk))))



(defun diff-hl-show-hunk-function-default (buffer line)
  "Show a posframe or a popup with the hunk in BUFFER, at  LINE."
  (let* ((posframe-used (when (featurep 'posframe)
                          (require 'posframe)
                          (when (posframe-workable-p)
                            (diff-hl-show-hunk-posframe buffer line))))
         (popup-used (when (not posframe-used)
                       (when (featurep 'popup)
                         (diff-hl-show-hunk-popup buffer line))))
         (success (or posframe-used popup-used)))
    (when (not success)
      (warn "diff-hl-show-hunk: Please install posframe or popup, or customize diff-hl-show-hunk-function"))
    success))

;;;###autoload
(defun diff-hl-show-hunk ()
  "Show a the diffs with vc last version in a posframe, if available.
If not, it fallbacks to `diff-hl-diff-goto-hunk`."
  (interactive)
  (cond ((not (vc-backend buffer-file-name))
         (user-error "The buffer is not under version control"))
        ((not (diff-hl-hunk-overlay-at (point)))
         (user-error "There is no modified hunk at pos %s" (point)))
        ((not diff-hl-show-hunk-function)
         (diff-hl-diff-goto-hunk))
        ((not (let ((buffer-and-line (diff-hl-show-hunk-buffer)))
                (setq diff-hl-show-hunk--original-buffer (current-buffer))
                (setq diff-hl-show-hunk--original-window (selected-window))
                (apply diff-hl-show-hunk-function buffer-and-line)))
         (diff-hl-diff-goto-hunk))))



;;;###autoload
(define-minor-mode diff-hl-show-hunk-mode
  "Enables the margin and fringe to show a posframe with vc diffs when clicked.
By default, the posframe shows only the current hunk, and the line of the hunk that matches the current position is highlighted.
The posframe face, border and other visual preferences are customizable.
The posframe can be also invoked with the command `diff-hl-show-hunk`"
  :group 'diff-hl-show-hunk-group)

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
