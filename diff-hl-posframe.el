;;; Code:



(require 'posframe)
(require 'diff-hl)

(defvar diff-hl-posframe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-posframe--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-posframe--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-posframe--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-posframe--click)
    map)
  "Keymap for diff-hl-posframe-mode.")

(defvar diff-hl-posframe-buffer-name "*diff-hl-posframe-hunk*" "Name of the posframe used by diff-hl-posframe.")
(defvar diff-hl-posframe-frame nil "The postframe frame used in diff-hl-posframe package.")

(defgroup diff-hl-posframe-group nil
  "Show vc diffs in a posframe."
  :group 'convenience)

(defcustom diff-hl-posframe-hunk-boundary "^@@.*@@"
  "Regex that marks the boundary of a hunk in *vc-diff* buffer."
  :type 'string)


(defcustom diff-hl-posframe-internal-border-width 2
  "Internal border width of the posframe.  The color can be customized with `internal-border` face."
  :type 'integer)


(defcustom diff-hl-posframe-internal-border-color "#00ffff"
  "Internal border color of the posframe.  If it doesn't work, try with `internal-border` face."
  :type 'color)

(defcustom diff-hl-posframe-narrow t
  "Narrow the differences to the current hunk."
  :type 'boolean)

(defcustom diff-hl-posframe-poshandler nil
  "Poshandler of the posframe (see `posframe-show`)."
  :type 'function)

(defcustom diff-hl-posframe-parameters nil
  "The frame parameters used by helm-posframe."
  :type 'string)

(defface diff-hl-posframe-clicked-line-face
  '((t (:inverse-video t)))
  "Face for the clicked line in the diff output.")

(defface diff-hl-posframe-face
  '((t (:height 0.8)))
  "Face for the posframe.")

(defun diff-hl-posframe--hide-handler  (_info)
  "Hide the posframe if the event is outside the posframe (after the posframe has been opened)."

  (if (not (frame-visible-p diff-hl-posframe-frame))
      t
    (let* ((invoking-command-p (or
                                (eq this-command 'diff-hl-posframe--click)
                                (eq this-command 'diff-hl-posframe-show)
                                (eq this-command 'handle-switch-frame)
                                ))
           (ignore-command-p (eq this-command 'ignore))
           (command-in-posframe-p (eq last-event-frame diff-hl-posframe-frame))
           (keep-open-p (or invoking-command-p command-in-posframe-p ignore-command-p)))
      (not keep-open-p))))


(defun diff-hl-posframe-buffer ()
  "Create the buffer with the contents of the hunk at point.
The buffer has the point in the corresponding line of the hunk.
Returns a list with the buffer and the line number of the clicked line."

  (let ((content)
        (point-in-buffer)
        (line)
        (overlay)
        (inhibit-redisplay t) ;;https://emacs.stackexchange.com/questions/35680/stop-emacs-from-updating-display
        (buffer (get-buffer-create diff-hl-posframe-buffer-name)))
    

    ;; Get differences
    (save-window-excursion
      (save-excursion
        (diff-hl-diff-goto-hunk)
        (with-current-buffer "*vc-diff*"
          (setq content (buffer-string))
          (setq point-in-buffer (point)))))

    (with-current-buffer buffer

      (erase-buffer)
      (insert content)
      
      ;; Highlight the clicked line
      (goto-char point-in-buffer)
      (setq overlay (make-overlay (point-at-bol) (min (point-max) (1+ (point-at-eol)))))
      (overlay-put overlay 'face 'diff-hl-posframe-clicked-line-face)
      
      ;; diff-mode, highlight hunks boundaries
      (diff-mode)
      (highlight-regexp diff-hl-posframe-hunk-boundary)
      

      ;; Change face size
      (buffer-face-set 'diff-hl-posframe-face)
      

      ;;  Find the hunk and narrow to it
      (when diff-hl-posframe-narrow
        (re-search-backward diff-hl-posframe-hunk-boundary nil 1)
        (forward-line 1)
        (let* ((start (point)))
          (re-search-forward diff-hl-posframe-hunk-boundary nil 1)
          (move-beginning-of-line nil)
          (narrow-to-region start (point)))
        ;; Come back to the clicked line
        (goto-char (overlay-start overlay)))
      

      (setq line (line-number-at-pos)))
    
    (list buffer line)))


(defun diff-hl-posframe--click (event)
  "Called when user clicks on margins.  EVENT is click information."
  (interactive "event")

  ;; Go to clicked spot
  (posn-set-point (event-start event))
  (diff-hl-posframe-show))


(defun diff-hl-posframe-show ()
  "Show a the diffs with vc last version in a posframe, if available.
If not, it fallbacks to `diff-hl-diff-goto-hunk`."
  (interactive)
  (cond ((not (vc-backend buffer-file-name))
         (message "The buffer is not under version control"))
        ((not (diff-hl-hunk-overlay-at (point)))
         (message "There is no modified hunk at pos %s" (point)))
        ((not (posframe-workable-p))
         (diff-hl-diff-goto-hunk))
        (t

         (let* ((buffer-and-line (diff-hl-posframe-buffer))
                (buffer (elt buffer-and-line 0))
                (line (elt buffer-and-line 1)))
        
           ;; Show posframe
           (setq posframe-mouse-banish nil)
           (setq
            diff-hl-posframe-frame
            (posframe-show buffer
                           :position (point)
                           :poshandler diff-hl-posframe-poshandler
                           :internal-border-width diff-hl-posframe-internal-border-width
                           :accept-focus  nil
                           :internal-border-color diff-hl-posframe-internal-border-color ; Doesn't always work, better define internal-border face
                           :hidehandler 'diff-hl-posframe--hide-handler
                           :override-parameters diff-hl-posframe-parameters
                           ))

           ;; Recenter arround point
           (with-selected-frame diff-hl-posframe-frame
             (with-current-buffer buffer
               (goto-char (point-min))
               (forward-line (1- line))
               (select-window (window-main-window diff-hl-posframe-frame))
               (recenter)))))))



;;;###autoload
(define-minor-mode diff-hl-posframe-mode
  "Enables the margin and fringe to show a posframe with vc diffs when clicked.
By default, the posframe shows only the current hunk, and the line of the hunk that matches the current position is highlighted.
The posframe face, border and other visual preferences are customizable.
The posframe can be also invoked with the command `diff-hl-posframe-show`"
  :group diff-hl-posframe-group

  (unless (and (featurep 'diff-hl) (featurep 'posframe) )
    (error "Required packages not available: diff-hl-posframe-mode needs diff-hl and posframe"))
  )

;;;###autoload
(define-globalized-minor-mode global-diff-hl-posframe-mode
  diff-hl-posframe-mode
  diff-hl-posframe-mode)

(provide 'diff-hl-posframe)
;;; diff-hl-posframe.el ends here
