;;; diff-hl-show-hunk.el --- Integrate popup/posframe and diff-hl-diff-goto-hunk -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author: Álvaro González <alvarogonzalezsotillo@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `diff-hl-show-hunk' shows a popup with the modification hunk at point.
;; `diff-hl-show-hunk-function' points to the backend used to show the
;; hunk.  Its default value is `diff-hl-show-hunk-inline-popup', that
;; shows diffs inline using overlay.  There is another built-in backend:
;; `diff-hl-show-hunk-posframe' (based on posframe).
;;
;; `diff-hl-show-hunk-mouse-mode' adds interaction on clicking in the
;; margin or the fringe (shows the current hunk as well).
;;
;; To use it in all buffers:
;;
;;   (global-diff-hl-show-hunk-mouse-mode)

;;; Code:

(require 'diff-hl-inline-popup)
(require 'diff-hl)

(defvar diff-hl-show-hunk-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    map)
  "Keymap for command `diff-hl-show-hunk-mouse-mode'.")

(defvar diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*"
  "Name of the buffer used by diff-hl-show-hunk.")

(defvar diff-hl-show-hunk-diff-buffer-name "*diff-hl-show-hunk-diff-buffer*"
  "Name of the buffer used by diff-hl-show-hunk to show the diff.")

(defvar diff-hl-show-hunk--original-window nil
  "The vc window of which the hunk is shown.")

(defvar diff-hl-show-hunk--original-buffer nil
  "The vc buffer of which the hunk is shown.")

(defvar diff-hl-show-hunk--original-content nil
  "The original content of the hunk.")

(defvar diff-hl-show-hunk--original-overlay nil
  "Copy of the diff-hl hunk overlay.")

(defgroup diff-hl-show-hunk nil
  "Show vc diffs in a posframe or popup."
  :group 'diff-hl)

(defconst diff-hl-show-hunk-boundary "^@@.*@@")
(defconst diff-hl-show-hunk--no-lines-removed-message (list "<<no lines removed>>"))

(defcustom diff-hl-show-hunk-inline-popup-hide-hunk nil
  "If t, inline-popup is shown over the hunk, hiding it."
  :type 'boolean)

(defcustom diff-hl-show-hunk-inline-popup-smart-lines t
  "If t, inline-popup tries to show only the deleted lines of the
hunk.  The added lines are shown when scrolling the popup.  If
the hunk consist only on added lines, then
`diff-hl-show-hunk--no-lines-removed-message' it is shown."
  :type 'boolean)

(defcustom diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
  "The function used to render the hunk.
The function receives as first parameter a buffer with the
contents of the hunk, and as second parameter the line number
corresponding to the clicked line in the original buffer."
  :type '(choice
          (const :tag "Show inline" diff-hl-show-hunk-inline-popup)
          (const :tag "Show using posframe" diff-hl-show-hunk-posframe)))

(defvar diff-hl-show-hunk--hide-function nil
  "Function to call to close the shown hunk.")

(defun diff-hl-show-hunk-hide ()
  "Hide the current shown hunk."
  (interactive)
  (if (and diff-hl-show-hunk--original-window (window-live-p diff-hl-show-hunk--original-window))
      (select-window diff-hl-show-hunk--original-window))
  (setq diff-hl-show-hunk--original-window nil)
  (if (buffer-live-p diff-hl-show-hunk--original-buffer)
      (switch-to-buffer diff-hl-show-hunk--original-buffer))
  (setq diff-hl-show-hunk--original-buffer nil)
  (with-current-buffer (get-buffer-create diff-hl-show-hunk-buffer-name)
    (read-only-mode -1)
    (erase-buffer))
  (bury-buffer diff-hl-show-hunk-buffer-name)
  (when (get-buffer diff-hl-show-hunk-diff-buffer-name)
    (bury-buffer diff-hl-show-hunk-diff-buffer-name))
  (when diff-hl-show-hunk--hide-function
    (let ((hidefunc diff-hl-show-hunk--hide-function))
      (setq diff-hl-show-hunk--hide-function nil)
      (funcall hidefunc)))
  (when diff-hl-show-hunk--original-overlay
    (diff-hl-show-hunk--goto-hunk-overlay diff-hl-show-hunk--original-overlay))
  (when diff-hl-show-hunk--original-overlay
    (delete-overlay diff-hl-show-hunk--original-overlay))
  (setq diff-hl-show-hunk--original-overlay nil))

(defun diff-hl-show-hunk-ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing the current hunk."
  (member command '(ignore diff-hl-show-hunk handle-switch-frame diff-hl-show-hunk--click)))

(defun diff-hl-show-hunk--compute-diffs ()
  "Compute diffs using functions of diff-hl.
Then put the differences inside a special buffer and set the
point in that buffer to the corresponding line of the original
buffer."
  (defvar vc-sentinel-movepoint)
  (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
         (line (line-number-at-pos))
         (dest-buffer diff-hl-show-hunk-diff-buffer-name))
    (with-current-buffer buffer
      (diff-hl-diff-buffer-with-reference (buffer-file-name buffer) dest-buffer)
      (switch-to-buffer dest-buffer)
      (diff-hl-diff-skip-to line)
      (setq vc-sentinel-movepoint (point)))
    dest-buffer))

(defun diff-hl-show-hunk--get-original-lines (content)
  "Extracts the lines starting with '-' from CONTENT and save them."
  (let* ((lines (split-string content "[\n\r]+" )))
    (cl-remove-if-not (lambda (l) (string-match-p "^-.*" l)) lines)))

(defun diff-hl-show-hunk--fill-original-content (content)
  "Extracts the lines starting with '-' from CONTENT and save them."
  (let* ((original-lines (diff-hl-show-hunk--get-original-lines content))
         (original-lines (mapcar (lambda (l) (substring l 1)) original-lines))
         (content (string-join original-lines "\n")))
    (setq diff-hl-show-hunk--original-content content)))

(defun diff-hl-show-hunk-buffer ()
  "Create the buffer with the contents of the hunk at point.
The buffer has the point in the corresponding line of the hunk.
Returns a list with the buffer and the line number of the clicked line."
  (let ((content)
        (point-in-buffer)
        (line)
        (line-overlay)
        ;; https://emacs.stackexchange.com/questions/35680/stop-emacs-from-updating-display
        (inhibit-redisplay t)
        (buffer (get-buffer-create diff-hl-show-hunk-buffer-name)))

    ;; Get differences
    (save-window-excursion
      (save-excursion
        (with-current-buffer (diff-hl-show-hunk--compute-diffs)
          (setq content (buffer-substring-no-properties (point-min) (point-max)))
          (setq point-in-buffer (point)))))

    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert content)

      ;; Highlight the clicked line
      (goto-char point-in-buffer)
      (setq line-overlay (make-overlay (line-beginning-position)
                                       (min (point-max)
                                            (1+ (line-end-position)))))

      ;; diff-mode
      (diff-mode)
      (read-only-mode 1)

      ;; Find the hunk and narrow to it
      (re-search-backward diff-hl-show-hunk-boundary nil 1)
      (forward-line 1)
      (let* ((start (point)))
        (re-search-forward diff-hl-show-hunk-boundary nil 1)
        (move-beginning-of-line nil)
        (narrow-to-region start (point)))

      ;; Store original content
      (let ((content (buffer-string)))
        (diff-hl-show-hunk--fill-original-content content))

      ;; Come back to the clicked line
      (goto-char (overlay-start line-overlay))
      (setq line (line-number-at-pos)))

    (list buffer line)))

(defun diff-hl-show-hunk--click (event)
  "Called when user clicks on margins.  EVENT is click information."
  (interactive "e")
  ;; Go the click's position.
  (posn-set-point (event-start event))
  (diff-hl-show-hunk))

(defvar diff-hl-show-hunk-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n") #'diff-hl-show-hunk-next)
    (define-key map (kbd "c") #'diff-hl-show-hunk-copy-original-text)
    (define-key map (kbd "r") #'diff-hl-show-hunk-revert-hunk)
    (define-key map (kbd "[") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "]") #'diff-hl-show-hunk-next)
    (define-key map (kbd "{") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "}") #'diff-hl-show-hunk-next)
    (define-key map (kbd "S") #'diff-hl-show-hunk-stage-hunk)
    map))

(defvar diff-hl-show-hunk--hide-function)

;;;###autoload
(defun diff-hl-show-hunk-inline-popup (buffer &optional _ignored-line)
  "Implementation to show the hunk in a inline popup.
BUFFER is a buffer with the hunk."
  (diff-hl-inline-popup-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-inline-popup-hide)
  (let* ((lines (split-string (with-current-buffer buffer (buffer-string)) "[\n\r]+" ))
         (smart-lines diff-hl-show-hunk-inline-popup-smart-lines)
         (original-lines-number (cl-count-if (lambda (s) (string-prefix-p "-" s)) lines))
         (lines (if (string= (car (last lines)) "" ) (butlast lines) lines))
         (lines (if (and (eq original-lines-number 0) smart-lines)
                    diff-hl-show-hunk--no-lines-removed-message
                  lines))
         (overlay diff-hl-show-hunk--original-overlay)
         (type (overlay-get overlay 'diff-hl-hunk-type))
         (point (if (eq type 'delete) (overlay-start overlay) (overlay-end overlay)))
         (propertize-line (lambda (l)
                            (propertize l 'face
                                        (cond ((string-prefix-p "+" l)
                                               'diff-added)
                                              ((string-prefix-p "-" l)
                                               'diff-removed)))))
         (propertized-lines (mapcar propertize-line lines)))

    (save-excursion
      ;; Save point in case the hunk is hidden, so next/previous works as expected
      ;; If the hunk is delete type, then don't hide the hunk
      ;; (because the hunk is located in a non deleted line)
      (when (and diff-hl-show-hunk-inline-popup-hide-hunk
                 (not (eq type 'delete)))
        (let* ((invisible-overlay (make-overlay (overlay-start overlay)
                                                (overlay-end overlay))))
          ;; Make new overlay, since the diff-hl overlay can be changed by diff-hl-flydiff
          (overlay-put invisible-overlay 'invisible t)
          ;; Change default hide popup function, to make the overlay visible
          (setq diff-hl-show-hunk--hide-function
                (lambda ()
                  (overlay-put invisible-overlay 'invisible nil)
                  (delete-overlay invisible-overlay)
                  (diff-hl-inline-popup-hide)))))
      (diff-hl-show-hunk--goto-hunk-overlay overlay)
      (let ((height
             (when smart-lines
               (when (not (eq 0 original-lines-number))
                 original-lines-number)))
            (footer "(q)Quit  (p)Previous  (n)Next  (r)Revert  (c)Copy original"))
        (unless diff-hl-show-staged-changes
          (setq footer (concat footer " (S)Stage")))
        (diff-hl-inline-popup-show
         propertized-lines
         (if (and (boundp 'diff-hl-reference-revision) diff-hl-reference-revision)
             (concat "Diff with " diff-hl-reference-revision)
           "Diff with HEAD")
         footer
         diff-hl-show-hunk-map
         #'diff-hl-show-hunk-hide
         point
         height))
      )))

(defun diff-hl-show-hunk-copy-original-text ()
  "Extracts all the lines from BUFFER starting with '-' to the kill ring."
  (interactive)
  (kill-new diff-hl-show-hunk--original-content)
  (message "Original hunk content added to kill-ring"))

(defun diff-hl-show-hunk-revert-hunk ()
  "Dismiss the popup and revert the current diff hunk."
  (interactive)
  (diff-hl-show-hunk-hide)
  (let (diff-hl-ask-before-revert-hunk)
    (diff-hl-revert-hunk)))

(defun diff-hl-show-hunk-stage-hunk ()
  "Dismiss the popup and stage the current hunk."
  (interactive)
  (diff-hl-show-hunk-hide)
  (diff-hl-stage-current-hunk))

;;;###autoload
(defun diff-hl-show-hunk-previous ()
  "Go to previous hunk/change and show it."
  (interactive)
  (let* ((point (if diff-hl-show-hunk--original-overlay
                    (overlay-start diff-hl-show-hunk--original-overlay)
                  nil))
         (previous-overlay (diff-hl-show-hunk--next-hunk t point)))
    (if (not previous-overlay)
        (message "There is no previous change")
      (diff-hl-show-hunk-hide)
      (diff-hl-show-hunk--goto-hunk-overlay previous-overlay)
      (recenter)
      (diff-hl-show-hunk))))

(defun diff-hl-show-hunk--next-hunk (backward point)
  "Same as `diff-hl-search-next-hunk', but in the current buffer
of `diff-hl-show-hunk'."
  (with-current-buffer (or diff-hl-show-hunk--original-buffer (current-buffer))
    (diff-hl-search-next-hunk backward point)))

(defun diff-hl-show-hunk--goto-hunk-overlay (overlay)
  "Tries to display the whole overlay, and place the point at the
end of the OVERLAY, so posframe/inline is placed below the hunk."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (let ((pt (point)))
      (goto-char (overlay-start overlay))
      (cond
       ((< (point) (window-start))
        (set-window-start nil (point)))
       ((> (point) pt)
        (redisplay))))
    (goto-char (1- (overlay-end overlay)))))

;;;###autoload
(defun diff-hl-show-hunk-next ()
  "Go to next hunk/change and show it."
  (interactive)
  (let* ((point (if diff-hl-show-hunk--original-overlay
                    (overlay-start diff-hl-show-hunk--original-overlay)
                  nil))
         (next-overlay (diff-hl-show-hunk--next-hunk nil point)))
    (if (not next-overlay)
        (message "There is no next change")
      (diff-hl-show-hunk-hide)
      (diff-hl-show-hunk--goto-hunk-overlay next-overlay)
      (recenter)
      (diff-hl-show-hunk))))

;;;###autoload
(defun diff-hl-show-hunk ()
  "Show the VC diff hunk at point.
The backend is determined by `diff-hl-show-hunk-function'."
  (interactive)

  ;; Close any previous hunk
  (save-excursion
    (diff-hl-show-hunk-hide))

  (unless (vc-backend buffer-file-name)
    (user-error "The buffer is not under version control"))

  (diff-hl-find-current-hunk)

  (setq diff-hl-show-hunk--original-overlay nil)

  ;; Store beginning and end of hunk overlay
  (let ((overlay (diff-hl-hunk-overlay-at (point))))
    (when overlay
      (let ((start (overlay-start overlay))
            (end (overlay-end overlay))
            (type (overlay-get overlay 'diff-hl-hunk-type)))
        (setq diff-hl-show-hunk--original-overlay (make-overlay start end))
        (overlay-put diff-hl-show-hunk--original-overlay 'diff-hl-hunk-type type)))

    (unless overlay
      (user-error "Not in a hunk")))

  (cond
   ((not diff-hl-show-hunk-function)
    (message "Please configure `diff-hl-show-hunk-function'")
    (diff-hl-diff-goto-hunk))
   ((let ((buffer-and-line (diff-hl-show-hunk-buffer)))
      (setq diff-hl-show-hunk--original-buffer (current-buffer))
      (setq diff-hl-show-hunk--original-window (selected-window))
      (apply diff-hl-show-hunk-function buffer-and-line))
    ;; We could fall back to `diff-hl-diff-goto-hunk', but the
    ;; current default should work in all environments (both GUI
    ;; and terminal), and if something goes wrong we better show
    ;; the error to the user.
    )))

;;;###autoload
(define-minor-mode diff-hl-show-hunk-mouse-mode
  "Enable margin and fringe to show a posframe/popup with vc diffs when clicked.
By default, the popup shows only the current hunk, and
the line of the hunk that matches the current position is
highlighted.  The face, border and other visual preferences are
customizable.  It can be also invoked with the command
`diff-hl-show-hunk'
\\{diff-hl-show-hunk-mouse-mode-map}"
  :group 'diff-hl-show-hunk
  :lighter "")

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mouse-mode
  diff-hl-show-hunk-mouse-mode
  diff-hl-show-hunk-mouse-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
