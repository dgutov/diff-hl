;;; diff-hl-show-hunk.el --- Integrate popup/posframe and diff-hl-diff-goto-hunk -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

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
;; Two hunks can overlap due to the contex of the diff algorithm.  To
;; avoid this overlap, set 0 contex lines in `vc-diff'.  The following
;; backends are automatically configured:
;;
;; - GIT: `vc-git-diff-switches' uses '--unified=0'.
;; - Subversion: `vc-svn-diff-switches' uses '--diff-cmd="diff -x -U0"'
;; - Bazaar: `vc-bzr-diff-switches' uses '--context=0'
;; - Mercurial: `vc-hg-diff-switches' uses '--unified=0'


;;; Code:

; REMOVE BEFORE RELEASE, USED FOR FLYCHECK
; (eval-when-compile (add-to-list 'load-path "/home/alvaro/github/diff-hl"))

(require 'diff-hl)

;; This package some runtime dependencies, so we need to declare
;; the external functions and variables
(declare-function posframe-workable-p "posframe")
(declare-function diff-hl-show-hunk-popup "diff-hl-show-hunk-popup")
(declare-function diff-hl-show-hunk-posframe "diff-hl-show-hunk-posframe")

(defvar diff-hl-show-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (concat diff-hl-command-prefix  "*") #'diff-hl-show-hunk)
    (define-key map (concat diff-hl-command-prefix  "{") #'diff-hl-show-hunk-previous)
    (define-key map (concat diff-hl-command-prefix  "}") #'diff-hl-show-hunk-next)
    map)
  "Keymap for command `diff-hl-show-hunk-mode'.")

(defvar diff-hl-show-hunk-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (set-keymap-parent map diff-hl-show-hunk-mode-map)
    map)
  "Keymap for command `diff-hl-show-hunk-mouse-mode'.")

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
  "Narrow the differences to the current hunk."
  :type 'boolean)

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


(defun diff-hl-show-hunk--compute-diffs ()
  "Compute diffs using funcions of diff-hl.
Then put the differences in *vc-diff* buffer.  In order to not
overlap hunks it tries to set the context lines of the diff to
0. Currently, Git, Subversion, Bazaar and Mercurial are
supported."
  (let ((vc-git-diff-switches "--unified=0")
        (vc-svg-diff-switches "--diff-cmd=\"diff -x -U0\"")
        (vc-bzr-diff-switches "--context=0")
        (vc-hg-diff-switches "--unified=0"))
  (diff-hl-diff-goto-hunk)))

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
        (diff-hl-show-hunk--compute-diffs)
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

;;;###autoload
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
      ;;(run-with-timer 0 nil #'diff-hl-show-hunk))))
      (diff-hl-show-hunk))))

;;;###autoload
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
      ;;(run-with-timer 0 nil #'diff-hl-show-hunk))))
      (diff-hl-show-hunk))))

(defun diff-hl-show-hunk-function-default (buffer line)
  "Show a posframe or a popup with the hunk in BUFFER, at  LINE."
  (let* ((posframe-used (when (and (featurep 'posframe) (featurep 'diff-hl-show-hunk-posframe))
                          (when (posframe-workable-p)
                            (diff-hl-show-hunk-posframe buffer line))))
         (popup-used (when (not posframe-used)
                       (when (and (featurep 'popup) (featurep 'diff-hl-show-hunk-popup))
                         (diff-hl-show-hunk-popup buffer line))))
         (success (or posframe-used popup-used)))
    (when (not success)
      (warn "diff-hl-show-hunk: Please install posframe and diff-hl-show-hunk-posframe, or popup and diff-hl-show-hunk-popup, or customize diff-hl-show-hunk-function"))
    success))

;;;###autoload
(defun diff-hl-show-hunk ()
  "Show the diffs at point with vc last version.
The backend is determined by `diff-hl-show-hunk-function'.  If
not, it fallbacks to `diff-hl-diff-goto-hunk'."
  (interactive)
  (cond ((not (vc-backend buffer-file-name))
         (user-error "The buffer is not under version control"))
        ((not (diff-hl-hunk-overlay-at (point)))
         (user-error "There is no modified hunk at pos %s" (point)))
        ((not diff-hl-show-hunk-function)
         (message "Please configure diff-hl-show-hunk-function")
         (diff-hl-diff-goto-hunk))
        ((not (let ((buffer-and-line (diff-hl-show-hunk-buffer)))
                (setq diff-hl-show-hunk--original-buffer (current-buffer))
                (setq diff-hl-show-hunk--original-window (selected-window))
                (apply diff-hl-show-hunk-function buffer-and-line)))
         (message "Current diff-hl-show-hunk-function did not success.  Using diff-hl-diff-goto-hunk as fallback.")
         (diff-hl-diff-goto-hunk))))

;;;###autoload
(define-minor-mode diff-hl-show-hunk-mouse-mode
  "Enables the margin and fringe to show a posframe/popup with vc diffs when clicked.
By default, the posframe/popup shows only the current hunk, and
the line of the hunk that matches the current position is
highlighted.  The face, border and other visual preferences are
customizable.  It can be also invoked with the command
`diff-hl-show-hunk'
\\{diff-hl-show-hunk-mouse-mode-map}"
  :group 'diff-hl-show-hunk-group)

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mouse-mode
  diff-hl-show-hunk-mouse-mode
  diff-hl-show-hunk-mouse-mode)

;;;###autoload
(define-minor-mode diff-hl-show-hunk-mode
  "Enables a keymap with some commands of the `diff-hl-show-hunk' package
\\{diff-hl-show-hunk-mode-map}"

  :group 'diff-hl-show-hunk-group)

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
