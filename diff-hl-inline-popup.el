;;; diff-hl-show-hunk-inline.el --- inline popup using phantom overlays -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author:   Álvaro González <alvarogonzalezsotillo@gmail.com>

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
;; Shows inline popups using phantom overlays.  The lines of the popup
;; can be scrolled.
;;; Code:

(require 'subr-x)

(defvar diff-hl-show-hunk-inline--current-popup nil "The overlay of the current inline popup.")
(defvar diff-hl-show-hunk-inline--current-lines nil "A list of the lines to show in the popup.")
(defvar diff-hl-show-hunk-inline--current-index nil "First line showed in popup.")
(defvar diff-hl-show-hunk-inline--invokinkg-command nil "Command that invoked the popup.")
(defvar diff-hl-show-hunk-inline--current-footer nil "String to be displayed in the footer.")
(defvar diff-hl-show-hunk-inline--current-header nil "String to be displayed in the header.")
(defvar diff-hl-show-hunk-inline--height nil "Height of the popup.")
(defvar diff-hl-show-hunk-inline--current-custom-keymap nil "Keymap to be added to the keymap of the inline popup.")
(defvar diff-hl-show-hunk-inline--close-hook nil "Function to be called when the popup closes.")

(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-popup)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-lines)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-index)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-header)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-footer)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--invokinkg-command)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--current-custom-keymap)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--height)
(make-variable-buffer-local 'diff-hl-show-hunk-inline--close-hook)

(defun diff-hl-show-hunk-inline--splice (list offset length)
  "Compute a sublist of LIST starting at OFFSET, of LENGTH."
  (butlast
   (nthcdr offset list)
   (- (length list) length offset)))

(defun diff-hl-show-hunk-inline--ensure-enough-lines (pos content-height)
  "Ensure there is enough lines below POS to show the inline popup.
CONTENT-HEIGHT specifies the height of the popup."
  (let* ((line (line-number-at-pos pos))
         (end (line-number-at-pos (window-end nil t)))
         (height (+ 6 content-height))
         (overflow (- (+ line height) end)))
    (when (< 0 overflow)
      (run-with-timer 0.1 nil #'scroll-up overflow))))

(defun diff-hl-show-hunk-inline--compute-content-height (&optional content-size)
  "Compute the height of the inline popup.
Default for CONTENT-SIZE is the size of the current lines"
  (let ((content-size (or content-size (length diff-hl-show-hunk-inline--current-lines)))
        (max-size (- (/(window-height) 2) 3)))
    (min content-size max-size)))

(defun diff-hl-show-hunk-inline--compute-content-lines (lines index window-size)
  "Compute the lines to show in the popup.
Compute it from LINES starting at INDEX with a WINDOW-SIZE."
  (let* ((len (length lines))
         (window-size (min window-size len))
         (index (min index (- len window-size))))
    (diff-hl-show-hunk-inline--splice lines index window-size)))

(defun diff-hl-show-hunk-inline--compute-header (width &optional header)
  "Compute the header of the popup.
Compute it from some WIDTH, and some optional HEADER text."
  (let* ((scroll-indicator (if (eq diff-hl-show-hunk-inline--current-index 0) "   " " ⬆ "))
         (header (or header ""))
         (new-width (- width (length header) (length scroll-indicator)))
         (header (if (< new-width 0) "" header))
         (new-width (- width (length header) (length scroll-indicator)))
         (line (propertize (concat (diff-hl-show-hunk-inline--separator new-width)
                                   header scroll-indicator )
                           'face '(:underline t))))
    (concat line "\n") ))

(defun diff-hl-show-hunk-inline--compute-footer (width &optional footer)
  "Compute the header of the popup.
Compute it from some WIDTH, and some optional FOOTER text."
  (let* ((scroll-indicator (if (>= diff-hl-show-hunk-inline--current-index
                                   (- (length diff-hl-show-hunk-inline--current-lines)
                                      diff-hl-show-hunk-inline--height))
                               "   "
                             " ⬇ "))
         (footer (or footer ""))
         (new-width (- width (length footer) (length scroll-indicator)))
         (footer (if (< new-width 0) "" footer))
         (new-width (- width (length footer) (length scroll-indicator)))
         (blank-line (if (display-graphic-p)
                         ""
                       (concat "\n" (propertize (diff-hl-show-hunk-inline--separator width)
                                                'face '(:underline t)))))
         (line (propertize (concat (diff-hl-show-hunk-inline--separator new-width)
                                   footer scroll-indicator)
                           'face '(:overline t))))
    (concat blank-line "\n" line)))

(defun diff-hl-show-hunk-inline--separator (width &optional sep)
  "Return the horizontal separator with character SEP and a WIDTH."
  (let ((sep (or sep ?\s)))
    (make-string width sep)))

(defun diff-hl-show-hunk-inline--available-width ()
  "Compute the available width in chars."
  (let ((magic-adjust 3))
    (if (not (display-graphic-p))
        (let* ((linumber-width (line-number-display-width nil))
               (width (- (window-body-width) linumber-width magic-adjust)))
          width)
      (let* ((font-width (window-font-width))
             (window-width (window-body-width nil t))
             (linenumber-width (line-number-display-width t))
             (available-pixels (- window-width linenumber-width))
             (width (- (/ available-pixels font-width) magic-adjust)))

        ;; https://emacs.stackexchange.com/questions/5495/how-can-i-determine-the-width-of-characters-on-the-screen
        width))))

(defun diff-hl-show-hunk-inline--compute-popup-str (lines index window-size header footer)
  "Compute the string that represents the popup.
There are some content LINES starting at INDEX, with a WINDOW-SIZE.  HEADER and
FOOTER are showed at start and end."
  (let* ((width (diff-hl-show-hunk-inline--available-width))
         (content-lines (diff-hl-show-hunk-inline--compute-content-lines lines index window-size))
         (header (diff-hl-show-hunk-inline--compute-header width header))
         (footer (diff-hl-show-hunk-inline--compute-footer width footer)))
    (concat header (string-join content-lines "\n") footer "\n")))

(defun diff-hl-show-hunk-inline-scroll-to (index)
  "Scroll the inline popup to make visible the line at position INDEX."
  (when diff-hl-show-hunk-inline--current-popup
    (setq diff-hl-show-hunk-inline--current-index (max 0 (min index (- (length diff-hl-show-hunk-inline--current-lines) diff-hl-show-hunk-inline--height))))
    (let* ((str (diff-hl-show-hunk-inline--compute-popup-str
                 diff-hl-show-hunk-inline--current-lines
                 diff-hl-show-hunk-inline--current-index
                 diff-hl-show-hunk-inline--height
                 diff-hl-show-hunk-inline--current-header
                 diff-hl-show-hunk-inline--current-footer)))
      ;; https://debbugs.gnu.org/38563, `company--replacement-string'.
      (add-face-text-property 0 (length str) 'default t str)
      (put-text-property 0 1 'cursor 0 str)
      (overlay-put diff-hl-show-hunk-inline--current-popup 'before-string str))))

(defun diff-hl-show-hunk-inline--popup-down()
  "Scrolls one line down."
  (interactive)
  (diff-hl-show-hunk-inline-scroll-to (1+ diff-hl-show-hunk-inline--current-index) ))

(defun diff-hl-show-hunk-inline--popup-up()
  "Scrolls one line up."
  (interactive)
  (diff-hl-show-hunk-inline-scroll-to (1- diff-hl-show-hunk-inline--current-index) ))

(defun diff-hl-show-hunk-inline--popup-pagedown()
  "Scrolls one page down."
  (interactive)
  (diff-hl-show-hunk-inline-scroll-to (+ diff-hl-show-hunk-inline--current-index  diff-hl-show-hunk-inline--height) ))

(defun diff-hl-show-hunk-inline--popup-pageup()
  "Scrolls one page up."
  (interactive)
  (diff-hl-show-hunk-inline-scroll-to (-  diff-hl-show-hunk-inline--current-index diff-hl-show-hunk-inline--height) ))

(defvar diff-hl-show-hunk-inline-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") #'diff-hl-show-hunk-inline--popup-pageup)
    (define-key map (kbd "M-v") #'diff-hl-show-hunk-inline--popup-pageup)
    (define-key map (kbd "<next>") #'diff-hl-show-hunk-inline--popup-pagedown)
    (define-key map (kbd "C-v") #'diff-hl-show-hunk-inline--popup-pagedown)
    (define-key map (kbd "<up>") #'diff-hl-show-hunk-inline--popup-up)
    (define-key map (kbd "C-p") #'diff-hl-show-hunk-inline--popup-up)
    (define-key map (kbd "<down>") #'diff-hl-show-hunk-inline--popup-down)
    (define-key map (kbd "C-n") #'diff-hl-show-hunk-inline--popup-down)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk-inline-hide)
    (define-key map [escape] #'diff-hl-show-hunk-inline-hide)
    (define-key map (kbd "q") #'diff-hl-show-hunk-inline-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'diff-hl-show-hunk-inline--popup-up)
    (define-key map (kbd "<wheel-up>") #'diff-hl-show-hunk-inline--popup-up)
    (define-key map (kbd "<mouse-5>") #'diff-hl-show-hunk-inline--popup-down)
    (define-key map (kbd "<wheel-down>") #'diff-hl-show-hunk-inline--popup-down)
    map)
  "Keymap for command `diff-hl-show-hunk-inline-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(defun diff-hl-show-hunk-inline--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  ;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
  (let ((keys (where-is-internal command (list diff-hl-show-hunk-inline--current-custom-keymap
                                               diff-hl-show-hunk-inline-transient-mode-map ) t))
        (invoking (eq command diff-hl-show-hunk-inline--invokinkg-command)))
    (or keys invoking)))

(defun diff-hl-show-hunk-inline--post-command-hook ()
  "Called each time a command is executed."
  (let ((allowed-command (or
                          (string-match-p "diff-hl-show-hunk-inline-" (symbol-name this-command))
                          (diff-hl-show-hunk-inline--ignorable-command-p this-command))))
    (unless allowed-command
      (diff-hl-show-hunk-inline-hide))))

(define-minor-mode diff-hl-show-hunk-inline-transient-mode
  "Temporal minor mode to control an inline popup"
  :global nil
  (remove-hook 'post-command-hook #'diff-hl-show-hunk-inline--post-command-hook t)
  (set-keymap-parent diff-hl-show-hunk-inline-transient-mode-map nil)

  (when diff-hl-show-hunk-inline-transient-mode
    (set-keymap-parent diff-hl-show-hunk-inline-transient-mode-map
                       diff-hl-show-hunk-inline--current-custom-keymap)
    (add-hook 'post-command-hook #'diff-hl-show-hunk-inline--post-command-hook 0 t)))

;;;###autoload
(defun diff-hl-show-hunk-inline-hide()
  "Hide the current inline popup."
  (interactive)
  (when diff-hl-show-hunk-inline-transient-mode
    (diff-hl-show-hunk-inline-transient-mode -1))
  (when diff-hl-show-hunk-inline--close-hook
    (funcall diff-hl-show-hunk-inline--close-hook)
    (setq diff-hl-show-hunk-inline--close-hook nil))
  (when diff-hl-show-hunk-inline--current-popup
    (delete-overlay diff-hl-show-hunk-inline--current-popup)
    (setq diff-hl-show-hunk-inline--current-popup nil)))

;;;###autoload
(defun diff-hl-show-hunk-inline-show (lines &optional header footer keymap close-hook point height)
  "Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed."
  (when diff-hl-show-hunk-inline--current-popup
    (delete-overlay diff-hl-show-hunk-inline--current-popup)
    (setq diff-hl-show-hunk-inline--current-popup nil))

  (when (< (diff-hl-show-hunk-inline--compute-content-height 99) 2)
    (user-error "There is no enough vertical space to show the inline popup"))
  (let* ((the-point (or point (line-end-position)))
         (the-buffer (current-buffer))
         (overlay (make-overlay the-point the-point the-buffer)))
    (overlay-put overlay 'phantom t)
    (overlay-put overlay 'diff-hl-show-hunk-inline t)
    (setq diff-hl-show-hunk-inline--current-popup overlay)

    (setq diff-hl-show-hunk-inline--current-lines
          (mapcar (lambda (s) (replace-regexp-in-string "\n" " " s)) lines))
    (setq diff-hl-show-hunk-inline--current-header header)
    (setq diff-hl-show-hunk-inline--current-footer footer)
    (setq diff-hl-show-hunk-inline--invokinkg-command this-command)
    (setq diff-hl-show-hunk-inline--current-custom-keymap keymap)
    (setq diff-hl-show-hunk-inline--close-hook close-hook)
    (setq diff-hl-show-hunk-inline--height (diff-hl-show-hunk-inline--compute-content-height height))
    (setq diff-hl-show-hunk-inline--height (min diff-hl-show-hunk-inline--height
                                            (length diff-hl-show-hunk-inline--current-lines)))
    ;; (diff-hl-show-hunk-inline--ensure-enough-lines point diff-hl-show-hunk-inline--height)
    (diff-hl-show-hunk-inline-transient-mode 1)
    (diff-hl-show-hunk-inline-scroll-to 0)
    overlay))

(defun diff-hl-show-hunk-inline--hide-all ()
  "Testing purposes, use in case some inline popups get stuck in a buffer."
  (interactive)
  (when diff-hl-show-hunk-inline-transient-mode
    (diff-hl-show-hunk-inline-transient-mode -1))
  (setq diff-hl-show-hunk-inline--current-popup nil)
  (let* ((all-overlays (overlays-in (point-min) (point-max)))
         (overlays (cl-remove-if-not (lambda (o)(overlay-get o 'diff-hl-show-hunk-inline)) all-overlays)))
    (dolist (o overlays)
      (delete-overlay o))))

(provide 'diff-hl-show-hunk-inline)
;;; diff-hl-show-hunk-inline ends here
