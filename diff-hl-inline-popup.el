;;; diff-hl-inline-popup.el --- inline popup using phantom overlays -*- lexical-binding: t -*-

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

(defvar diff-hl-inline-popup--current-popup nil "The overlay of the current inline popup.")
(defvar diff-hl-inline-popup--current-lines nil "A list of the lines to show in the popup.")
(defvar diff-hl-inline-popup--current-index nil "First line showed in popup.")
(defvar diff-hl-inline-popup--invokinkg-command nil "Command that invoked the popup.")
(defvar diff-hl-inline-popup--current-footer nil "String to be displayed in the footer.")
(defvar diff-hl-inline-popup--current-header nil "String to be displayed in the header.")
(defvar diff-hl-inline-popup--height nil "Height of the popup.")
(defvar diff-hl-inline-popup--current-custom-keymap nil "Keymap to be added to the keymap of the inline popup.")
(defvar diff-hl-inline-popup--close-hook nil "Function to be called when the popup closes.")

(make-variable-buffer-local 'diff-hl-inline-popup--current-popup)
(make-variable-buffer-local 'diff-hl-inline-popup--current-lines)
(make-variable-buffer-local 'diff-hl-inline-popup--current-index)
(make-variable-buffer-local 'diff-hl-inline-popup--current-header)
(make-variable-buffer-local 'diff-hl-inline-popup--current-footer)
(make-variable-buffer-local 'diff-hl-inline-popup--invokinkg-command)
(make-variable-buffer-local 'diff-hl-inline-popup--current-custom-keymap)
(make-variable-buffer-local 'diff-hl-inline-popup--height)
(make-variable-buffer-local 'diff-hl-inline-popup--close-hook)

(defun diff-hl-inline-popup--splice (list offset length)
  "Compute a sublist of LIST starting at OFFSET, of LENGTH."
  (butlast
   (nthcdr offset list)
   (- (length list) length offset)))

(defun diff-hl-inline-popup--ensure-enough-lines (pos content-height)
  "Ensure there is enough lines below POS to show the inline popup.
CONTENT-HEIGHT specifies the height of the popup."
  (let* ((line (line-number-at-pos pos))
         (end (line-number-at-pos (window-end nil t)))
         (height (+ 6 content-height))
         (overflow (- (+ line height) end)))
    (when (< 0 overflow)
      (run-with-timer 0.1 nil #'scroll-up overflow))))

(defun diff-hl-inline-popup--compute-content-height (&optional content-size)
  "Compute the height of the inline popup.
Default for CONTENT-SIZE is the size of the current lines"
  (let ((content-size (or content-size (length diff-hl-inline-popup--current-lines)))
        (max-size (- (/(window-height) 2) 3)))
    (min content-size max-size)))

(defun diff-hl-inline-popup--compute-content-lines (lines index window-size)
  "Compute the lines to show in the popup.
Compute it from LINES starting at INDEX with a WINDOW-SIZE."
  (let* ((len (length lines))
         (window-size (min window-size len))
         (index (min index (- len window-size))))
    (diff-hl-inline-popup--splice lines index window-size)))

(defun diff-hl-inline-popup--compute-header (width &optional header)
  "Compute the header of the popup.
Compute it from some WIDTH, and some optional HEADER text."
  (let* ((scroll-indicator (if (eq diff-hl-inline-popup--current-index 0) "   " " ⬆ "))
         (header (or header ""))
         (new-width (- width (length header) (length scroll-indicator)))
         (header (if (< new-width 0) "" header))
         (new-width (- width (length header) (length scroll-indicator)))
         (line (propertize (concat (diff-hl-inline-popup--separator new-width)
                                   header scroll-indicator )
                           'face '(:underline t))))
    (concat line "\n") ))

(defun diff-hl-inline-popup--compute-footer (width &optional footer)
  "Compute the header of the popup.
Compute it from some WIDTH, and some optional FOOTER text."
  (let* ((scroll-indicator (if (>= diff-hl-inline-popup--current-index
                                   (- (length diff-hl-inline-popup--current-lines)
                                      diff-hl-inline-popup--height))
                               "   "
                             " ⬇ "))
         (footer (or footer ""))
         (new-width (- width (length footer) (length scroll-indicator)))
         (footer (if (< new-width 0) "" footer))
         (new-width (- width (length footer) (length scroll-indicator)))
         (blank-line (if (display-graphic-p)
                         ""
                       (concat "\n" (propertize (diff-hl-inline-popup--separator width)
                                                'face '(:underline t)))))
         (line (propertize (concat (diff-hl-inline-popup--separator new-width)
                                   footer scroll-indicator)
                           'face '(:overline t))))
    (concat blank-line "\n" line)))

(defun diff-hl-inline-popup--separator (width &optional sep)
  "Return the horizontal separator with character SEP and a WIDTH."
  (let ((sep (or sep ?\s)))
    (make-string width sep)))

(defun diff-hl-inline-popup--available-width ()
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

(defun diff-hl-inline-popup--compute-popup-str (lines index window-size header footer)
  "Compute the string that represents the popup.
There are some content LINES starting at INDEX, with a WINDOW-SIZE.  HEADER and
FOOTER are showed at start and end."
  (let* ((width (diff-hl-inline-popup--available-width))
         (content-lines (diff-hl-inline-popup--compute-content-lines lines index window-size))
         (header (diff-hl-inline-popup--compute-header width header))
         (footer (diff-hl-inline-popup--compute-footer width footer)))
    (concat header (string-join content-lines "\n") footer "\n")))

(defun diff-hl-inline-popup-scroll-to (index)
  "Scroll the inline popup to make visible the line at position INDEX."
  (when diff-hl-inline-popup--current-popup
    (setq diff-hl-inline-popup--current-index (max 0 (min index (- (length diff-hl-inline-popup--current-lines) diff-hl-inline-popup--height))))
    (let* ((str (diff-hl-inline-popup--compute-popup-str
                 diff-hl-inline-popup--current-lines
                 diff-hl-inline-popup--current-index
                 diff-hl-inline-popup--height
                 diff-hl-inline-popup--current-header
                 diff-hl-inline-popup--current-footer)))
      ;; https://debbugs.gnu.org/38563, `company--replacement-string'.
      (add-face-text-property 0 (length str) 'default t str)
      (overlay-put diff-hl-inline-popup--current-popup 'after-string str))))

(defun diff-hl-inline-popup--popup-down()
  "Scrolls one line down."
  (interactive)
  (diff-hl-inline-popup-scroll-to (1+ diff-hl-inline-popup--current-index) ))

(defun diff-hl-inline-popup--popup-up()
  "Scrolls one line up."
  (interactive)
  (diff-hl-inline-popup-scroll-to (1- diff-hl-inline-popup--current-index) ))

(defun diff-hl-inline-popup--popup-pagedown()
  "Scrolls one page down."
  (interactive)
  (diff-hl-inline-popup-scroll-to (+ diff-hl-inline-popup--current-index  diff-hl-inline-popup--height) ))

(defun diff-hl-inline-popup--popup-pageup()
  "Scrolls one page up."
  (interactive)
  (diff-hl-inline-popup-scroll-to (-  diff-hl-inline-popup--current-index diff-hl-inline-popup--height) ))

(defvar diff-hl-inline-popup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") #'diff-hl-inline-popup--popup-pageup)
    (define-key map (kbd "M-v") #'diff-hl-inline-popup--popup-pageup)
    (define-key map (kbd "<next>") #'diff-hl-inline-popup--popup-pagedown)
    (define-key map (kbd "C-v") #'diff-hl-inline-popup--popup-pagedown)
    (define-key map (kbd "<up>") #'diff-hl-inline-popup--popup-up)
    (define-key map (kbd "C-p") #'diff-hl-inline-popup--popup-up)
    (define-key map (kbd "<down>") #'diff-hl-inline-popup--popup-down)
    (define-key map (kbd "C-n") #'diff-hl-inline-popup--popup-down)
    (define-key map (kbd "C-g") #'diff-hl-inline-popup-hide)
    (define-key map [escape] #'diff-hl-inline-popup-hide)
    (define-key map (kbd "q") #'diff-hl-inline-popup-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'diff-hl-inline-popup--popup-up)
    (define-key map (kbd "<wheel-up>") #'diff-hl-inline-popup--popup-up)
    (define-key map (kbd "<mouse-5>") #'diff-hl-inline-popup--popup-down)
    (define-key map (kbd "<wheel-down>") #'diff-hl-inline-popup--popup-down)
    map)
  "Keymap for command `diff-hl-inline-popup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(defun diff-hl-inline-popup--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  ;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
  (let ((keys (where-is-internal command (list diff-hl-inline-popup--current-custom-keymap
                                               diff-hl-inline-popup-transient-mode-map ) t))
        (invoking (eq command diff-hl-inline-popup--invokinkg-command)))
    (or keys invoking)))

(defun diff-hl-inline-popup--post-command-hook ()
  "Called each time a command is executed."
  (let ((allowed-command (or
                          (string-match-p "diff-hl-inline-popup-" (symbol-name this-command))
                          (diff-hl-inline-popup--ignorable-command-p this-command))))
    (unless allowed-command
      (diff-hl-inline-popup-hide))))

(define-minor-mode diff-hl-inline-popup-transient-mode
  "Temporal minor mode to control an inline popup"
  :global nil
  (remove-hook 'post-command-hook #'diff-hl-inline-popup--post-command-hook t)
  (set-keymap-parent diff-hl-inline-popup-transient-mode-map nil)

  (when diff-hl-inline-popup-transient-mode
    (set-keymap-parent diff-hl-inline-popup-transient-mode-map
                       diff-hl-inline-popup--current-custom-keymap)
    (add-hook 'post-command-hook #'diff-hl-inline-popup--post-command-hook 0 t)))

;;;###autoload
(defun diff-hl-inline-popup-hide()
  "Hide the current inline popup."
  (interactive)
  (when diff-hl-inline-popup-transient-mode
    (diff-hl-inline-popup-transient-mode -1))
  (when diff-hl-inline-popup--close-hook
    (funcall diff-hl-inline-popup--close-hook)
    (setq diff-hl-inline-popup--close-hook nil))
  (when diff-hl-inline-popup--current-popup
    (delete-overlay diff-hl-inline-popup--current-popup)
    (setq diff-hl-inline-popup--current-popup nil)))

;;;###autoload
(defun diff-hl-inline-popup-show (lines &optional header footer keymap close-hook point height)
  "Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed."
  (when diff-hl-inline-popup--current-popup
    (delete-overlay diff-hl-inline-popup--current-popup)
    (setq diff-hl-inline-popup--current-popup nil))

  (when (< (diff-hl-inline-popup--compute-content-height 99) 2)
    (user-error "There is no enough vertical space to show the inline popup"))
  (let* ((the-point (or point (line-end-position)))
         (the-buffer (current-buffer))
         (overlay (make-overlay the-point the-point the-buffer)))
    (overlay-put overlay 'phantom t)
    (overlay-put overlay 'diff-hl-inline-popup t)
    (setq diff-hl-inline-popup--current-popup overlay)

    (setq diff-hl-inline-popup--current-lines
          (mapcar (lambda (s) (replace-regexp-in-string "\n" " " s)) lines))
    (setq diff-hl-inline-popup--current-header header)
    (setq diff-hl-inline-popup--current-footer footer)
    (setq diff-hl-inline-popup--invokinkg-command this-command)
    (setq diff-hl-inline-popup--current-custom-keymap keymap)
    (setq diff-hl-inline-popup--close-hook close-hook)
    (setq diff-hl-inline-popup--height (diff-hl-inline-popup--compute-content-height height))
    (setq diff-hl-inline-popup--height (min diff-hl-inline-popup--height
                                            (length diff-hl-inline-popup--current-lines)))
    ;; (diff-hl-inline-popup--ensure-enough-lines point diff-hl-inline-popup--height)
    (diff-hl-inline-popup-transient-mode 1)
    (diff-hl-inline-popup-scroll-to 0)
    overlay))

(defun diff-hl-inline-popup--hide-all ()
  "Testing purposes, use in case some inline popups get stuck in a buffer."
  (interactive)
  (when diff-hl-inline-popup-transient-mode
    (diff-hl-inline-popup-transient-mode -1))
  (setq diff-hl-inline-popup--current-popup nil)
  (let* ((all-overlays (overlays-in (point-min) (point-max)))
         (overlays (cl-remove-if-not (lambda (o)(overlay-get o 'diff-hl-inline-popup)) all-overlays)))
    (dolist (o overlays)
      (delete-overlay o))))

(provide 'diff-hl-inline-popup)
;;; diff-hl-inline-popup ends here
