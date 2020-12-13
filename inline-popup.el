;;; inline-popup.el --- inline popup using phantom overlays -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

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

(defvar inlup--current-popup nil "The overlay of the current inline popup.")
(defvar inlup--current-lines nil)
(defvar inlup--current-index nil)
(defvar inlup--invokinkg-command nil)
(defvar inlup--current-footer nil)
(defvar inlup--current-header nil)
(defvar inlup--current-custom-keymap-exiter nil)
(defvar inlup--current-custom-keymap nil)
(defvar inlup--close-hook nil)

(make-variable-buffer-local 'inlup--current-popup)
(make-variable-buffer-local 'inlup--current-lines)
(make-variable-buffer-local 'inlup--current-index)
(make-variable-buffer-local 'inlup--current-header)
(make-variable-buffer-local 'inlup--current-footer)
(make-variable-buffer-local 'inlup--invokinkg-command)
(make-variable-buffer-local 'inlup--current-custom-keymap-exiter)
(make-variable-buffer-local 'inlup--current-custom-keymap)
(make-variable-buffer-local 'inlup--close-hook)

(defun inlup--splice (list offset length)
  "Compute a sublist of LIST starting at OFFSET, of LENGTH."
  (butlast
   (nthcdr offset list)
   (- (length list) length offset)))

(defun inlup--first-visible-line-in-window ()
  "Return first visible line in current window."
  (line-number-at-pos (window-start)))

(defun inlup--ensure-enough-lines (pos content-height)
  "Ensure there is enough lines below POS to show the inline popup with CONTENT-HEIGHT height."
  (let* ((line (line-number-at-pos pos))
         (end (line-number-at-pos (window-end nil t)))
         (height (+ 6 content-height))
         (overflow (- (+ line height) end)))
    ;; (message "line:%s end:%s height:%s overflow:%s" line end height overflow)
    (when (< 0 overflow)
      (run-with-timer 0.1 nil #'scroll-up overflow))))

(defun inlup--compute-content-height (&optional content-size)
  "Compute the height of the inline popup."
  (let ((content-size (or content-size (length inlup--current-lines)))
        (max-size (- (/(window-height) 2) 3)))
    (min content-size max-size)))

(defun inlup--compute-content-lines (lines index window-size)
  "Compute the lines to show in the popup, from LINES starting at INDEX with a WINDOW-SIZE."
  (let* ((len (length lines))
         (window-size (min window-size len))
         (index (min index (- len window-size))))
    (inlup--splice lines index window-size)))

(defun inlup--compute-header (width &optional header)
  "Compute the header of the popup, with some WIDTH, and some optional HEADER text."
  (let* ((scroll-indicator (if (eq inlup--current-index 0) "   " " ⬆ "))
         (header (or header ""))
         (width (- width (length header) (length scroll-indicator)))
         (line (propertize (concat (inlup--separator width) header scroll-indicator ) 'face '(:underline t))))
    (concat "\n" line "\n") ))

(defun inlup--compute-footer (width &optional footer)
  "Compute the header of the popup, with some WIDTH, and some optional FOOTER text."
  (let* ((scroll-indicator (if (>= inlup--current-index (- (length inlup--current-lines) (inlup--compute-content-height))) "   "     " ⬇ "))
         (footer (or footer ""))
         (new-width(- width (length footer) (length scroll-indicator)))
         (blank-line (propertize (inlup--separator width) 'face '(:underline t)))
         (line (propertize (concat (inlup--separator new-width) footer scroll-indicator))))
    (concat "\n" blank-line "\n" line)))

(defun inlup--separator (width &optional sep)
  "Return the horizontal separator with character SEP and a WIDTH."
  (let ((sep (or sep ?\s)))
    (make-string width sep)))

(defun inlup--compute-popup-str (lines index window-size header footer)
  "Compute the string that represenst the popup, from some
content LINES starting at INDEX, with a WINDOW-SIZE.  HEADER and
FOOTER are showed at start and end."
  (let* ((magic-adjust-that-works-on-my-pc 6)
         (width (- (window-body-width) magic-adjust-that-works-on-my-pc))
         (content-lines (inlup--compute-content-lines lines index window-size))
         (header (inlup--compute-header width header))
         (footer (inlup--compute-footer width footer)))
    (concat header (string-join content-lines  "\n" ) footer)))


(defun inlup-scroll-to (index)
  "Scroll the inline popup to make visible the line at position INDEX."
  (when inlup--current-popup
    (setq inlup--current-index (max 0 (min index (- (length inlup--current-lines) (inlup--compute-content-height)))))
    (let* ((str (inlup--compute-popup-str inlup--current-lines inlup--current-index (inlup--compute-content-height) inlup--current-header inlup--current-footer)))
      (overlay-put inlup--current-popup 'after-string str))))

(defun inlup--popup-down()
  "Scrolls one line down."
  (interactive)
  (inlup-scroll-to (1+ inlup--current-index) ))

(defun inlup--popup-up()
  "Scrolls one line up."
  (interactive)
  (inlup-scroll-to (1- inlup--current-index) ))

(defun inlup--popup-pagedown()
  "Scrolls one page down."
  (interactive)
  (inlup-scroll-to (+ inlup--current-index  (inlup--compute-content-height)) ))

(defun inlup--popup-pageup()
  "Scrolls one page up."
  (interactive)
  (inlup-scroll-to (-  inlup--current-index (inlup--compute-content-height)) ))

(defvar inlup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") #'inlup--popup-pageup)
    (define-key map (kbd "M-v") #'inlup--popup-pageup)
    (define-key map (kbd "<next>") #'inlup--popup-pagedown)
    (define-key map (kbd "C-v") #'inlup--popup-pagedown)
    (define-key map (kbd "<up>") #'inlup--popup-up)
    (define-key map (kbd "C-p") #'inlup--popup-up)
    (define-key map (kbd "<down>") #'inlup--popup-down)
    (define-key map (kbd "C-n") #'inlup--popup-down)
    (define-key map (kbd "C-g") #'inlup-hide)
    (define-key map [escape] #'inlup-hide)
    (define-key map (kbd "q") #'inlup-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'inlup--popup-up)
    (define-key map (kbd "<wheel-up>") #'inlup--popup-up)
    (define-key map (kbd "<mouse-5>") #'inlup--popup-down)
    (define-key map (kbd "<wheel-down>") #'inlup--popup-down)
    
    map)
  "Keymap for command `inlup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(defun inlup--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  ;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
  (let ((keys (where-is-internal command (list inlup--current-custom-keymap inlup-transient-mode-map ) t))
        (invoking (eq command inlup--invokinkg-command)))
    ;;(message "command:%s %s keys:%s" command keys inlup--invokinkg-command)
    (or keys invoking)))
  
(defun inlup--post-command-hook ()
  "Called each time a command is executed."
  (message "terminal-map:%s" overriding-terminal-local-map)
  (let ((allowed-command (or
                          (string-match-p "inlup-" (symbol-name this-command))
                          (inlup--ignorable-command-p this-command))))
    ;;(message "allowed-command:%s" allowed-command)
    (unless allowed-command
      (inlup-hide))))

(define-minor-mode inlup-transient-mode
  "Temporal minor mode to control an inline popup"
  :global nil
  (remove-hook 'post-command-hook #'inlup--post-command-hook t)
  (when inlup-transient-mode
    (set-keymap-parent inlup-transient-mode-map inlup--current-custom-keymap)
    (add-hook 'post-command-hook #'inlup--post-command-hook 0 t)))

;;;###autoload
(defun inlup-hide()
  "Hide the current inline popup."
  (interactive)
  (when inlup-transient-mode
    (inlup-transient-mode -1))
  (when inlup--current-custom-keymap-exiter
    (funcall inlup--current-custom-keymap-exiter)
    (setq inlup--current-custom-keymap-exiter nil))
  (when inlup--close-hook
    (funcall inlup--close-hook)
    (setq inlup--close-hook))
  (when inlup--current-popup
    (delete-overlay inlup--current-popup)
    (setq inlup--current-popup nil)))

;;;###autoload
(defun inlup-show (lines &optional header footer keymap close-hook point)
  "Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed."
  (when (< (inlup--compute-content-height 99) 2)
    (user-error "There is no enough vertical space to show the inline popup"))
  (let* ((the-point (or point (point-at-eol)))
         (the-buffer (current-buffer))
         (overlay (make-overlay the-point the-point the-buffer)))
    (overlay-put overlay 'phantom t)
    (overlay-put overlay 'inlup t)
    (setq inlup--current-popup overlay)

    (setq inlup--current-lines lines)
    (setq inlup--current-header header)
    (setq inlup--current-footer footer)
    (setq inlup--invokinkg-command this-command)
    (setq inlup--current-custom-keymap keymap)
    (setq inlup--close-hook close-hook)
    (inlup--ensure-enough-lines point (inlup--compute-content-height))
    (inlup-transient-mode 1)
    (inlup-scroll-to 0)
    overlay))

(defun inlup--hide-all ()
  "Testing purposes, do not use."
  (interactive)
  (when inlup-transient-mode
    (inlup-transient-mode -1))
  (setq inlup--current-popup nil)
  (let* ((all-overlays (overlays-in (point-min) (point-max)))
         (overlays (cl-remove-if-not (lambda (o)(overlay-get o 'inlup)) all-overlays)))
    (dolist (o overlays)
      (delete-overlay o))))

(defun inlup--test()
  "Testing purposes, do not use."
  (interactive)
  (inlup-show  (list "INICIO" "Hola" "Que" "Tal" "yo" "bien" "gracias" "y" "usted" "pues" "aqui" "ando" "FIN")))

(provide 'inline-popup)
;;; inline-popup ends here
