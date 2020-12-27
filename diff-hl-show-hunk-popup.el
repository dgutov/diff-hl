;;; diff-hl-show-hunk-popup.el --- popup backend for diff-hl-show-hunk -*- lexical-binding: t -*-

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
;; Provides `diff-hl-show-hunk-popup' than can be used as
;; `diff-hl-show-hunk-function'.  'popup.el' is a runtime dependency,
;; it is not required by this package, but should be installed and
;; loaded.

;;; Code:

; REMOVE BEFORE RELEASE, USED FOR FLYCHECK
; (eval-when-compile (add-to-list 'load-path "/home/alvaro/github/diff-hl"))


;; This package uses some runtime dependencies, so we need to declare
;; the external functions and variables
(declare-function popup-scroll-up "popup")
(declare-function popup-height "popup")
(declare-function popup-scroll-down "popup")
(declare-function popup-hide "popup")
(declare-function popup-hide "popup")
(declare-function popup-create "popup")
(declare-function popup-set-list "popup")
(declare-function popup-select "popup")
(declare-function popup-draw "popup")
(defvar diff-hl-show-hunk--hide-function)

(require 'diff-hl-show-hunk)

(defvar diff-hl-show-hunk--popup nil "Popup where show the current hunk.")

(defgroup diff-hl-show-hunk-popup nil
  "Show vc diffs in a posframe."
  :group 'diff-hl-show-hunk)

(defcustom diff-hl-show-hunk-popup-default-height
  20
  "Height of the popup."
  :type 'integer)

(defcustom diff-hl-show-hunk-popup-width-function
  #'diff-hl-show-hunk-popup-width
  "Function to compute the width of the popup.  By default, it is `diff-hl-show-hunk-popup-width'."
  :type 'function)

(defcustom diff-hl-show-hunk-popup-height-function
  #'diff-hl-show-hunk-popup-height
  "Function to compute the height of the popup.  By default, it returns the min of `diff-hl-show-hunk-popup-height' and thee available height."
  :type 'function)

(defun diff-hl-show-hunk-popup-height ()
  "Desired size of the displayed popup."
  (let ((magic-adjust-working-in-my-pc 3))
    (min diff-hl-show-hunk-popup-default-height (- (window-body-height) magic-adjust-working-in-my-pc))))

(defun diff-hl-show-hunk-popup-width ()
  "Return the available width."
  (let ((magic-adjust-working-in-my-pc 6))
    (- (window-body-width) magic-adjust-working-in-my-pc)))

(defun diff-hl-show-hunk--popup-up ()
  "Used in `diff-hl-show-hunk--popup-transient-mode-map'."
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-up diff-hl-show-hunk--popup)))

(defun diff-hl-show-hunk--popup-pageup ()
  "Used in `diff-hl-show-hunk--popup-transient-mode-map'."
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-up diff-hl-show-hunk--popup (popup-height diff-hl-show-hunk--popup))))

(defun diff-hl-show-hunk--popup-pagedown ()
  "Used in `diff-hl-show-hunk--popup-transient-mode-map'."
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-down diff-hl-show-hunk--popup (popup-height diff-hl-show-hunk--popup))))

(defun diff-hl-show-hunk--popup-down ()
  "Used in `diff-hl-show-hunk--popup-transient-mode-map'."
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-down diff-hl-show-hunk--popup)))

(defun diff-hl-show-hunk--popup-hide ()
  "Used in `diff-hl-show-hunk--popup-transient-mode-map'."
  (interactive)
  (diff-hl-show-hunk--popup-transient-mode -1)
  (when diff-hl-show-hunk--popup
    (popup-hide diff-hl-show-hunk--popup)
    (setq diff-hl-show-hunk--popup nil)))

(defvar diff-hl-show-hunk--popup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") #'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "M-v") #'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "<next>") #'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "C-v") #'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "<up>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "C-p") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<down>") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-n") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk-hide)
    (define-key map [escape] #'diff-hl-show-hunk-hide)
    (define-key map (kbd "q") #'diff-hl-show-hunk-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<wheel-up>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<mouse-5>") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "<wheel-down>") #'diff-hl-show-hunk--popup-down)

    (define-key map (kbd "c") #'diff-hl-show-hunk-original-text-to-kill-ring)
    (define-key map (kbd "p") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n") #'diff-hl-show-hunk-next)
    (define-key map (kbd "C-x v {") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "C-x v }") #'diff-hl-show-hunk-next)
    map)
  "Keymap for command `diff-hl-show-hunk--popup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(define-minor-mode diff-hl-show-hunk--popup-transient-mode
  "Temporal minor mode to control diff-hl popup."
  :global nil
  :lighter ""
  (remove-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)
  (when diff-hl-show-hunk--popup-transient-mode
    (add-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)))

(defun diff-hl-show-hunk--popup-post-command-hook ()
  "Called each time the region is changed."
  (let ((allowed-command (or
                          (diff-hl-show-hunk-ignorable-command-p this-command)
                          (string-match-p "diff-hl-" (symbol-name this-command)))))
    (unless allowed-command
      (diff-hl-show-hunk--popup-hide))))

(defun diff-hl-show-hunk-popup (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  
  (unless (featurep 'popup)
    (user-error "Required package for diff-hl-show-hunk-popup not available: popup.  Please customize diff-hl-show-hunk-function"))

  (require 'popup)
  
  (diff-hl-show-hunk--popup-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk--popup-hide)
  
  (let* ((lines (split-string (with-current-buffer buffer (buffer-string)) "[\n\r]+" ))
         (width (funcall diff-hl-show-hunk-popup-width-function))
         (height (funcall diff-hl-show-hunk-popup-height-function))
         (popup (popup-create (point-at-bol) width height :around t :scroll-bar t))
         (line (max 0 (- line 1)))
         (clicked-line (propertize (nth line lines) 'face 'diff-hl-show-hunk-clicked-line-face)))
    (setq diff-hl-show-hunk--popup popup)
    (setcar (nthcdr line lines) clicked-line)
    (popup-set-list popup lines)
    (popup-scroll-down popup line)
    (popup-select popup line)
    (popup-draw popup)
    (diff-hl-show-hunk--popup-transient-mode))
  t)

(provide 'diff-hl-show-hunk-popup)
;;; diff-hl-show-hunk-popup.el ends here
