;;; diff-hl-show-hunk-quick-peek.el --- posframe backend for diff-hl-show-hunk -*- lexical-binding: t -*-

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
;;
;;  This provides `diff-hl-show-hunk-quick-peek' than can be used as
;;  `diff-hl-show-hunk-function'.  `scrollable-quick-peek' is a
;;  runtime dependency, it is not required by this package, but it
;;  should be installed.
;;
;;; Code:

(require 'diff-hl-show-hunk)

;; This package uses some runtime dependencies, so we need to declare
;; the external functions and variables
(declare-function scrollable-quick-peek-show "scrollable-quick-peek")
(declare-function quick-peek-hide "quick-peek")

(defvar diff-hl-show-hunk-quick-peek--current-custom-keymap nil)
(defvar diff-hl-show-hunk-quick-peek--invoking-command nil)

(defvar diff-hl-show-hunk-quick-peek-transient-mode-map
  (let ((map (make-sparse-keymap)))

    ;; close quick-peek
    (define-key map [escape] #'diff-hl-show-hunk-quick-peek-hide)
    (define-key map (kbd "q") #'diff-hl-show-hunk-quick-peek-hide)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk-quick-peek-hide)

    ;; buffer scroll
    (define-key map (kbd "<prior>") #'scrollable-quick-peek-scroll-down)
    (define-key map (kbd "M-v") #'scrollable-quick-peek-scroll-down)
    (define-key map (kbd "<next>") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "C-v") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "<up>") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "C-p") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "<down>") #'scrollable-quick-peek-scroll-down)
    (define-key map (kbd "C-n") #'scrollable-quick-peek-scroll-down)
    (define-key map (kbd "<mouse-4>") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "<wheel-up>") #'scrollable-quick-peek-scroll-up)
    (define-key map (kbd "<mouse-5>") #'scrollable-quick-peek-scroll-down)
    (define-key map (kbd "<wheel-down>") #'scrollable-quick-peek-scroll-down)

    ;; move between hunks
    (define-key map (kbd "p")   #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n")   #'diff-hl-show-hunk-next)
    (define-key map (kbd "c")   #'diff-hl-show-hunk-copy-original-text)
    (define-key map (kbd "r")   #'diff-hl-show-hunk-revert-hunk)
    (define-key map (kbd "[")   #'diff-hl-show-hunk-previous)
    (define-key map (kbd "]")   #'diff-hl-show-hunk-next)
    (define-key map (kbd "{")   #'diff-hl-show-hunk-previous)
    (define-key map (kbd "}")   #'diff-hl-show-hunk-next)
    map)
  "Keymap for command `diff-hl-show-hunk-quick-peek-transient-mode'.")

(defun diff-hl-show-hunk-quick-peek--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing an inline popup."
  ;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
  (let ((keys (where-is-internal command (list diff-hl-show-hunk-quick-peek-transient-mode-map ) t))
        (invoking (eq command diff-hl-show-hunk-quick-peek--invoking-command)))
    (or keys invoking)))
  
(defun diff-hl-show-hunk-quick-peek--post-command-hook ()
  "Called each time a command is executed."
  (let ((allowed-command (or
                          (string-match-p "diff-hl-show-hunk-quick-peek-" (symbol-name this-command))
                          (diff-hl-show-hunk-quick-peek--ignorable-command-p this-command))))
    (unless allowed-command
      (diff-hl-show-hunk-quick-peek-hide))))

(define-minor-mode diff-hl-show-hunk-quick-peek-transient-mode
  "Temporal minor mode to control an inline popup"
  :global nil
  (remove-hook 'post-command-hook #'diff-hl-show-hunk-quick-peek--post-command-hook t)
  (set-keymap-parent diff-hl-show-hunk-quick-peek-transient-mode-map nil)
  
  (when diff-hl-show-hunk-quick-peek-transient-mode
    (when diff-hl-show-hunk-quick-peek--current-custom-keymap
      (set-keymap-parent diff-hl-show-hunk-quick-peek-transient-mode-map diff-hl-show-hunk-quick-peek--current-custom-keymap))
    (add-hook 'post-command-hook #'diff-hl-show-hunk-quick-peek--post-command-hook 0 t)))

;;;###autoload
(defun diff-hl-show-hunk-quick-peek-hide ()
  "Hides the quick-peek overlay that displays the hunk."
  (interactive)
  (diff-hl-show-hunk-quick-peek-transient-mode -1)
  (quick-peek-hide))

;;;###autoload
(defun diff-hl-show-hunk-quick-peek (buffer _line)
  "Implementation to show the hunk in quick-peek."
  (unless (require 'quick-peek nil t)
    (user-error
     (concat
      "`diff-hl-show-hunk-quick-peek' requires the `quick-peek' package."
      "  Please install it or customize `diff-hl-show-hunk-function'.")))

  (diff-hl-show-hunk-quick-peek-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk-quick-peek-hide)
  (setq diff-hl-show-hunk-quick-peek--invoking-command this-command)
  
  (let* ((str (with-current-buffer buffer (buffer-string)))
         (line-count (with-current-buffer buffer
                       (line-number-at-pos (point-max))))
         (max-h (min line-count (/ (window-height) 2))))
    (scrollable-quick-peek-show str (point) 3 max-h))
  (diff-hl-show-hunk-quick-peek-transient-mode))

(provide 'diff-hl-show-hunk-quick-peek)
;;; diff-hl-show-hunk-quick-peek.el ends here
