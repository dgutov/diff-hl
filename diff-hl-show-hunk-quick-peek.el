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
;;  `diff-hl-show-hunk-function'.  `quick-peek' is a runtime dependency,
;;  it is not required by this package, but it should be installed.
;;
;;; Code:

(eval-when-compile (add-to-list 'load-path "/home/alvaro/github/diff-hl"))
(require 'diff-hl-show-hunk)

;; This package uses some runtime dependencies, so we need to declare
;; the external functions and variables
(message "REMOVE OR FULLFILL")
(declare-function quick-peek-show "quick-peek")
(declare-function quick-peek-hide "quick-peek")

(defvar diff-hl-show-hunk-quick-peek--current-custom-keymap nil)
(defvar diff-hl-show-hunk-quick-peek--invoking-command nil)

(defvar diff-hl-show-hunk-quick-peek-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] #'diff-hl-show-hunk-quick-peek-hide)
    (define-key map (kbd "q") #'diff-hl-show-hunk-quick-peek-hide)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk-quick-peek-hide)
    (define-key map (kbd "<prior>") #'scroll-down-command)
    (define-key map (kbd "M-v") #'scroll-down-command)
    (define-key map (kbd "<next>") #'scroll-up-command)
    (define-key map (kbd "C-v") #'scroll-up-command)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "C-p") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "<mouse-4>") #'previous-line)
    (define-key map (kbd "<wheel-up>") #'previous-line)
    (define-key map (kbd "<mouse-5>") #'next-line)
    (define-key map (kbd "<wheel-down>") #'next-line)
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
      (message "this-command:%s" this-command)
      (message "6")
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
  (message "1")
  (unless (require 'quick-peek nil t)
    (user-error
     (concat
      "`diff-hl-show-hunk-quick-peek' requires the `quick-peek' package."
      "  Please install it or customize `diff-hl-show-hunk-function'.")))

  ;;(diff-hl-show-hunk-quick-peek-hide)
  (message "2")
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk-quick-peek-hide)
  (setq diff-hl-show-hunk-quick-peek--invoking-command this-command)
  
  (let ((str (with-current-buffer buffer (buffer-string))))
    (quick-peek-show str (point) 3 'none))
  (message "3")
  (diff-hl-show-hunk-quick-peek-transient-mode)
  )

(provide 'diff-hl-show-hunk-quick-peek)
;;; diff-hl-show-hunk-quick-peek.el ends here
