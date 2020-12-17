;;; diff-hl-show-hunk-inline-popup.el --- inline popup backend for diff-hl-show-hunk -*- lexical-binding: t -*-

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
;; Provides `diff-hl-show-hunk-inline-popup' than can be used as `diff-hl-show-hunk-function'
;;; Code:

(require 'diff-hl-show-hunk)
(require 'inline-popup)

(defface diff-hl-show-hunk-added-face  '((t (:foreground "green"))) "Face for added lines" :group 'diff-hl-show-hunk-group)
(defface diff-hl-show-hunk-deleted-face  '((t (:foreground "red" :strike-through t))) "Face for deleted lines" :group 'diff-hl-show-hunk-group)

(defvar diff-hl-show-hunk--inlup-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n") #'diff-hl-show-hunk-next)
    (define-key map (kbd "r") (lambda ()
                                (interactive) (diff-hl-show-hunk-hide) (diff-hl-revert-hunk)))
    (define-key map (kbd "C-x v {") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "C-x v }") #'diff-hl-show-hunk-next)
    map))

;;;###autoload
(defun diff-hl-show-hunk-inline-popup (buffer line)
  "Implementation to show the hunk in a inline popup.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  (inlup-hide)
  (setq diff-hl-show-hunk--hide-function #'inlup-hide)
  
  (let* ((lines (split-string (with-current-buffer buffer (buffer-string)) "[\n\r]+" ))
         (line (max 0 (- line 1)))
         (propertize-line (lambda (l) (propertize l 'face (cond ((string-prefix-p "+" l) 'diff-hl-show-hunk-added-face)
                                                                ((string-prefix-p "-" l) 'diff-hl-show-hunk-deleted-face)))))
         (propertized-lines (mapcar propertize-line lines))
         (clicked-line (propertize (nth line lines) 'face 'diff-hl-show-hunk-clicked-line-face)))
    (setcar (nthcdr line propertized-lines) clicked-line)
    (inlup-show propertized-lines "Diff with HEAD" "(q)Quit  (p)Previous  (n)Next  (r)Revert" diff-hl-show-hunk--inlup-map)
    (inlup-scroll-to line))
  t)

(provide 'diff-hl-show-hunk-inline-popup)
;;; diff-hl-show-hunk-inline-popup ends here
