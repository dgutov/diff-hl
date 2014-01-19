;;; diff-hl-margin.el --- Highlight relative positions of buffer changes -*- lexical-binding: t -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

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

;; This is a global mode, it modifies `diff-hl-mode' to use the
;; library `indicators.el' to draw the relative positions of changes
;; on the right margin.
;;
;; To enable it, add this to your init file:
;;
;; (diff-hl-indicators-mode)
;;
;; `indicators.el' has to be installed separately.

;;; Code:

(require 'diff-hl)
(require 'indicators)

;;;###autoload
(define-minor-mode diff-hl-indicators-mode
  "Toggle relative positions display on the right margin."
  :lighter "" :global t
  (if diff-hl-indicators-mode
      (progn
        (add-hook 'diff-hl-line-functions 'diff-hl-indicators-add-line)
        (add-hook 'diff-hl-clear-hook 'ind-clear-indicators-relative))
    (remove-hook 'diff-hl-line-functions 'diff-hl-indicators-add-line)
    (remove-hook 'diff-hl-clear-hook 'ind-clear-indicators-relative)
    (dolist (buffer (buffer-list))
      (when diff-hl-mode (diff-hl-update)))))

(defun diff-hl-indicators-add-line (type _shape)
  (ind-create-indicator (point) :dynamic t :managed t
                        :face (intern (format "diff-hl-%s" type))))

(provide 'diff-hl-indicators)

;;; diff-hl-indicators.el ends here
