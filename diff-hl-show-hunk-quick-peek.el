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
;;  `diff-hl-show-hunk-function'. `quick-peek' is a runtime dependency,
;;  it is not required by this package, but it should be installed.
;;
;;; Code:

(require 'diff-hl-show-hunk)

;; This package uses some runtime dependencies, so we need to declare
;; the external functions and variables
(message "REMOVE OR FULLFILL")

(defgroup diff-hl-show-hunk-quick-peek nil
  "Show vc diffs in a posframe."
  :group 'diff-hl-show-hunk)



(defun diff-hl-show-hunk--quick-peek-hide ()
  (quick-peek-hide))

;;;###autoload
(defun diff-hl-show-hunk-quick-peek (buffer line)
  "Implementation to show the hunk in quick-peek."

  (unless (require 'quick-peek nil t)
    (user-error
     (concat
      "`diff-hl-show-hunk-quick-peek' requires the `quick-peek' package."
      "  Please install it or customize `diff-hl-show-hunk-function'.")))

  (diff-hl-show-hunk--quick-peek-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk--quick-peek-hide)

  (let ((str (with-current-buffer buffer (buffer-string))))
    (quick-peek-show str)))

(provide 'diff-hl-show-hunk-quick-peek)
;;; diff-hl-show-hunk-quick-peek.el ends here
