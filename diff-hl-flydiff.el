;; Copyright (C) 2015-2021 Free Software Foundation, Inc. -*- lexical-binding: t -*-

;; Author:   Jonathan Hayase <PythonNut@gmail.com>
;; URL:      https://github.com/dgutov/diff-hl

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode enables diffing on-the-fly (i.e. without saving the buffer first)
;; Toggle in all buffers with M-x diff-hl-flydiff-mode

;;; Code:

(require 'diff-hl)
(require 'diff)

(defgroup diff-hl-flydiff nil
  "Highlight changes on the fly"
  :group 'diff-hl)

(defcustom diff-hl-flydiff-delay 0.3
  "The idle delay in seconds before highlighting is updated."
  :type 'number)

(defvar diff-hl-flydiff-modified-tick nil)
(defvar diff-hl-flydiff-timer nil)
(make-variable-buffer-local 'diff-hl-flydiff-modified-tick)

(defun diff-hl-flydiff-changes-buffer (file &optional backend)
  (setq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick))
  (diff-hl-diff-buffer-with-reference file " *diff-hl-diff*" backend))

(defun diff-hl-flydiff-update ()
  (unless (or
           (not diff-hl-mode)
           (eq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick))
           (not buffer-file-name)
           (file-remote-p default-directory)
           (not (file-exists-p buffer-file-name)))
    (diff-hl-update)))

(defun diff-hl-flydiff/modified-p (_state)
  (buffer-modified-p))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works."
  :lighter "" :global t
  (and diff-hl-flydiff-timer
       (cancel-timer diff-hl-flydiff-timer))

  (if diff-hl-flydiff-mode
      (progn
        (advice-add 'diff-hl-overlay-modified :override #'ignore)

        (advice-add 'diff-hl-modified-p :before-until
                    #'diff-hl-flydiff/modified-p)
        (advice-add 'diff-hl-changes-buffer :override
                    #'diff-hl-flydiff-changes-buffer)
        (setq diff-hl-flydiff-timer
              (run-with-idle-timer diff-hl-flydiff-delay t #'diff-hl-flydiff-update)))

    (advice-remove 'diff-hl-overlay-modified #'ignore)

    (advice-remove 'diff-hl-modified-p #'diff-hl-flydiff/modified-p)
    (advice-remove 'diff-hl-changes-buffer #'diff-hl-flydiff-changes-buffer)))

(provide 'diff-hl-flydiff)
