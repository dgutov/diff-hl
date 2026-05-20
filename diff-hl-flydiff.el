;; Copyright (C) 2015-2026 Free Software Foundation, Inc. -*- lexical-binding: t -*-

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
  :type 'number
  :group 'diff-hl-flydiff)

(defvar-local diff-hl-flydiff-modified-tick nil)
(defvar diff-hl-flydiff-timer nil)

(defun diff-hl-flydiff-changes-buffer (file backend &optional new-rev buffer)
  (setq buffer (or buffer " *diff-hl-diff*"))
  (if new-rev
      (diff-hl-with-diff-switches
       (diff-hl-diff-against-reference file backend buffer new-rev))
    (diff-hl-diff-buffer-with-reference file buffer backend)))

(defun diff-hl-flydiff-update (orig-fun)
  "Update diff markings if buffer has been modified since last call.

Calls ORIG-FUN only when buffer has been modified since last
call, buffer exists as a file, and it is not a remote file."
  (when (and diff-hl-mode
             (diff-hl-flydiff/modified-p nil)
             (stringp buffer-file-name)
             (not (file-remote-p default-directory))
             (file-exists-p buffer-file-name))
    (apply orig-fun nil)))

(defun diff-hl-flydiff/modified-p (state)
  "Return t if buffer modified since last call unless given STATE
is one of `added' or `missing', otherwise nil."
  (unless (memq state '(added missing))
    (let ((buffer-state (not (eq diff-hl-flydiff-modified-tick
                          (buffer-chars-modified-tick)))))
      (if buffer-state
          (setq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick)))
      buffer-state)))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works."
  :lighter "" :global t
  (and diff-hl-flydiff-timer
       (cancel-timer diff-hl-flydiff-timer))

  (if diff-hl-flydiff-mode
      (progn
        ;; Ensure current diff state is shown when mode is enabled.
        (setq diff-hl-flydiff-modified-tick nil)

        (advice-add 'diff-hl-overlay-modified :override #'ignore)

        (advice-add 'diff-hl-modified-p :before-until
                    #'diff-hl-flydiff/modified-p)
        (advice-add 'diff-hl-changes-buffer :override
                    #'diff-hl-flydiff-changes-buffer)
        (advice-add 'diff-hl-update :around
                    #'diff-hl-flydiff-update)
        (setq diff-hl-flydiff-timer
              (run-with-idle-timer diff-hl-flydiff-delay t #'diff-hl-update)))

    (advice-remove 'diff-hl-update #'diff-hl-flydiff-update)
    (advice-remove 'diff-hl-overlay-modified #'ignore)

    (advice-remove 'diff-hl-modified-p #'diff-hl-flydiff/modified-p)
    (advice-remove 'diff-hl-changes-buffer #'diff-hl-flydiff-changes-buffer)))

(provide 'diff-hl-flydiff)
