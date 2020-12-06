;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode enables diffing on-the-fly (i.e. without saving the buffer first)
;; Toggle in all buffers with M-x diff-hl-flydiff-mode

;;; Code:

(require 'diff-hl)
(require 'diff)
(unless (require 'nadvice nil t)
  (error "`diff-hl-flydiff-mode' requires Emacs 24.4 or newer"))

(defgroup diff-hl-flydiff nil
  "Highlight changes on the fly"
  :group 'diff-hl)

(defcustom diff-hl-flydiff-delay 0.3
  "The idle delay in seconds before highlighting is updated."
  :type 'number)

(defvar diff-hl-flydiff-modified-tick nil)
(defvar diff-hl-flydiff-timer nil)
(make-variable-buffer-local 'diff-hl-flydiff-modified-tick)

(defun diff-hl-flydiff/vc-git--symbolic-ref (file)
  (or
   (vc-file-getprop file 'vc-git-symbolic-ref)
   (let* (process-file-side-effects
          (str (vc-git--run-command-string nil "symbolic-ref" "HEAD")))
     (vc-file-setprop file 'vc-git-symbolic-ref
                      (if str
                          (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
                              (match-string 2 str)
                            str))))))

(defun diff-hl-flydiff/vc-git-working-revision (_file)
  "Git-specific version of `vc-working-revision'."
  (let (process-file-side-effects)
    (vc-git--rev-parse "HEAD")))

(defun diff-hl-flydiff/vc-git-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE."
  (let* ((rev (vc-working-revision file))
         (disp-rev (or (diff-hl-flydiff/vc-git--symbolic-ref file)
                       (substring rev 0 7)))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml))
         (face   (get-text-property 0 'face def-ml)))
    (propertize (replace-regexp-in-string (concat rev "\\'") disp-rev def-ml t t)
                'face face
                'help-echo (concat help-echo "\nCurrent revision: " rev))))

;; Polyfill concrete revisions for vc-git-working-revision in Emacs 24.4, 24.5
(when (version<= emacs-version "25.0")
  (with-eval-after-load 'vc-git
    (advice-add 'vc-git-working-revision :override
                #'diff-hl-flydiff/vc-git-working-revision)
    (advice-add 'vc-git-mode-line-string :override
                #'diff-hl-flydiff/vc-git-mode-line-string)))

(defun diff-hl-flydiff-buffer-with-head (file &optional _backend)
  "View the differences between FILE and its associated file in HEAD revision.
This requires the external program `diff' to be in your
`exec-path'."
  (diff-hl-diff-buffer-with-head file " *diff-hl-diff*"))

(defun diff-hl-flydiff-update ()
  (unless (or
           (not diff-hl-mode)
           (eq diff-hl-flydiff-modified-tick (buffer-chars-modified-tick))
           (not buffer-file-name)
           (not (file-exists-p buffer-file-name))
           (file-remote-p default-directory))
    (diff-hl-update)))

(defun diff-hl-flydiff/modified-p (state)
  (buffer-modified-p))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Perform highlighting on-the-fly.
This is a global minor mode.  It alters how `diff-hl-mode' works."
  :lighter "" :global t
  (if diff-hl-flydiff-mode
      (progn
        (advice-add 'diff-hl-overlay-modified :override #'ignore)

        (advice-add 'diff-hl-modified-p :before-until
                    #'diff-hl-flydiff/modified-p)
        (advice-add 'diff-hl-changes-buffer :override
                    #'diff-hl-flydiff-buffer-with-head)
        (setq diff-hl-flydiff-timer
              (run-with-idle-timer diff-hl-flydiff-delay t #'diff-hl-flydiff-update)))

    (advice-remove 'diff-hl-overlay-modified #'ignore)

    (advice-remove 'diff-hl-modified-p #'diff-hl-flydiff/modified-p)
    (advice-remove 'diff-hl-changes-buffer #'diff-hl-flydiff-buffer-with-head)

    (and diff-hl-flydiff-timer
         (cancel-timer diff-hl-flydiff-timer))))

(provide 'diff-hl-flydiff)
