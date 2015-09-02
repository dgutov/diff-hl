;; Copyright (C) 2012-2013  Free Software Foundation, Inc.

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
(require 'nadvice)

(defvar diff-hl-flydiff-modified-tick 0)
(defvar diff-hl-flydiff-timer)
(make-variable-buffer-local 'diff-hl-flydiff-modified-tick)

;; Polyfill concrete revisions for vc-git-working-revision in Emacs 24.4, 24.5
(when (version<= emacs-version "25.0")
  (with-eval-after-load 'vc-git
    (defun vc-git--symbolic-ref (file)
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
              (disp-rev (or (vc-git--symbolic-ref file)
                          (substring rev 0 7)))
              (def-ml (vc-default-mode-line-string 'Git file))
              (help-echo (get-text-property 0 'help-echo def-ml)))
        (propertize (replace-regexp-in-string (concat rev "\\'") disp-rev def-ml t t)
          'help-echo (concat help-echo "\nCurrent revision: " rev))))

    (advice-add 'vc-git-working-revision :override
      #'diff-hl-flydiff/vc-git-working-revision)
    (advice-add 'vc-git-mode-line-string :override
      #'diff-hl-flydiff/vc-git-mode-line-string)))

(defun diff-hl-flydiff-make-temp-file-name (file rev &optional manual)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned."
  (let* ((auto-save-file-name-transforms
           `((".*" ,temporary-file-directory t))))
    (expand-file-name
      (concat (make-auto-save-file-name)
        ".~" (subst-char-in-string
               ?/ ?_ rev)
        (unless manual ".") "~")
      temporary-file-directory)))

(defun diff-hl-flydiff-create-revision (file revision)
  "Read REVISION of FILE into a buffer and return the buffer."
  (let ((automatic-backup (diff-hl-flydiff-make-temp-file-name file revision))
         (filebuf (get-file-buffer file))
         (filename (diff-hl-flydiff-make-temp-file-name file revision 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
        (rename-file automatic-backup filename nil)
        (with-current-buffer filebuf
          (let ((failed t)
                 (coding-system-for-read 'no-conversion)
                 (coding-system-for-write 'no-conversion))
            (unwind-protect
              (with-temp-file filename
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to get local value of
                  ;; vc-checkout-switches.
                  (with-current-buffer filebuf
                    (vc-call find-revision file revision outbuf))))
              (setq failed nil)
              (when (and failed (file-exists-p filename))
                (delete-file filename)))))))
    filename))

(defun diff-hl-flydiff-buffer-with-head ()
  "View the differences between BUFFER and its associated file.
This requires the external program `diff' to be in your `exec-path'."
  (interactive)
  (vc-ensure-vc-buffer)
  (with-current-buffer (get-buffer (current-buffer))
    (let ((rev (diff-hl-flydiff-create-revision
                 buffer-file-name
                 (vc-working-revision buffer-file-name
                   (vc-responsible-backend buffer-file-name))))
           (temporary-file-directory
             (if (file-directory-p "/dev/shm/")
               "/dev/shm/"
               temporary-file-directory)))
      (diff-no-select rev (current-buffer) "-U 0" 'noasync
        (get-buffer-create " *diff-hl-diff*")))))


(defun diff-hl-flydiff/update (old-fun &optional auto)
  (unless (and auto
            (or
              (= diff-hl-flydiff-modified-tick (buffer-modified-tick))
              (file-remote-p default-directory)
              (not (buffer-modified-p))))
    (funcall old-fun)))

(defun diff-hl-flydiff/changes (&rest args)
  (let* ((file buffer-file-name)
          (backend (vc-backend file)))
    (when backend
      (let ((state (vc-state file backend)))
        (cond
          ((or
             (buffer-modified-p)
             (eq state 'edited)
             (and (eq state 'up-to-date)
               ;; VC state is stale in after-revert-hook.
               (or revert-buffer-in-progress-p
                 ;; Diffing against an older revision.
                 diff-hl-reference-revision)))
            (let (diff-auto-refine-mode res)
              (with-current-buffer (diff-hl-flydiff-buffer-with-head)
                (goto-char (point-min))
                (unless (eobp)
                  (ignore-errors
                    (diff-beginning-of-hunk t))
                  (while (looking-at diff-hunk-header-re-unified)
                    (let ((line (string-to-number (match-string 3)))
                           (len (let ((m (match-string 4)))
                                  (if m (string-to-number m) 1)))
                           (beg (point)))
                      (diff-end-of-hunk)
                      (let* ((inserts (diff-count-matches "^\\+" beg (point)))
                              (deletes (diff-count-matches "^-" beg (point)))
                              (type (cond ((zerop deletes) 'insert)
                                      ((zerop inserts) 'delete)
                                      (t 'change))))
                        (when (eq type 'delete)
                          (setq len 1)
                          (cl-incf line))
                        (push (list line len type) res))))))
              (setq diff-hl-flydiff-modified-tick (buffer-modified-tick))
              (nreverse res)))
          ((eq state 'added)
            `((1 ,(line-number-at-pos (point-max)) insert)))
          ((eq state 'removed)
            `((1 ,(line-number-at-pos (point-max)) delete))))))))

;;;###autoload
(define-minor-mode diff-hl-flydiff-mode
  "Highlight diffs on-the-fly"
  :lighter ""
  :global t
  (if diff-hl-flydiff-mode
    (progn
      (advice-add 'diff-hl-update :around #'diff-hl-flydiff/update)
      (advice-add 'diff-hl-changes :override #'diff-hl-flydiff/changes)
      (advice-add 'diff-hl-overlay-modified :override #'ignored)

      (remove-hook 'after-change-functions #'diff-hl-edit t)
      (setq diff-hl-flydiff-timer
        (run-with-idle-timer 0.3 t #'diff-hl-update t)))

    (advice-remove 'diff-hl-update #'diff-hl-flydiff/update)
    (advice-remove 'diff-hl-changes #'diff-hl-flydiff/changes)
    (advice-remove 'diff-hl-overlay-modified #'ignored)

    (cancel-timer diff-hl-flydiff-timer)
    (when diff-hl-mode
      (add-hook 'after-change-functions 'diff-hl-edit nil t))))

(provide 'diff-hl-flydiff)
