;;; diff-hl-dired.el --- Highlight changed files in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017, 2023  Free Software Foundation, Inc.

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

;; To enable in all Dired buffers, add this to your init file:
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;
;; or
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
;;
;; to do it only in local Dired buffers.

;;; Code:

(require 'diff-hl)
(require 'dired)
(require 'vc-hooks)

(defvar diff-hl-dired-process-buffer nil)

(defvar diff-hl-dired--pending nil
  "Non-nil if a `diff-hl-dired-update' is pending due to concurrent VC process.")

(defgroup diff-hl-dired nil
  "VC diff highlighting on the side of a Dired window."
  :group 'diff-hl)

(defface diff-hl-dired-insert
  '((default :inherit diff-hl-insert))
  "Face used to highlight added files.")

(defface diff-hl-dired-delete
  '((default :inherit diff-hl-delete))
  "Face used to highlight directories with deleted files.")

(defface diff-hl-dired-change
  '((default :inherit diff-hl-change))
  "Face used to highlight changed files.")

(defface diff-hl-dired-unknown
  '((default :inherit dired-ignored))
  "Face used to highlight unregistered files.")

(defface diff-hl-dired-ignored
  '((default :inherit dired-ignored))
  "Face used to highlight ignored files.")

(defcustom diff-hl-dired-extra-indicators t
  "Non-nil to indicate ignored files."
  :type 'boolean)

(defcustom diff-hl-dired-ignored-backends '(RCS)
  "VC backends to ignore.
The directories registered to one of these backends won't have
status indicators."
  :type `(repeat (choice ,@(mapcar
                            (lambda (name)
                              `(const :tag ,(symbol-name name) ,name))
                            vc-handled-backends))))

(defcustom diff-hl-dired-fringe-bmp-function 'diff-hl-fringe-bmp-from-type
  "Function to determine fringe bitmap from change type and position."
  :type 'function)

;;;###autoload
(define-minor-mode diff-hl-dired-mode
  "Toggle VC diff highlighting on the side of a Dired window."
  :lighter ""
  (if diff-hl-dired-mode
      (progn
        (diff-hl-maybe-define-bitmaps)
        (set (make-local-variable 'diff-hl-dired-process-buffer) nil)
        (set (make-local-variable 'diff-hl-dired--pending) nil)
        (add-hook 'dired-after-readin-hook 'diff-hl-dired-update nil t))
    (remove-hook 'dired-after-readin-hook 'diff-hl-dired-update t)
    (diff-hl-dired-clear)))

(defun diff-hl-dired-update ()
  "Highlight the Dired buffer."
  (let ((backend (ignore-errors (vc-responsible-backend default-directory)))
        (def-dir default-directory)
        (buffer (current-buffer))
        (state-to-type '( edited change
                          added insert
                          removed delete
                          unregistered unknown
                          ignored ignored))
        dirs-alist files-alist)
    (when (and backend (not (memq backend diff-hl-dired-ignored-backends)))
      ;; queue update if VC process running
      ;; pending flag debounces: N calls -> 1 retry
      ;; e.g., open N subtrees while VC runs -> 1 retry after completion
      (if (and (buffer-live-p diff-hl-dired-process-buffer)
               (get-buffer-process diff-hl-dired-process-buffer)
               (process-live-p (get-buffer-process diff-hl-dired-process-buffer)))
          (setq diff-hl-dired--pending t) ;; queued, early return
        (setq diff-hl-dired--pending nil)
        (unless (buffer-live-p diff-hl-dired-process-buffer)
          (setq diff-hl-dired-process-buffer
                (generate-new-buffer " *diff-hl-dired* tmp status")))
        (with-current-buffer diff-hl-dired-process-buffer
          (setq default-directory (expand-file-name def-dir))
          (erase-buffer)
          (let ((files
                 (when diff-hl-dired-extra-indicators
                   (cl-loop for file in (directory-files def-dir)
                            unless (member file '("." ".." ".hg"))
                            collect file)))
                (update-fn
                 (lambda (entries &optional more-to-come)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (dolist (entry entries)
                         (cl-destructuring-bind (file state &rest r) entry
                           ;; Work around http://debbugs.gnu.org/18605
                           (setq file (replace-regexp-in-string "\\` " "" file))
                           (let ((type (plist-get state-to-type state))
                                 (dirs (cl-loop with pos = 0
                                                while (string-match "/" file pos)
                                                do (setq pos (match-end 0))
                                                collect (substring file 0 (1- pos)))))
                             (dolist (dir dirs)
                               (let ((value (cdr (assoc dir dirs-alist))))
                                 (cond
                                  ((eq value type)) ;; skip
                                  ((eq state 'up-to-date)) ;; skip
                                  ((null value)
                                   (push (cons dir type) dirs-alist))
                                  ((not (eq type 'ignored))
                                   (setcdr (assoc dir dirs-alist) 'change)))))
                             (push (cons file type) files-alist)
                             )))
                       (unless more-to-come
                         (diff-hl-dired-highlight-items
                          (append dirs-alist files-alist))
                         (when diff-hl-dired--pending
                           (run-at-time 0 nil
                                        (lambda ()
                                          (when (buffer-live-p buffer)
                                            (with-current-buffer buffer
                                              (setq diff-hl-dired--pending nil)
                                              (diff-hl-dired-update))))))))
                     (unless more-to-come
                       (kill-buffer diff-hl-dired-process-buffer))))))
            (diff-hl-dired-status-files backend def-dir files update-fn)))
        ))))

(defun diff-hl-dired-status-files (backend dir files update-function)
  "Using version control BACKEND, return list of (FILE STATE EXTRA) entries
for DIR containing FILES. Call UPDATE-FUNCTION as entries are added."
  (vc-call-backend backend 'dir-status-files
                   dir files update-function))

(defun diff-hl-dired-highlight-items (alist)
  "Highlight ALIST containing (FILE . TYPE) elements."
  ;; clear overlays right before drawing to avoid flicker
  (diff-hl-dired-clear)
  (dolist (pair alist)
    (let ((file (car pair))
          (type (cdr pair)))
      (save-excursion
        (goto-char (point-min))
        (when (and type
                   (dired-goto-file-1
                    (file-name-nondirectory file) ;; basename
                    (expand-file-name file) nil))
          (let* ((diff-hl-fringe-bmp-function diff-hl-dired-fringe-bmp-function)
                 (diff-hl-fringe-face-function 'diff-hl-dired-face-from-type)
                 (o (diff-hl-add-highlighting type 'single)))
            (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
            (overlay-put o 'diff-hl-dired-type type)
            ))))))

(defun diff-hl-dired-face-from-type (type _pos)
  (intern (format "diff-hl-dired-%s" type)))

(defalias 'diff-hl-dired-clear 'diff-hl-remove-overlays)

;;;###autoload
(defun diff-hl-dired-mode-unless-remote ()
  (unless (file-remote-p default-directory)
    (diff-hl-dired-mode)))

(provide 'diff-hl-dired)

;;; diff-hl-dired.el ends here
