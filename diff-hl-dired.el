;;; diff-hl-dired.el --- Highlight changed files in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2012-2014  Free Software Foundation, Inc.

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

;; To enable in all Dired buffers, add this to your init file:
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;; Code:

(require 'diff-hl)

(defvar diff-hl-dired-process-buffer nil)

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
  '((default :inherit diff-header))
  "Face used to highlight unregistered files.")

(defface diff-hl-dired-ignored
  '((default :inherit shadow))
  "Face used to highlight unregistered files.")

;;;###autoload
(define-minor-mode diff-hl-dired-mode
  "Toggle VC diff highlighting on the side of a Dired window."
  :lighter ""
  (if diff-hl-dired-mode
      (progn
        (diff-hl-maybe-define-bitmaps)
        (set (make-local-variable 'diff-hl-dired-process-buffer) nil)
        (add-hook 'dired-after-readin-hook 'diff-hl-dired-update nil t))
    (remove-hook 'dired-after-readin-hook 'diff-hl-dired-update t)
    (diff-hl-dired-clear)))

(defun diff-hl-dired-update ()
  "Highlight the Dired buffer."
  (let ((backend (ignore-errors (vc-responsible-backend default-directory)))
        (def-dir default-directory)
        (buffer (current-buffer))
        dirs-alist files-alist)
    (when backend
      (diff-hl-dired-clear)
      (if (buffer-live-p diff-hl-dired-process-buffer)
          (let ((proc (get-buffer-process diff-hl-dired-process-buffer)))
            (when proc (kill-process proc)))
        (setq diff-hl-dired-process-buffer
              (generate-new-buffer " *diff-hl-dired* tmp status")))
      (with-current-buffer diff-hl-dired-process-buffer
        (setq default-directory (expand-file-name def-dir))
        (erase-buffer)
        (vc-call-backend
         backend 'dir-status def-dir
         (lambda (entries &optional more-to-come)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (dolist (entry entries)
                 (cl-destructuring-bind (file state &rest) entry
                   (let ((type (plist-get
                                '(edited change added insert removed delete
                                         unregistered unknown)
                                state)))
                     (if (string-match "\\`\\([^/]+\\)/" file)
                         (let* ((dir (match-string 1 file))
                                (value (cdr (assoc dir dirs-alist))))
                           (unless (eq value type)
                             (if (null value)
                                 (push (cons dir type) dirs-alist)
                               (setcdr (assoc dir dirs-alist) 'change))))
                       (push (cons file type) files-alist)))))
               (unless more-to-come
                 (diff-hl-dired-highlight-items (append dirs-alist
                                                        files-alist))
                 (diff-hl-dired-update-ignores backend def-dir)))))
         )))))

(defun diff-hl-dired-update-ignores (backend def-dir)
  (let ((buffer (current-buffer))
        entries-alist)
    (with-current-buffer diff-hl-dired-process-buffer
      (erase-buffer)
      (vc-call-backend
       backend 'dir-status-files def-dir
       (cl-loop for file in (directory-files def-dir)
                unless (member file '("." ".."))
                collect file)
       nil
       (lambda (entries &optional more-to-come)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (dolist (entry entries)
               (cl-destructuring-bind (file state &rest) entry
                 ;; Work around http://debbugs.gnu.org/18605
                 (setq file (replace-regexp-in-string "\\` " "" file))
                 (when (eq state 'ignored)
                   (push (cons (directory-file-name file)
                               'ignored) entries-alist))))
             (unless more-to-come
               (diff-hl-dired-highlight-items entries-alist)))))
       ))))

(defun diff-hl-dired-highlight-items (alist)
  "Highlight ALIST containing (FILE . TYPE) elements."
  (dolist (pair alist)
    (let ((file (car pair))
          (type (cdr pair)))
      (save-excursion
        (goto-char (point-min))
        (when (and type (dired-goto-file-1
                         file (expand-file-name file) nil))
          (let* ((diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
                 (diff-hl-fringe-face-function 'diff-hl-dired-face-from-type)
                 (o (diff-hl-add-highlighting type 'single)))
            (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
            ))))))

(defun diff-hl-dired-face-from-type (type _pos)
  (intern (format "diff-hl-dired-%s" type)))

(defalias 'diff-hl-dired-clear 'diff-hl-remove-overlays)

(provide 'diff-hl-dired)

;;; diff-hl-dired.el ends here
