;;; diff-hl-dired.el --- Highlight changed files in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017, 2023-2026  Free Software Foundation, Inc.

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
  "Face used to highlight unregistered files.")

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
        (add-hook 'dired-after-readin-hook 'diff-hl-dired-update 10 t))
    (remove-hook 'dired-after-readin-hook 'diff-hl-dired-update t)
    (diff-hl-dired-clear)))

(defun diff-hl-dired-update ()
  "Highlight the Dired buffer."
  (let ((backend (ignore-errors (vc-responsible-backend default-directory)))
        (def-dir default-directory)
        (buffer (current-buffer))
        dirs-alist files-alist)
    (when (and backend (not (memq backend diff-hl-dired-ignored-backends)))
      (if (buffer-live-p diff-hl-dired-process-buffer)
          (let ((proc (get-buffer-process diff-hl-dired-process-buffer)))
            (when proc (kill-process proc)))
        (setq diff-hl-dired-process-buffer
              (generate-new-buffer " *diff-hl-dired* tmp status")))
      (with-current-buffer diff-hl-dired-process-buffer
        (setq default-directory (expand-file-name def-dir))
        (erase-buffer)
        (diff-hl-dired-status-files
         backend def-dir
         (when diff-hl-dired-extra-indicators
           (with-current-buffer buffer
             (diff-hl-dired-nondirectory-files)))
         (lambda (entries &optional more-to-come)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (dolist (entry entries)
                 (cl-destructuring-bind (file state &rest r) entry
                   (unless (eq state 'up-to-date)
                     (let ((type (plist-get '( edited change added insert removed delete
                                               unregistered unknown ignored ignored)
                                            state))
                           (dirs (cl-loop with pos = 0
                                          while (string-match "/" file pos)
                                          do (setq pos (match-end 0))
                                          collect (substring file 0 (1- pos)))))
                       (dolist (dir dirs)
                         (let ((value (cdr (assoc dir dirs-alist))))
                           (unless (eq value type)
                             (cond
                              ((null value)
                               (push (cons dir type) dirs-alist))
                              ((not (eq type 'ignored))
                               (setcdr (assoc dir dirs-alist) 'change))))))
                       (push (cons file type) files-alist)))))
               (unless more-to-come
                 (diff-hl-dired-highlight-items
                  (append dirs-alist files-alist))))
             (unless more-to-come
               (kill-buffer diff-hl-dired-process-buffer))))
         )))))

(defun diff-hl-dired-status-files (backend dir files update-function)
  "Using VC BACKEND, fetch list of (FILE STATE EXTRA) entries for DIR.
Call UPDATE-FUNCTION as entries are added."
  (vc-call-backend
   backend 'dir-status-files
   dir nil
   (lambda (entries &optional more-to-come)
     (if (or more-to-come
             (not diff-hl-dired-extra-indicators))
         (funcall update-function entries more-to-come)
       (diff-hl-dir-status-ignored-files
        backend
        dir
        files
        (lambda (ignored-entries &optional more-to-come)
          (funcall update-function ignored-entries t)
          (unless more-to-come
            (funcall update-function entries nil))))
       ))))

(defun diff-hl-dired-nondirectory-files ()
  (cl-mapcan
   (lambda (entry)
     (let* ((dir (file-relative-name (car entry)))
            (all (file-name-all-completions "" dir))
            res)
       (dolist (file all)
         (unless (directory-name-p file)
           (push
            (if (equal dir "./")
                file
              (concat dir file))
            res)))
       res))
   dired-subdir-alist))

(defun diff-hl-dir-status-ignored-files (backend dir files update-function)
  (cond
   ((eq backend 'Git)
    (vc-git-dir-status-goto-stage
     (make-vc-git-dir-status-state :stage 'ls-files-ignored
                                   :files files
                                   :update-function update-function)))
   ((eq backend 'Hg)
    (let ((default-directory dir))
      (apply #'vc-hg-command '(t nil) 'async files
             "status" "-i"
             (if (version<= "4.2" (vc-hg--program-version))
                 '("--config" "commands.status.relative=1")
               '("re:" "-I" "."))))
    (vc-run-delayed-success 0
      (vc-hg-after-dir-status update-function)))
   ;; No specialized solution for "list only ignored state", list all.
   ;; If the backend doesn't use several process calls (like Git), the
   ;; difference should be trivial.
   (t
    (vc-call-backend backend 'dir-status-files dir files
                     update-function))))

(defun diff-hl-dired-highlight-items (alist)
  "Highlight ALIST containing (FILE . TYPE) elements."
  (diff-hl-dired-clear) ;; clear overlays right before drawing to avoid flicker
  (dolist (pair alist)
    (let ((file (car pair))
          (type (cdr pair)))
      (save-excursion
        (goto-char (point-min))
        (when (and type (dired-goto-file-1
                         (file-name-nondirectory file) (expand-file-name file) nil))
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
