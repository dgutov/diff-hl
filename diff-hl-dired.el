;;; diff-hl-dired.el --- Highlight changed files in Dired -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: Raises "Stack overflow in equal" error in Emacs 24.3.50.2
;; (at least) when changing directory.
;;
;; To enable in all Dired buffers, add this to your init file:
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;; Code:

(require 'diff-hl)

(define-minor-mode diff-hl-dired-mode
  "Toggle VC diff fringe highlighting in a Dired buffer."
  :lighter ""
  (if diff-hl-dired-mode
      (progn
        (add-hook 'dired-after-readin-hook 'diff-hl-dired-update nil t)
        (diff-hl-dired-update))
    (remove-hook 'dired-after-readin-hook 'diff-hl-dired-update t)
    (diff-hl-dired-clear)))

(defvar diff-hl-dired-process-buffer nil)

(defun diff-hl-dired-update ()
  (let ((backend (cl-loop for file in (diff-hl-dired-files)
                          for backend = (vc-backend file)
                          thereis backend))
        (def-dir default-directory)
        (buffer (current-buffer)))
    (diff-hl-dired-clear)
    (unless (buffer-live-p diff-hl-dired-process-buffer)
      (setq diff-hl-dired-process-buffer
            (generate-new-buffer " *diff-hl-dired* tmp status")))
    (with-current-buffer diff-hl-dired-process-buffer
      (setq default-directory (expand-file-name def-dir))
      (erase-buffer)
      (vc-call-backend
       backend 'dir-status def-dir
       (lambda (entries &optional more-to-come)
         (with-current-buffer buffer
           (dolist (entry entries)
             (cl-destructuring-bind (file state &rest) entry
               (let ((type (plist-get
                            '(edited change added insert removed delete)
                            state)))
                 (save-excursion
                   (goto-char (point-min))
                   (when (and type (dired-goto-file-1 file
                                                      (expand-file-name file)
                                                      (point-max)))
                     (diff-hl-add-highlighting type 'middle))))))))))))

(defalias 'diff-hl-dired-clear 'diff-hl-remove-overlays)

(defun diff-hl-dired-files ()
  (cl-loop for file in (directory-files default-directory)
           when (and (file-exists-p file)
                     (not (file-directory-p file)))
           collect (expand-file-name file)))

(provide 'diff-hl-dired)
