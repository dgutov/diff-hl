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

;; To enable in all Dired buffers, add this to your init file:
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;; Code:

(require 'diff-hl)

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

(defvar diff-hl-dired-process-buffer nil)

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
             ;; Process's finished, time to use the results.
             (unless (get-buffer-process diff-hl-dired-process-buffer)
               (diff-hl-dired-highlight-items (append dirs-alist
                                                      files-alist)))))
         )))))

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
                 (o (diff-hl-add-highlighting type 'single)))
            (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
            ))))))

(defalias 'diff-hl-dired-clear 'diff-hl-remove-overlays)

(provide 'diff-hl-dired)

;;; diff-hl-dired.el ends here
