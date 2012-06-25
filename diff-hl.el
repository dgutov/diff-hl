;;; diff-hl.el --- VC diff fringe highlighting -*- lexical-binding: t -*-

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; Keywords: vc, diff

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

(require 'diff-mode)
(require 'vc)
(eval-when-compile
  (require 'cl)
  (require 'vc-git)
  (require 'vc-hg))

(defface diff-hl-insert
  '((t :inherit diff-added))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

(defface diff-hl-delete
  '((t :inherit diff-removed))
  "Face used to highlight deleted lines."
  :group 'diff-hl)

(defface diff-hl-change
  '((((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "#333355"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

(define-fringe-bitmap 'diff-hl-empty [0] 1 1 'center)

(defun diff-hl-changes ()
  (let* ((buf-name " *vc-diff-hl* ")
         (vc-git-diff-switches nil)
         (vc-hg-diff-switches nil)
         (vc-diff-switches '("-U0"))
         (vc-disable-async-diff t)
         (file (buffer-file-name))
         (backend (vc-backend file))
         res)
    (when backend
      (vc-call-backend backend 'diff (list file) nil nil buf-name)
      (with-current-buffer buf-name
        (goto-char (point-min))
        (unless (eobp)
          (diff-beginning-of-hunk t)
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
                (push (list line len type) res)))))))
    (nreverse res)))

(eval-and-compile
  (dolist (type '(insert delete change))
    (let* ((type-str (symbol-name type))
           (spec-sym (intern (concat "diff-hl-" type-str "-spec")))
           (face-sym (intern (concat "diff-hl-" type-str))))
      (eval `(defconst ,spec-sym
               ,(propertize " " 'display
                            `((left-fringe diff-hl-empty ,face-sym))))))))

(defun diff-hl-update ()
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (o) (when (overlay-get o 'diff-hl) (delete-overlay o)))
            (overlays-in (point-min) (point-max)))
      (dolist (c changes)
        (destructuring-bind (line len type) c
          (when (eq type 'delete)
            (setq len 1)
            (incf line))
          (forward-line (- line current-line))
          (setq current-line line)
          (while (plusp len)
            (let ((o (make-overlay (point) (line-end-position))))
              (overlay-put o 'diff-hl t)
              (overlay-put o 'before-string
                           (case type
                             ('insert diff-hl-insert-spec)
                             ('delete diff-hl-delete-spec)
                             ('change diff-hl-change-spec)))
              (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
              (overlay-put o 'insert-in-front-hooks '(diff-hl-overlay-modified)))
            (forward-line 1)
            (incf current-line)
            (decf len)))))))

(defun diff-hl-overlay-modified (ov after-p _beg _end &optional _length)
  ;; Do the simplest possible thing, for now.
  (when after-p (delete-overlay ov)))

(defvar diff-hl-timer nil)

(defun diff-hl-edit (_beg _end _len)
  ;; DTRT when we've `undo'-ed the buffer into unmodified state.
  (when undo-in-progress
    (when diff-hl-timer
      (cancel-timer diff-hl-timer))
    (setq diff-hl-timer
          (run-with-idle-timer 0.01 nil #'diff-hl-after-undo (current-buffer)))))

(defun diff-hl-after-undo (buffer)
  (with-current-buffer buffer
    (unless (buffer-modified-p)
      (diff-hl-update))))

;;;###autoload
(define-minor-mode diff-hl-mode
  "Toggle display of VC diff indicators in the left fringe."
  :after-hook (diff-hl-update)
  (if diff-hl-mode
      (progn
        (add-hook 'after-save-hook 'diff-hl-update nil t)
        (add-hook 'after-change-functions 'diff-hl-edit nil t))
    (remove-hook 'after-save-hook 'diff-hl-update t)
    (remove-hook 'after-change-functions 'diff-hl-edit t)))

(defun turn-on-diff-hl-mode ()
  ;; FIXME: Why is this called twice for each buffer?
  ;; Isn't fundamental-mode supposed to not run any hooks?
  (when (and buffer-file-name (not (eq major-mode (default-value 'major-mode))))
    (diff-hl-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-diff-hl-mode diff-hl-mode
  turn-on-diff-hl-mode)

(provide 'diff-hl)
