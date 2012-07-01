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
  (require 'vc-hg)
  (require 'face-remap))

(defgroup diff-hl nil
  "VC diff fringe highlighting"
  :group 'vc)

(defface diff-hl-insert
  '((t :inherit diff-added))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

(defface diff-hl-delete
  '((t :inherit diff-removed))
  "Face used to highlight deleted lines."
  :group 'diff-hl)

(defface diff-hl-change
  '((default
     :foreground "blue")
    (((class color) (min-colors 88) (background light))
     :background "#ddddff")
    (((class color) (min-colors 88) (background dark))
     :background "#333355"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

(defcustom diff-hl-draw-borders t
  "Non-nil to draw borders around fringe indicators."
  :group 'diff-hl
  :type 'boolean)

(defun diff-hl-define-bitmaps ()
  (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                         (plusp text-scale-mode-amount))
                    (expt text-scale-mode-step text-scale-mode-amount)
                  1))
         (h (round (* (frame-char-height) scale)))
         (w (frame-parameter nil 'left-fringe))
         (middle (make-vector h (expt 2 (1- w))))
         (ones (1- (expt 2 w)))
         (top (copy-sequence middle))
         (bottom (copy-sequence middle))
         (single (copy-sequence middle)))
    (aset top 0 ones)
    (aset bottom (1- h) ones)
    (aset single 0 ones)
    (aset single (1- h) ones)
    (define-fringe-bitmap 'diff-hl-bmp-top top h w 'top)
    (define-fringe-bitmap 'diff-hl-bmp-middle middle h w 'center)
    (define-fringe-bitmap 'diff-hl-bmp-bottom bottom h w 'bottom)
    (define-fringe-bitmap 'diff-hl-bmp-single single h w 'top)))

(when (window-system)
  (define-fringe-bitmap 'diff-hl-bmp-empty [0] 1 1 'center)
  (diff-hl-define-bitmaps))

(defvar diff-hl-spec-cache (make-hash-table :test 'equal))

(defun diff-hl-fringe-spec (type pos)
  (let* ((key (cons type pos))
         (val (gethash key diff-hl-spec-cache)))
    (unless val
      (let* ((face-sym (intern (concat "diff-hl-" (symbol-name type))))
             (bmp-sym (intern (concat "diff-hl-bmp-" (symbol-name pos)))))
        (setq val (propertize " " 'display `((left-fringe ,bmp-sym ,face-sym))))
        (puthash key val diff-hl-spec-cache)))
    val))

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

(defun diff-hl-update ()
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (diff-hl-remove-overlays)
    (save-excursion
      (goto-char (point-min))
      (dolist (c changes)
        (destructuring-bind (line len type) c
          (when (eq type 'delete)
            (setq len 1)
            (incf line))
          (forward-line (- line current-line))
          (setq current-line line)
          (let ((hunk-beg (point)))
            (while (plusp len)
              (let ((o (make-overlay (point) (line-end-position))))
                (overlay-put o 'diff-hl t)
                (overlay-put o 'before-string
                             (diff-hl-fringe-spec
                              type
                              (cond
                               ((not diff-hl-draw-borders) 'empty)
                               ((and (= len 1) (= line current-line)) 'single)
                               ((= len 1) 'bottom)
                               ((= line current-line) 'top)
                               (t 'middle)))))
              (forward-line 1)
              (incf current-line)
              (decf len))
            (let ((h (make-overlay hunk-beg (point)))
                  (hook '(diff-hl-overlay-modified)))
              (overlay-put h 'diff-hl t)
              (overlay-put h 'modification-hooks hook)
              (overlay-put h 'insert-in-front-hooks hook))))))))

(defun diff-hl-remove-overlays ()
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'diff-hl) (delete-overlay o))))

(defun diff-hl-overlay-modified (ov after-p _beg _end &optional _length)
  ;; Delete the overlay and all our overlays inside it.
  (when after-p
    (save-restriction
      (narrow-to-region (overlay-start ov) (overlay-end ov))
      (diff-hl-remove-overlays))
    (delete-overlay ov)))

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

(defun diff-hl-diff-goto-hunk ()
  "Open diff buffer and skip to the line corresponding to current."
  (interactive)
  (vc-buffer-sync)
  (let* ((line (line-number-at-pos)))
    (vc-diff-internal t (vc-deduce-fileset) nil nil t)
    (vc-exec-after `(diff-hl-diff-skip-to ,line))))

(defun diff-hl-diff-skip-to (line)
  (unless (eobp)
    (diff-hunk-next)
    (let (found)
      (while (and (looking-at diff-hunk-header-re-unified) (not found))
        (let ((hunk-line (string-to-number (match-string 3)))
              (len (let ((m (match-string 4)))
                     (if m (string-to-number m) 1))))
          (if (> line (+ hunk-line len))
              (diff-hunk-next)
            (setq found t)
            (let ((to-go (1+ (- line hunk-line))))
              (while (plusp to-go)
                (forward-line 1)
                (unless (looking-at "^-")
                  (decf to-go))))))))))

;;;###autoload
(define-minor-mode diff-hl-mode
  "Toggle display of VC diff indicators in the left fringe."
  :lighter "" :keymap '(([remap vc-diff] . diff-hl-diff-goto-hunk))
  (if diff-hl-mode
      (progn
        (add-hook 'after-save-hook 'diff-hl-update nil t)
        (add-hook 'after-change-functions 'diff-hl-edit nil t)
        (if vc-mode
            (diff-hl-update)
          (add-hook 'find-file-hook 'diff-hl-update t t))
        (add-hook 'vc-checkin-hook 'diff-hl-update nil t)
        (add-hook 'text-scale-mode-hook 'diff-hl-define-bitmaps nil t))
    (remove-hook 'after-save-hook 'diff-hl-update t)
    (remove-hook 'after-change-functions 'diff-hl-edit t)
    (remove-hook 'find-file-hook 'diff-hl-update t)
    (remove-hook 'vc-checkin-hook 'diff-hl-update t)
    (remove-hook 'text-scale-mode-hook 'diff-hl-define-bitmaps t)
    (diff-hl-remove-overlays)))

(defun turn-on-diff-hl-mode ()
  ;; FIXME: Why is this called twice for each buffer?
  ;; Isn't fundamental-mode supposed to not run any hooks?
  (and buffer-file-name (not (eq major-mode (default-value 'major-mode)))
       (window-system) ;; No fringes in the console.
       (diff-hl-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-diff-hl-mode diff-hl-mode
  turn-on-diff-hl-mode)

(provide 'diff-hl)
