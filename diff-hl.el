;;; diff-hl.el --- Highlight uncommitted changes -*- lexical-binding: t -*-

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; URL:      https://github.com/dgutov/diff-hl
;; Keywords: vc, diff
;; Version:  1.4.0
;; Package-Requires: ((cl-lib "0.2"))

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

;; `diff-hl-mode' highlights uncommitted changes on the left fringe of the
;; window, allows you to jump between the hunks and revert them selectively.

;; Provided commands:
;;
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
;;
;; The mode takes advantage of `smartrep' if it is installed.

;; Add either of the following to your init file.
;;
;; To use it in all buffers:
;;
;; (global-diff-hl-mode)
;;
;; Only in `prog-mode' buffers, with `vc-dir' integration:
;;
;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;; (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;;; Code:

(require 'diff-mode)
(require 'vc)
(require 'vc-dir)
(eval-when-compile
  (require 'cl-lib)
  (require 'vc-git)
  (require 'vc-hg)
  (require 'face-remap))

(defgroup diff-hl nil
  "VC diff fringe highlighting"
  :group 'vc)

(defface diff-hl-insert
  '((default :inherit diff-added)
    (((class color)) :foreground "green4"))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

(defface diff-hl-delete
  '((default :inherit diff-removed)
    (((class color)) :foreground "red3"))
  "Face used to highlight deleted lines."
  :group 'diff-hl)

(defface diff-hl-change
  '((default :foreground "blue3")
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
                         (cl-plusp text-scale-mode-amount))
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

(defmacro diff-hl-with-diff-switches (body)
  `(let ((vc-git-diff-switches nil)
         (vc-hg-diff-switches nil)
         (vc-diff-switches '("-U0"))
         (vc-disable-async-diff t))
     ,body))

(defun diff-hl-changes ()
  (let* ((file buffer-file-name)
         (backend (vc-backend file)))
    (when backend
      (cl-case (vc-state file backend)
        (edited
         (let* ((buf-name " *diff-hl* ")
                res)
           (diff-hl-with-diff-switches
            (vc-call-backend backend 'diff (list file) nil nil buf-name))
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
                     (when (eq type 'delete)
                       (setq len 1)
                       (cl-incf line))
                     (push (list line len type) res))))))
           (nreverse res)))
        (added
         `((1 ,(line-number-at-pos (point-max)) insert)))
        (removed
         `((1 ,(line-number-at-pos (point-max)) delete)))))))

(defun diff-hl-update ()
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (diff-hl-remove-overlays)
    (save-excursion
      (goto-char (point-min))
      (dolist (c changes)
        (cl-destructuring-bind (line len type) c
          (forward-line (- line current-line))
          (let ((hunk-beg (point)))
            (forward-line len)
            (setq current-line (+ line len))
            (let ((h (make-overlay hunk-beg (1- (point))))
                  (hook '(diff-hl-overlay-modified)))
              (overlay-put h 'diff-hl t)
              (if (= len 1)
                  (overlay-put h 'before-string (diff-hl-fringe-spec type 'single))
                (overlay-put h 'before-string (diff-hl-fringe-spec type 'top))
                (overlay-put h 'line-prefix (diff-hl-fringe-spec type 'middle))
                (overlay-put h 'after-string (diff-hl-fringe-spec type 'bottom)))
              (overlay-put h 'modification-hooks hook)
              (overlay-put h 'insert-in-front-hooks hook)
              (overlay-put h 'insert-behind-hooks hook))))))))

(defun diff-hl-remove-overlays ()
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'diff-hl) (delete-overlay o))))

(defun diff-hl-overlay-modified (ov after-p _beg _end &optional _length)
  "Delete the overlay."
  (unless after-p
    (when (overlay-buffer ov)
      (delete-overlay ov))))

(defvar diff-hl-timer nil)

(defun diff-hl-edit (_beg _end _len)
  "DTRT when we've `undo'-ne the buffer into unmodified state."
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
  "Run VC diff command and go to the line corresponding to the current."
  (interactive)
  (vc-buffer-sync)
  (let* ((line (line-number-at-pos))
         (buffer (current-buffer)))
    (vc-diff-internal t (vc-deduce-fileset) nil nil t)
    (vc-exec-after `(if (< (line-number-at-pos (point-max)) 3)
                        (with-current-buffer ,buffer (diff-hl-remove-overlays))
                      (diff-hl-diff-skip-to ,line)
                      (setq vc-sentinel-movepoint (point))))))

(defun diff-hl-diff-skip-to (line)
  "In `diff-mode', skip to the hunk and line corresponding to LINE
in the source file, or the last line of the hunk above it."
  (diff-hunk-next)
  (let (found)
    (while (and (looking-at diff-hunk-header-re-unified) (not found))
      (let ((hunk-line (string-to-number (match-string 3)))
            (len (let ((m (match-string 4)))
                   (if m (string-to-number m) 1))))
        (if (> line (+ hunk-line len))
            (diff-hunk-next)
          (setq found t)
          (if (< line hunk-line)
              ;; Retreat to the previous hunk.
              (forward-line -1)
            (let ((to-go (1+ (- line hunk-line))))
              (while (cl-plusp to-go)
                (forward-line 1)
                (unless (looking-at "^-")
                  (cl-decf to-go))))))))))

(defun diff-hl-revert-hunk ()
  "Revert the diff hunk with changes at or above the point."
  (interactive)
  (vc-buffer-sync)
  (let ((diff-buffer (generate-new-buffer-name "*diff-hl*"))
        (buffer (current-buffer))
        (line (save-excursion
                (unless (diff-hl-hunk-overlay-at (point))
                  (diff-hl-previous-hunk))
                (line-number-at-pos)))
        (fileset (vc-deduce-fileset)))
    (unwind-protect
        (progn
          (vc-diff-internal nil fileset nil nil nil diff-buffer)
          (vc-exec-after
           `(let (beg-line end-line)
              (when (eobp)
                (with-current-buffer ,buffer (diff-hl-remove-overlays))
                (error "Buffer is up-to-date"))
              (diff-hl-diff-skip-to ,line)
              (save-excursion
                (while (looking-at "[-+]") (forward-line 1))
                (setq end-line (line-number-at-pos (point)))
                (unless (eobp) (diff-split-hunk)))
              (unless (looking-at "[-+]") (forward-line -1))
              (while (looking-at "[-+]") (forward-line -1))
              (setq beg-line (line-number-at-pos (point)))
              (unless (looking-at "@")
                (forward-line 1)
                (diff-split-hunk))
              (let ((wbh (window-body-height)))
                (if (>= wbh (- end-line beg-line))
                    (recenter (/ (+ wbh (- beg-line end-line) 2) 2))
                  (recenter 1)))
              (unless (yes-or-no-p (format "Revert current hunk in %s?"
                                           ,(cl-caadr fileset)))
                (error "Revert canceled"))
              (let ((diff-advance-after-apply-hunk nil))
                (diff-apply-hunk t))
              (with-current-buffer ,buffer
                (save-buffer))
              (message "Hunk reverted"))))
      (quit-windows-on diff-buffer))))

(defun diff-hl-hunk-overlay-at (pos)
  (cl-loop for o in (overlays-at pos)
           when (overlay-get o 'diff-hl)
           return o))

(defun diff-hl-next-hunk (&optional backward)
  "Go to the beginning of the next hunk in the current buffer."
  (interactive)
  (let ((pos (save-excursion
               (catch 'found
                 (while (not (if backward (bobp) (eobp)))
                   (goto-char (if backward
                                  (previous-overlay-change (point))
                                (next-overlay-change (point))))
                   (let ((o (diff-hl-hunk-overlay-at (point))))
                     (when (and o (if backward
                                      (<= (overlay-end o) (1+ (point)))
                                    (>= (overlay-start o) (point))))
                       (throw 'found (overlay-start o)))))))))
    (if pos
        (goto-char pos)
      (error "No further hunks found"))))

(defun diff-hl-previous-hunk ()
  "Go to the beginning of the previous hunk in the current buffer."
  (interactive)
  (diff-hl-next-hunk t))

(define-minor-mode diff-hl-mode
  "Toggle VC diff fringe highlighting."
  :lighter "" :keymap `(([remap vc-diff] . diff-hl-diff-goto-hunk)
                        (,(kbd "C-x v n") . diff-hl-revert-hunk)
                        (,(kbd "C-x v [") . diff-hl-previous-hunk)
                        (,(kbd "C-x v ]") . diff-hl-next-hunk))
  (if diff-hl-mode
      (progn
        (add-hook 'after-save-hook 'diff-hl-update nil t)
        (add-hook 'after-change-functions 'diff-hl-edit nil t)
        (if vc-mode
            (diff-hl-update)
          (add-hook 'find-file-hook 'diff-hl-update t t))
        (add-hook 'vc-checkin-hook 'diff-hl-update nil t)
        (add-hook 'after-revert-hook 'diff-hl-update nil t)
        (add-hook 'text-scale-mode-hook 'diff-hl-define-bitmaps nil t))
    (remove-hook 'after-save-hook 'diff-hl-update t)
    (remove-hook 'after-change-functions 'diff-hl-edit t)
    (remove-hook 'find-file-hook 'diff-hl-update t)
    (remove-hook 'vc-checkin-hook 'diff-hl-update t)
    (remove-hook 'after-revert-hook 'diff-hl-update t)
    (remove-hook 'text-scale-mode-hook 'diff-hl-define-bitmaps t)
    (diff-hl-remove-overlays)))

(when (require 'smartrep nil t)
  (let (smart-keys)
    (cl-labels ((scan (map)
                      (map-keymap
                       (lambda (event binding)
                         (if (consp binding)
                             (scan binding)
                           (when (characterp event)
                             (push (cons (string event) binding) smart-keys))))
                       map)))
      (scan diff-hl-mode-map)
      (smartrep-define-key diff-hl-mode-map "C-x v" smart-keys))))

(defun diff-hl-dir-update ()
  (dolist (pair (if (vc-dir-marked-files)
                    (vc-dir-marked-only-files-and-states)
                  (vc-dir-child-files-and-states)))
    (when (eq 'up-to-date (cdr pair))
      (let ((buffer (find-buffer-visiting (car pair))))
        (when buffer
          (with-current-buffer buffer
            (diff-hl-remove-overlays)))))))

(define-minor-mode diff-hl-dir-mode
  "Toggle `diff-hl-mode' integration in a `vc-dir-mode' buffer."
  :lighter ""
  (if diff-hl-dir-mode
      (add-hook 'vc-checkin-hook 'diff-hl-dir-update t t)
    (remove-hook 'vc-checkin-hook 'diff-hl-dir-update t)))

;;;###autoload
(defun turn-on-diff-hl-mode ()
  "Turn on `diff-hl-mode' or `diff-hl-dir-mode' in a buffer if appropriate."
  (when (window-system) ;; No fringes in the console.
    (cond
     (buffer-file-name
      (diff-hl-mode 1))
     ((eq major-mode 'vc-dir-mode)
      (diff-hl-dir-mode 1)))))

;;;###autoload
(define-globalized-minor-mode global-diff-hl-mode diff-hl-mode
  turn-on-diff-hl-mode :after-hook (diff-hl-global-mode-change))

(defun diff-hl-global-mode-change ()
  (unless global-diff-hl-mode
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when diff-hl-dir-mode
          (diff-hl-dir-mode -1))))))

(provide 'diff-hl)

;;; diff-hl.el ends here
