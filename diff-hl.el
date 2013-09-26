;;; diff-hl.el --- Highlight uncommitted changes -*- lexical-binding: t -*-

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; URL:      https://github.com/dgutov/diff-hl
;; Keywords: vc, diff
;; Version:  1.5.0
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

;; `diff-hl-mode' highlights uncommitted changes on the left side of
;; the window (using the fringe, by default), allows you to jump
;; between the hunks and revert them selectively.

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
  "VC diff highlighting on the side of a window"
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

(defface diff-hl-unknown
  '((default :inherit diff-header))
  "Face used to highlight unregistered files.")

(defcustom diff-hl-draw-borders t
  "Non-nil to draw borders around fringe indicators."
  :group 'diff-hl
  :type 'boolean)

(defcustom diff-hl-highlight-function 'diff-hl-highlight-on-fringe
  "Function to highlight the current line. Its arguments are
  overlay, change type and position within a hunk."
  :group 'diff-hl
  :type 'function)

(defcustom diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-pos
  "Function to choose the fringe bitmap for a given change type
  and position within a hunk.  Should accept two arguments."
  :group 'diff-hl
  :type '(choice (const diff-hl-fringe-bmp-from-pos)
                 (const diff-hl-fringe-bmp-from-type)
                 function))

(defvar diff-hl-reference-revision nil
  "Revision to diff against.  nil means the most recent one.")

(defun diff-hl-define-bitmaps ()
  (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                         (numberp text-scale-mode-amount))
                    (expt text-scale-mode-step text-scale-mode-amount)
                  1))
         (spacing (or (default-value 'line-spacing) 0))
         (h (round (+ (* (frame-char-height) scale)
                      (if (floatp spacing)
                          (* (frame-char-height) spacing)
                        spacing))))
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
    (define-fringe-bitmap 'diff-hl-bmp-single single h w 'top)
    (let* ((w2 (* (/ w 2) 2))
           (delete-row (- (expt 2 (1- w2)) 2))
           (middle-pos (1- (/ w2 2)))
           (middle-bit (expt 2 middle-pos))
           (insert-bmp (make-vector w2 (* 3 middle-bit))))
      (define-fringe-bitmap 'diff-hl-bmp-delete (make-vector 2 delete-row) w2 w2)
      (aset insert-bmp 0 0)
      (aset insert-bmp middle-pos delete-row)
      (aset insert-bmp (1+ middle-pos) delete-row)
      (aset insert-bmp (1- w2) 0)
      (define-fringe-bitmap 'diff-hl-bmp-insert insert-bmp w2 w2)
      (define-fringe-bitmap 'diff-hl-bmp-change (make-vector
                                                 w2 (* 3 middle-bit)) w2 w2))))

(defun diff-hl-maybe-define-bitmaps ()
  (when (window-system) ;; No fringes in the console.
    (unless (fringe-bitmap-p 'diff-hl-bmp-empty)
      (diff-hl-define-bitmaps)
      (define-fringe-bitmap 'diff-hl-bmp-empty [0] 1 1 'center))))

(defvar diff-hl-spec-cache (make-hash-table :test 'equal))

(defun diff-hl-fringe-spec (type pos)
  (let* ((key (list type pos diff-hl-fringe-bmp-function))
         (val (gethash key diff-hl-spec-cache)))
    (unless val
      (let* ((face-sym (intern (format "diff-hl-%s" type)))
             (bmp-sym (funcall diff-hl-fringe-bmp-function type pos)))
        (setq val (propertize " " 'display `((left-fringe ,bmp-sym ,face-sym))))
        (puthash key val diff-hl-spec-cache)))
    val))

(defun diff-hl-fringe-bmp-from-pos (type pos)
  (intern (format "diff-hl-bmp-%s" pos)))

(defun diff-hl-fringe-bmp-from-type (type pos)
  (if (eq type 'unknown)
      'question-mark
    (intern (format "diff-hl-bmp-%s" type))))

(defmacro diff-hl-with-diff-switches (body)
  `(let ((vc-git-diff-switches nil)
         (vc-hg-diff-switches nil)
         (vc-svn-diff-switches nil)
         (vc-diff-switches '("-U0"))
         (vc-disable-async-diff t))
     ,body))

(defun diff-hl-changes ()
  (let* ((file buffer-file-name)
         (backend (vc-backend file)))
    (when backend
      (let ((state (vc-state file backend)))
        (cond
         ((or (eq state 'edited)
              (and (eq state 'up-to-date)
                   ;; VC state is stale in after-revert-hook.
                   (or revert-buffer-in-progress-p
                       ;; Diffing against an older revision.
                       diff-hl-reference-revision)))
          (let* ((buf-name " *diff-hl* ")
                 diff-auto-refine-mode
                 res)
            (diff-hl-with-diff-switches
             (vc-call-backend backend 'diff (list file)
                              diff-hl-reference-revision nil
                              buf-name))
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
         ((eq state 'added)
          `((1 ,(line-number-at-pos (point-max)) insert)))
         ((eq state 'removed)
          `((1 ,(line-number-at-pos (point-max)) delete))))))))

(defun diff-hl-update ()
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (diff-hl-remove-overlays)
    (save-excursion
      (goto-char (point-min))
      (dolist (c changes)
        (cl-destructuring-bind (line len type) c
          (forward-line (- line current-line))
          (setq current-line line)
          (let ((hunk-beg (point)))
            (while (cl-plusp len)
              (diff-hl-add-highlighting
               type
               (cond
                ((not diff-hl-draw-borders) 'empty)
                ((and (= len 1) (= line current-line)) 'single)
                ((= len 1) 'bottom)
                ((= line current-line) 'top)
                (t 'middle)))
              (forward-line 1)
              (cl-incf current-line)
              (cl-decf len))
            (let ((h (make-overlay hunk-beg (point)))
                  (hook '(diff-hl-overlay-modified)))
              (overlay-put h 'diff-hl t)
              (overlay-put h 'diff-hl-hunk t)
              (overlay-put h 'modification-hooks hook)
              (overlay-put h 'insert-in-front-hooks hook)
              (overlay-put h 'insert-behind-hooks hook))))))))

(defun diff-hl-add-highlighting (type shape)
  (let ((o (make-overlay (point) (point))))
    (overlay-put o 'diff-hl t)
    (funcall diff-hl-highlight-function o type shape)
    o))

(defun diff-hl-highlight-on-fringe (ovl type shape)
  (overlay-put ovl 'before-string (diff-hl-fringe-spec type shape)))

(defun diff-hl-remove-overlays ()
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'diff-hl) (delete-overlay o))))

(defun diff-hl-overlay-modified (ov after-p _beg _end &optional _length)
  "Delete the hunk overlay and all our line overlays inside it."
  (unless after-p
    (when (overlay-buffer ov)
      (save-restriction
        (narrow-to-region (overlay-start ov) (overlay-end ov))
        (diff-hl-remove-overlays))
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
    (vc-diff-internal t (vc-deduce-fileset) diff-hl-reference-revision nil t)
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
          (vc-diff-internal nil fileset diff-hl-reference-revision nil
                            nil diff-buffer)
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
  (cl-loop for o in (overlays-in pos (1+ pos))
           when (overlay-get o 'diff-hl-hunk)
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
                     (when (and o (= (overlay-start o) (point)))
                       (throw 'found (overlay-start o)))))))))
    (if pos
        (goto-char pos)
      (error "No further hunks found"))))

(defun diff-hl-previous-hunk ()
  "Go to the beginning of the previous hunk in the current buffer."
  (interactive)
  (diff-hl-next-hunk t))

;;;###autoload
(define-minor-mode diff-hl-mode
  "Toggle VC diff highlighting."
  :lighter "" :keymap `(([remap vc-diff] . diff-hl-diff-goto-hunk)
                        (,(kbd "C-x v n") . diff-hl-revert-hunk)
                        (,(kbd "C-x v [") . diff-hl-previous-hunk)
                        (,(kbd "C-x v ]") . diff-hl-next-hunk))
  (if diff-hl-mode
      (progn
        (diff-hl-maybe-define-bitmaps)
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
  (cond
   (buffer-file-name
    (diff-hl-mode 1))
   ((eq major-mode 'vc-dir-mode)
    (diff-hl-dir-mode 1))))

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
