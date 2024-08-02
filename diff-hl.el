;;; diff-hl.el --- Highlight uncommitted changes using VC -*- lexical-binding: t -*-

;; Copyright (C) 2012-2024  Free Software Foundation, Inc.

;; Author:   Dmitry Gutov <dmitry@gutov.dev>
;; URL:      https://github.com/dgutov/diff-hl
;; Keywords: vc, diff
;; Version:  1.10.0
;; Package-Requires: ((cl-lib "0.2") (emacs "25.1"))

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

;; `diff-hl-mode' highlights uncommitted changes on the side of the
;; window (using the fringe, by default), allows you to jump between
;; the hunks and revert them selectively.

;; Provided commands:
;;
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
;; `diff-hl-show-hunk'       C-x v *
;; `diff-hl-stage-current-hunk' C-x v S
;; `diff-hl-set-reference-rev'
;; `diff-hl-reset-reference-rev'
;; `diff-hl-unstage-file'
;;
;; The mode takes advantage of `smartrep' if it is installed.
;;
;; Alternatively, it integrates with `repeat-mode' (Emacs 28+).

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

(require 'fringe)
(require 'diff-mode)
(require 'vc)
(require 'vc-dir)
(require 'log-view)

(eval-when-compile
  (require 'cl-lib)
  (require 'vc-git)
  (require 'vc-hg)
  (require 'face-remap)
  (declare-function smartrep-define-key 'smartrep))

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

(defcustom diff-hl-command-prefix (kbd "C-x v")
  "The prefix for all `diff-hl' commands."
  :group 'diff-hl
  :type 'string)

(defcustom diff-hl-draw-borders t
  "Non-nil to draw borders around fringe indicators."
  :group 'diff-hl
  :type 'boolean)

(defcustom diff-hl-disable-on-remote nil
  "Non-nil will disable `diff-hl' in remote buffers."
  :group 'diff-hl
  :type 'boolean)

(defcustom diff-hl-ask-before-revert-hunk t
  "Non-nil to ask for confirmation before revert a hunk."
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

(defcustom diff-hl-fringe-face-function 'diff-hl-fringe-face-from-type
  "Function to choose the fringe face for a given change type
  and position within a hunk.  Should accept two arguments."
  :group 'diff-hl
  :type 'function)

(defcustom diff-hl-side 'left
  "Which side to use for indicators."
  :type '(choice (const left)
                 (const right))
  :initialize 'custom-initialize-default
  :set (lambda (var value)
         (let ((on (bound-and-true-p global-diff-hl-mode)))
           (when on (global-diff-hl-mode -1))
           (set-default var value)
           (when on (global-diff-hl-mode 1)))))

(defcustom diff-hl-highlight-revert-hunk-function
  #'diff-hl-revert-narrow-to-hunk
  "Function to emphasize the current hunk in `diff-hl-revert-hunk'.
The function is called at the beginning of the hunk and is passed
the end position as its only argument."
  :type '(choice (const :tag "Do nothing" ignore)
                 (const :tag "Highlight the first column"
                        diff-hl-revert-highlight-first-column)
                 (const :tag "Narrow to the hunk"
                        diff-hl-revert-narrow-to-hunk)))

(defcustom diff-hl-global-modes '(not image-mode)
  "Modes for which `diff-hl-mode' is automagically turned on.
This affects the behavior of `global-diff-hl-mode'.
If nil, no modes have `diff-hl-mode' automatically turned on.
If t, all modes have `diff-hl-mode' enabled.
If a list, it should be a list of `major-mode' symbol names for
which it should be automatically turned on. The sense of the list
is negated if it begins with `not'. As such, the default value
 (not image-mode)
means that `diff-hl-mode' is turned on in all modes except for
`image-mode' buffers. Previously, `diff-hl-mode' caused worse
performance when viewing such files in certain conditions."
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode"))))
  :group 'diff-hl)

(defcustom diff-hl-show-staged-changes t
  "Whether to include staged changes in the indicators.
Only affects Git, it's the only backend that has staging area."
  :type 'boolean)

(defcustom diff-hl-goto-hunk-old-revisions nil
  "When non-nil, `diff-hl-diff-goto-hunk' will always try to
navigate to the line in the diff that corresponds to the current
line in the file buffer (or as close as it can get to it).

When this variable is nil (default), `diff-hl-diff-goto-hunk'
only does that when called without the prefix argument, or when
the NEW revision is not specified (meaning, the diff is against
the current version of the file)."
  :type 'boolean)

(defcustom diff-hl-update-async nil
  "When non-nil, `diff-hl-update' will run asynchronously.

This can help prevent Emacs from freezing, especially by a slow version
control (VC) backend. It's disabled in remote buffers, though, since it
didn't work reliably in such during testing."
  :type 'boolean)

;; Threads are not reliable with remote files, yet.
(defcustom diff-hl-async-inhibit-functions (list #'diff-hl-with-editor-p
                                                 #'file-remote-p)
  "Functions to call to check whether asychronous method should be disabled.

When `diff-hl-update-async' is non-nil, these functions are called in turn
and passed the value `default-directory'.

If any returns non-nil, `diff-hl-update' will run synchronously anyway."
  :type '(repeat :tag "Predicate" function))

(defvar diff-hl-reference-revision nil
  "Revision to diff against.  nil means the most recent one.")

(defun diff-hl-define-bitmaps ()
  (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                         (numberp text-scale-mode-amount))
                    (expt text-scale-mode-step text-scale-mode-amount)
                  1))
         (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
         (h (+ (ceiling (* (frame-char-height) scale))
               (if (floatp spacing)
                   (truncate (* (frame-char-height) spacing))
                 spacing)))
         (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                 16))
         (_ (when (zerop w) (setq w 16)))
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
    (define-fringe-bitmap 'diff-hl-bmp-i [3 3 0 3 3 3 3 3 3 3] nil 2 'center)
    (let* ((w2 (* (/ w 2) 2))
           ;; When fringes are disabled, it's easier to fix up the width,
           ;; instead of doing nothing (#20).
           (w2 (if (zerop w2) 2 w2))
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
      )))

(defun diff-hl-maybe-define-bitmaps ()
  (when (window-system) ;; No fringes in the console.
    (unless (fringe-bitmap-p 'diff-hl-bmp-empty)
      (diff-hl-define-bitmaps)
      (define-fringe-bitmap 'diff-hl-bmp-empty [0] 1 1 'center))))

(defun diff-hl-maybe-redefine-bitmaps ()
  (when (window-system)
    (diff-hl-define-bitmaps)))

(defvar diff-hl-spec-cache (make-hash-table :test 'equal))

(defun diff-hl-fringe-spec (type pos side)
  (let* ((key (list type pos side
                    diff-hl-fringe-face-function
                    diff-hl-fringe-bmp-function))
         (val (gethash key diff-hl-spec-cache)))
    (unless val
      (let* ((face-sym (funcall diff-hl-fringe-face-function type pos))
             (bmp-sym (funcall diff-hl-fringe-bmp-function type pos)))
        (setq val (propertize " " 'display `((,(intern (format "%s-fringe" side))
                                              ,bmp-sym ,face-sym))))
        (puthash key val diff-hl-spec-cache)))
    val))

(defun diff-hl-fringe-face-from-type (type _pos)
  (intern (format "diff-hl-%s" type)))

(defun diff-hl-fringe-bmp-from-pos (_type pos)
  (intern (format "diff-hl-bmp-%s" pos)))

(defun diff-hl-fringe-bmp-from-type (type _pos)
  (cl-case type
    (unknown 'question-mark)
    (change 'exclamation-mark)
    (ignored 'diff-hl-bmp-i)
    (t (intern (format "diff-hl-bmp-%s" type)))))

(defvar vc-svn-diff-switches)
(defvar vc-fossil-diff-switches)

(defmacro diff-hl-with-diff-switches (body)
  `(let ((vc-git-diff-switches
          ;; https://github.com/dgutov/diff-hl/issues/67
          (cons "-U0"
                ;; https://github.com/dgutov/diff-hl/issues/9
                (and (boundp 'vc-git-diff-switches)
                     (listp vc-git-diff-switches)
                     (cl-remove-if-not
                      (lambda (arg)
                        (member arg '("--histogram" "--patience" "--minimal" "--textconv")))
                      vc-git-diff-switches))))
         (vc-hg-diff-switches nil)
         (vc-svn-diff-switches nil)
         (vc-fossil-diff-switches '("-c" "0"))
         (vc-diff-switches '("-U0"))
         ,@(when (boundp 'vc-disable-async-diff)
             '((vc-disable-async-diff t))))
     ,body))

(defun diff-hl-modified-p (state)
  (or (memq state '(edited conflict))
      (and (eq state 'up-to-date)
           ;; VC state is stale in after-revert-hook.
           (or revert-buffer-in-progress-p
               ;; Diffing against an older revision.
               diff-hl-reference-revision))))

(declare-function vc-git-command "vc-git")

(defun diff-hl-changes-buffer (file backend)
  (diff-hl-with-diff-switches
   (diff-hl-diff-against-reference file backend " *diff-hl* ")))

(defun diff-hl-diff-against-reference (file backend buffer)
  (if (and (eq backend 'Git)
           (not diff-hl-reference-revision)
           (not diff-hl-show-staged-changes))
      (apply #'vc-git-command buffer 1
             (list file)
             "diff-files"
             (cons "-p" (vc-switches 'git 'diff)))
    (condition-case err
        (vc-call-backend backend 'diff (list file)
                         diff-hl-reference-revision nil
                         buffer)
      (error
       ;; https://github.com/dgutov/diff-hl/issues/117
       (when (string-match-p "\\`Failed (status 128)" (error-message-string err))
         (vc-call-backend backend 'diff (list file)
                          "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
                          nil
                          buffer)))))
  buffer)

(defun diff-hl-changes ()
  (let* ((file buffer-file-name)
         (backend (vc-backend file)))
    (when backend
      (let ((state (vc-state file backend)))
        (cond
         ((diff-hl-modified-p state)
          (diff-hl-changes-from-buffer
           (diff-hl-changes-buffer file backend)))
         ((eq state 'added)
          `((1 ,(line-number-at-pos (point-max)) insert)))
         ((eq state 'removed)
          `((1 ,(line-number-at-pos (point-max)) delete))))))))

(defun diff-hl-changes-from-buffer (buf)
  (with-current-buffer buf
    (let (res)
      (goto-char (point-min))
      (unless (eobp)
        ;; TODO: When 27.1 is the minimum requirement, we can drop
        ;; these bindings: that version, in addition to switching over
        ;; to the diff-refine var, also added the
        ;; called-interactively-p check, so refinement can't be
        ;; triggered by code calling the navigation functions, only by
        ;; direct interactive invocations.
        (ignore-errors
          (with-no-warnings
            (let (diff-auto-refine-mode)
              (diff-beginning-of-hunk t))))
        (while (looking-at diff-hunk-header-re-unified)
          (let ((line (string-to-number (match-string 3)))
                (len (let ((m (match-string 4)))
                       (if m (string-to-number m) 1)))
                (beg (point)))
            (with-no-warnings
              (let (diff-auto-refine-mode)
                (diff-end-of-hunk)))
            (let* ((inserts (diff-count-matches "^\\+" beg (point)))
                   (deletes (diff-count-matches "^-" beg (point)))
                   (type (cond ((zerop deletes) 'insert)
                               ((zerop inserts) 'delete)
                               (t 'change))))
              (when (eq type 'delete)
                (setq len 1)
                (cl-incf line))
              (push (list line len type) res)))))
      (nreverse res))))

(defun diff-hl-update ()
  "Updates the diff-hl overlay."
  (if (and diff-hl-update-async
           (not
            (run-hook-with-args-until-success 'diff-hl-async-inhibit-functions
                                              default-directory)))
      ;; TODO: debounce if a thread is already running.
      (make-thread 'diff-hl--update-safe "diff-hl--update-safe")
    (diff-hl--update)))

(defun diff-hl-with-editor-p (_dir)
  (bound-and-true-p with-editor-mode))

(defun diff-hl--update-safe ()
  "Updates the diff-hl overlay. It handles and logs when an error is signaled."
  (condition-case err
      (diff-hl--update)
    (error
     (message "An error occurred in diff-hl--update: %S" err)
     nil)))

(defun diff-hl--update ()
  "Updates the diff-hl overlay."
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (diff-hl-remove-overlays)
    (save-excursion
      (save-restriction
        (widen)
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
                (overlay-put h 'diff-hl-hunk-type type)
                (overlay-put h 'modification-hooks hook)
                (overlay-put h 'insert-in-front-hooks hook)
                (overlay-put h 'insert-behind-hooks hook)))))))))

(defvar-local diff-hl--modified-tick nil)

(put 'diff-hl--modified-tick 'permanent-local t)

(defun diff-hl-update-once ()
  (unless (equal diff-hl--modified-tick (buffer-chars-modified-tick))
    (diff-hl-update)
    (setq diff-hl--modified-tick (buffer-chars-modified-tick))))

(defun diff-hl-add-highlighting (type shape)
  (let ((o (make-overlay (point) (point))))
    (overlay-put o 'diff-hl t)
    (funcall diff-hl-highlight-function o type shape)
    o))

(defun diff-hl-highlight-on-fringe (ovl type shape)
  (overlay-put ovl 'before-string (diff-hl-fringe-spec type shape
                                                       diff-hl-side)))

(defun diff-hl-remove-overlays (&optional beg end)
  (save-restriction
    (widen)
    (dolist (o (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (overlay-get o 'diff-hl) (delete-overlay o)))))

(defun diff-hl-overlay-modified (ov after-p _beg _end &optional _length)
  "Delete the hunk overlay and all our line overlays inside it."
  (unless after-p
    (when (overlay-buffer ov)
      (diff-hl-remove-overlays (overlay-start ov) (overlay-end ov))
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

(defun diff-hl-after-revert ()
  (when (bound-and-true-p revert-buffer-preserve-modes)
    (diff-hl-update)))

(defun diff-hl-diff-goto-hunk-1 (historic)
  (defvar vc-sentinel-movepoint)
  (vc-buffer-sync)
  (let* ((line (line-number-at-pos))
         (buffer (current-buffer))
         (rev1 diff-hl-reference-revision)
         rev2)
    (when historic
      (let ((revs (diff-hl-diff-read-revisions rev1)))
        (setq rev1 (car revs)
              rev2 (cdr revs))))
    (vc-diff-internal t (vc-deduce-fileset) rev1 rev2 t)
    (vc-run-delayed (if (< (line-number-at-pos (point-max)) 3)
                        (with-current-buffer buffer (diff-hl-remove-overlays))
                      (when (or (not rev2) diff-hl-goto-hunk-old-revisions)
                        (diff-hl-diff-skip-to line))
                      (setq vc-sentinel-movepoint (point))))))

(defun diff-hl-diff-goto-hunk (&optional historic)
  "Run VC diff command and go to the line corresponding to the current."
  (interactive (list current-prefix-arg))
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (diff-hl-diff-goto-hunk-1 historic)))

(defun diff-hl-diff-read-revisions (rev1-default)
  (let* ((file buffer-file-name)
         (files (list file))
         (backend (vc-backend file))
         (rev2-default nil))
    (cond
     ;; if the file is not up-to-date, use working revision as older revision
     ((not (vc-up-to-date-p file))
      (setq rev1-default
            (or rev1-default
                (vc-working-revision file))))
     ((not rev1-default)
      (setq rev1-default (ignore-errors ;If `previous-revision' doesn't work.
                           (vc-call-backend backend 'previous-revision file
                                            (vc-working-revision file))))
      (when (string= rev1-default "") (setq rev1-default nil))))
    ;; finally read the revisions
    (let* ((rev1-prompt (if rev1-default
                            (concat "Older revision (default "
                                    rev1-default "): ")
                          "Older revision: "))
           (rev2-prompt (concat "Newer revision (default "
                                (or rev2-default "current source") "): "))
           (rev1 (vc-read-revision rev1-prompt files backend rev1-default))
           (rev2 (vc-read-revision rev2-prompt files backend rev2-default)))
      (when (string= rev1 "") (setq rev1 nil))
      (when (string= rev2 "") (setq rev2 nil))
      (cons rev1 rev2))))

(defun diff-hl-diff-skip-to (line)
  "In `diff-mode', skip to the hunk and line corresponding to LINE
in the source file, or the last line of the hunk above it."
  (goto-char (point-min)) ; Counteract any similar behavior in diff-mode.
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
                (unless (looking-at "^[-\\]")
                  (cl-decf to-go))))))))))

(defface diff-hl-reverted-hunk-highlight
  '((default :inverse-video t))
  "Face used to highlight the first column of the hunk to be reverted.")

(defun diff-hl-revert-highlight-first-column (end)
  (re-search-forward "^[+-]")
  (forward-line 0)
  (setq end (diff-hl-split-away-changes 0))
  (let ((inhibit-read-only t))
    (save-excursion
      (while (< (point) end)
        (font-lock-prepend-text-property (point) (1+ (point)) 'font-lock-face
                                         'diff-hl-reverted-hunk-highlight)
        (forward-line 1)))))

(defun diff-hl-revert-narrow-to-hunk (end)
  (narrow-to-region (point) end))

(defun diff-hl-revert-hunk-1 ()
  (save-restriction
    (widen)
    (vc-buffer-sync)
    (let* ((diff-buffer (get-buffer-create
                         (generate-new-buffer-name "*diff-hl*")))
           (buffer (current-buffer))
           (line (save-excursion
                   (diff-hl-find-current-hunk)
                   (line-number-at-pos)))
           (file buffer-file-name)
           (backend (vc-backend file)))
      (unwind-protect
          (progn
            (vc-setup-buffer diff-buffer)
            (diff-hl-diff-against-reference file backend diff-buffer)
            (set-buffer diff-buffer)
            (diff-mode)
            (setq-local diff-vc-backend backend)
            (setq-local diff-vc-revisions (list diff-hl-reference-revision nil))
            (setq buffer-read-only t)
            (pop-to-buffer diff-buffer)
            (vc-run-delayed
              (vc-diff-finish diff-buffer nil)
              (let (beg-line end-line m-beg m-end)
                (when (eobp)
                  (with-current-buffer buffer (diff-hl-remove-overlays))
                  (user-error "Buffer is up-to-date"))
                (with-no-warnings
                  (let (diff-auto-refine-mode)
                    (diff-hl-diff-skip-to line)))
                (setq m-end (diff-hl-split-away-changes 3))
                (setq m-beg (point-marker))
                (funcall diff-hl-highlight-revert-hunk-function m-end)
                (setq beg-line (line-number-at-pos m-beg)
                      end-line (line-number-at-pos m-end))
                (let ((wbh (window-body-height)))
                  (if (>= wbh (- end-line beg-line))
                      (recenter (/ (+ wbh (- beg-line end-line) 2) 2))
                    (recenter 1)))
                (with-no-warnings
                  (when diff-auto-refine-mode
                    (diff-refine-hunk)))
                (if diff-hl-ask-before-revert-hunk
                    (unless (yes-or-no-p (format "Revert current hunk in %s? "
                                                 file))
                      (user-error "Revert canceled")))
                (widen)
                (let ((diff-advance-after-apply-hunk nil))
                  (save-window-excursion
                    (diff-apply-hunk t)))
                (with-current-buffer buffer
                  (save-buffer))
                (message "Hunk reverted"))))
        (quit-windows-on diff-buffer t)))))

(defun diff-hl-split-away-changes (max-context)
  "Split away the minimal hunk at point from the rest of the hunk.

The minimal hunk is the hunk a diff program would produce if
asked for 0 lines of context. Add MAX-CONTEXT lines of context at
most (stop when encounter another minimal hunk).

Move point to the beginning of the delineated hunk and return
its end position."
  (let (end-marker)
    (save-excursion
      (while (looking-at "[-+\\]") (forward-line 1))
      (dotimes (_i max-context)
        (unless (looking-at "@\\|[-+]")
          (forward-line 1)))
      (setq end-marker (point-marker))
      (unless (or (eobp)
                  (looking-at "@"))
        (diff-split-hunk)))
    (unless (looking-at "[-+]") (forward-line -1))
    (while (looking-at "[-+\\]") (forward-line -1))
    (dotimes (_i max-context)
      (unless (looking-at "@\\|[-+]")
        (forward-line -1)))
    (unless (looking-at "@")
      (forward-line 1)
      (diff-split-hunk)
      (forward-line -1))
    end-marker))

(defun diff-hl-revert-hunk ()
  "Revert the diff hunk with changes at or above the point."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (diff-hl-revert-hunk-1)))

(defun diff-hl-hunk-overlay-at (pos)
  (cl-loop for o in (overlays-in pos (1+ pos))
           when (overlay-get o 'diff-hl-hunk)
           return o))

(defun diff-hl-search-next-hunk (&optional backward point)
  "Search the next hunk in the current buffer, or previous if BACKWARD."
  (save-excursion
    (when point
      (goto-char point))
    (catch 'found
      (while (not (if backward (bobp) (eobp)))
        (goto-char (if backward
                       (previous-overlay-change (point))
                     (next-overlay-change (point))))
        (let ((o (diff-hl-hunk-overlay-at (point))))
          (when (and o (= (overlay-start o) (point)))
            (throw 'found o)))))))

(defun diff-hl-next-hunk (&optional backward)
  "Go to the beginning of the next hunk in the current buffer."
  (interactive)
  (let ((overlay (diff-hl-search-next-hunk backward)))
    (if overlay
        (goto-char (overlay-start overlay))
      (user-error "No further hunks found"))))

(defun diff-hl-previous-hunk ()
  "Go to the beginning of the previous hunk in the current buffer."
  (interactive)
  (diff-hl-next-hunk t))

(defun diff-hl-find-current-hunk ()
  (let (o)
    (cond
     ((diff-hl-hunk-overlay-at (point)))
     ((setq o (diff-hl-search-next-hunk t))
      (goto-char (overlay-start o)))
     (t
      (diff-hl-next-hunk)))))

(defun diff-hl-mark-hunk ()
  (interactive)
  (let ((hunk (diff-hl-hunk-overlay-at (point))))
    (unless hunk
      (user-error "No hunk at point"))
    (goto-char (overlay-start hunk))
    (push-mark (overlay-end hunk) nil t)))

(defun diff-hl--ensure-staging-supported ()
  (let ((backend (vc-backend buffer-file-name)))
    (unless (eq backend 'Git)
      (user-error "Only Git supports staging; this file is controlled by %s" backend))))

(defun diff-hl-stage-diff (orig-buffer)
  (let ((patchfile (make-temp-file "diff-hl-stage-patch"))
        success)
    (write-region (point-min) (point-max) patchfile
                  nil 'silent)
    (unwind-protect
        (with-current-buffer orig-buffer
          (with-output-to-string
            (vc-git-command standard-output 0
                            patchfile
                            "apply" "--cached" )
            (setq success t)))
      (delete-file patchfile))
    success))

(defun diff-hl-stage-current-hunk ()
  "Stage the hunk at or near point.

Only supported with Git."
  (interactive)
  (diff-hl--ensure-staging-supported)
  (diff-hl-find-current-hunk)
  (let* ((line (line-number-at-pos))
         (file buffer-file-name)
         (dest-buffer (get-buffer-create " *diff-hl-stage*"))
         (orig-buffer (current-buffer))
         ;; FIXME: If the file name has double quotes, these need to be quoted.
         (file-base (file-name-nondirectory file))
         success)
    (with-current-buffer dest-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (diff-hl-diff-buffer-with-reference file dest-buffer nil 3)
    (with-current-buffer dest-buffer
      (with-no-warnings
        (let (diff-auto-refine-mode)
          (diff-hl-diff-skip-to line)))
      (let ((inhibit-read-only t))
        (diff-hl-split-away-changes 3)
        (save-excursion
          (diff-end-of-hunk)
          (delete-region (point) (point-max)))
        (diff-beginning-of-hunk)
        (delete-region (point-min) (point))
        ;; diff-no-select creates a very ugly header; Git rejects it
        (insert (format "diff a/%s b/%s\n" file-base file-base))
        (insert (format "--- a/%s\n" file-base))
        (insert (format "+++ b/%s\n" file-base)))
      (setq success (diff-hl-stage-diff orig-buffer)))
    (when success
      (if diff-hl-show-staged-changes
          (message (concat "Hunk staged; customize `diff-hl-show-staged-changes'"
                           " to highlight only unstaged changes"))
        (message "Hunk staged"))
      (unless diff-hl-show-staged-changes
        (diff-hl-update)))))

(defun diff-hl-unstage-file ()
  "Unstage all changes in the current file.

Only supported with Git."
  (interactive)
  (unless buffer-file-name
    (user-error "No current file"))
  (diff-hl--ensure-staging-supported)
  (vc-git-command nil 0 buffer-file-name "reset")
  (message "Unstaged all")
  (unless diff-hl-show-staged-changes
    (diff-hl-update)))

(defun diff-hl-stage-dwim (&optional with-edit)
  "Stage the current hunk or choose the hunks to stage.
When called with the prefix argument, invokes `diff-hl-stage-some'."
  (interactive "P")
  (if (or with-edit (region-active-p))
      (call-interactively #'diff-hl-stage-some)
    (call-interactively #'diff-hl-stage-current-hunk)))

(defvar diff-hl-stage--orig nil)

(define-derived-mode diff-hl-stage-diff-mode diff-mode "Stage Diff"
  "Major mode for editing a diff buffer before staging.

\\[diff-hl-stage-commit]"
  (setq revert-buffer-function #'ignore))

(define-key diff-hl-stage-diff-mode-map (kbd "C-c C-c") #'diff-hl-stage-finish)

(defun diff-hl-stage-some (&optional beg end)
  "Stage some or all of the current changes, interactively.
Pops up a diff buffer that can be edited to choose the changes to stage."
  (interactive "r")
  (diff-hl--ensure-staging-supported)
  (let* ((line-beg (and beg (line-number-at-pos beg t)))
         (line-end (and end (line-number-at-pos end t)))
         (file buffer-file-name)
         (dest-buffer (get-buffer-create "*diff-hl-stage-some*"))
         (orig-buffer (current-buffer))
         ;; FIXME: If the file name has double quotes, these need to be quoted.
         (file-base (file-name-nondirectory file)))
    (with-current-buffer dest-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (diff-hl-diff-buffer-with-reference file dest-buffer nil 3)
    (with-current-buffer dest-buffer
      (let ((inhibit-read-only t))
        (when end
          (with-no-warnings
            (let (diff-auto-refine-mode)
              (diff-hl-diff-skip-to line-end)
              (diff-hl-split-away-changes 3)
              (diff-end-of-hunk)))
          (delete-region (point) (point-max)))
        (if beg
            (with-no-warnings
              (let (diff-auto-refine-mode)
                (diff-hl-diff-skip-to line-beg)
                (diff-hl-split-away-changes 3)
                (diff-beginning-of-hunk)))
          (goto-char (point-min))
          (forward-line 3))
        (delete-region (point-min) (point))
        ;; diff-no-select creates a very ugly header; Git rejects it
        (insert (format "diff a/%s b/%s\n" file-base file-base))
        (insert (format "--- a/%s\n" file-base))
        (insert (format "+++ b/%s\n" file-base)))
      (let ((diff-default-read-only t))
        (diff-hl-stage-diff-mode))
      (setq-local diff-hl-stage--orig orig-buffer))
    (pop-to-buffer dest-buffer)
    (message "Press %s and %s to navigate, %s to split, %s to kill hunk, %s to undo, and %s to stage the diff after editing"
             (substitute-command-keys "\\`n'")
             (substitute-command-keys "\\`p'")
             (substitute-command-keys "\\[diff-split-hunk]")
             (substitute-command-keys "\\[diff-hunk-kill]")
             (substitute-command-keys "\\[diff-undo]")
             (substitute-command-keys "\\[diff-hl-stage-finish]"))))

(defun diff-hl-stage-finish ()
  (interactive)
  (let ((count 0))
    (when (diff-hl-stage-diff diff-hl-stage--orig)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re-unified nil t)
          (cl-incf count)))
      (message "Staged %d hunks" count)
      (bury-buffer))))

(defvar diff-hl-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'diff-hl-revert-hunk)
    (define-key map "[" 'diff-hl-previous-hunk)
    (define-key map "]" 'diff-hl-next-hunk)
    (define-key map "*" 'diff-hl-show-hunk)
    (define-key map "{" 'diff-hl-show-hunk-previous)
    (define-key map "}" 'diff-hl-show-hunk-next)
    (define-key map "S" 'diff-hl-stage-dwim)
    map))
(fset 'diff-hl-command-map diff-hl-command-map)

(defvar diff-hl-lighter ""
  "Mode line lighter for Diff Hl.

The value of this variable is a mode line template as in
`mode-line-format'.")

;;;###autoload
(define-minor-mode diff-hl-mode
  "Toggle VC diff highlighting."
  :lighter diff-hl-lighter
  :keymap `(([remap vc-diff] . diff-hl-diff-goto-hunk)
            (,diff-hl-command-prefix . diff-hl-command-map))
  (if diff-hl-mode
      (progn
        (diff-hl-maybe-define-bitmaps)
        (add-hook 'after-save-hook 'diff-hl-update nil t)
        (add-hook 'after-change-functions 'diff-hl-edit nil t)
        (add-hook (if vc-mode
                      ;; Defer until the end of this hook, so that its
                      ;; elements can modify the update behavior.
                      'diff-hl-mode-on-hook
                    ;; If we're only opening the file now,
                    ;; `vc-find-file-hook' likely hasn't run yet, so
                    ;; let's wait until the state information is
                    ;; saved, in order not to fetch it twice.
                    'find-file-hook)
                  'diff-hl-update-once t t)
        ;; Never removed because it acts globally.
        (add-hook 'vc-checkin-hook 'diff-hl-after-checkin)
        (add-hook 'after-revert-hook 'diff-hl-after-revert nil t)
        ;; Magit does call `auto-revert-handler', but it usually
        ;; doesn't do much, because `buffer-stale--default-function'
        ;; doesn't care about changed VC state.
        ;; https://github.com/magit/magit/issues/603
        (add-hook 'magit-revert-buffer-hook 'diff-hl-update nil t)
        ;; Magit versions 2.0-2.3 don't do the above and call this
        ;; instead, but only when they don't call `revert-buffer':
        (add-hook 'magit-not-reverted-hook 'diff-hl-update nil t)
        (add-hook 'text-scale-mode-hook 'diff-hl-maybe-redefine-bitmaps nil t))
    (remove-hook 'after-save-hook 'diff-hl-update t)
    (remove-hook 'after-change-functions 'diff-hl-edit t)
    (remove-hook 'find-file-hook 'diff-hl-update t)
    (remove-hook 'after-revert-hook 'diff-hl-after-revert t)
    (remove-hook 'magit-revert-buffer-hook 'diff-hl-update t)
    (remove-hook 'magit-not-reverted-hook 'diff-hl-update t)
    (remove-hook 'text-scale-mode-hook 'diff-hl-maybe-redefine-bitmaps t)
    (diff-hl-remove-overlays)))

(defun diff-hl-after-checkin ()
  (let ((fileset (vc-deduce-fileset t)))
    (dolist (file (nth 1 fileset))
      (let ((buf (find-buffer-visiting file)))
        (when buf
          (with-current-buffer buf
            (when diff-hl-mode
              (diff-hl-update))))))))

(defvar diff-hl-repeat-exceptions '(diff-hl-show-hunk
                                    diff-hl-show-hunk-previous
                                    diff-hl-show-hunk-next))

(when (require 'smartrep nil t)
  (let (smart-keys)
    (cl-labels ((scan (map)
                      (map-keymap
                       (lambda (event binding)
                         (if (consp binding)
                             (scan binding)
                           (when (and (characterp event)
                                      (not (memq binding diff-hl-repeat-exceptions)))
                             (push (cons (string event) binding) smart-keys))))
                       map)))
      (scan diff-hl-command-map)
      (smartrep-define-key diff-hl-mode-map diff-hl-command-prefix smart-keys))))

;; Integrate with `repeat-mode' in Emacs 28 (https://debbugs.gnu.org/47566)
;;
;; While smartrep feels solid, it looks kinda abandoned.  And the
;; chances of it being put into GNU ELPA are slim too.
(map-keymap
 (lambda (_key cmd)
   (unless (memq cmd diff-hl-repeat-exceptions)
     (put cmd 'repeat-map 'diff-hl-command-map)))
 diff-hl-command-map)

(declare-function magit-toplevel "magit-git")
(declare-function magit-unstaged-files "magit-git")

(defvar diff-hl--magit-unstaged-files nil)

(defun diff-hl-magit-pre-refresh ()
  (unless (and diff-hl-disable-on-remote
               (file-remote-p default-directory))
   (setq diff-hl--magit-unstaged-files (magit-unstaged-files t))))

(defun diff-hl-magit-post-refresh ()
  (unless (and diff-hl-disable-on-remote
               (file-remote-p default-directory))
   (let* ((topdir (magit-toplevel))
         (modified-files
          (mapcar (lambda (file) (expand-file-name file topdir))
                  (delete-consecutive-dups
                   (sort
                    (nconc (magit-unstaged-files t)
                           diff-hl--magit-unstaged-files)
                    #'string<))))
         (unmodified-states '(up-to-date ignored unregistered)))
    (setq diff-hl--magit-unstaged-files nil)
    (dolist (buf (buffer-list))
      (when (and (buffer-local-value 'diff-hl-mode buf)
                 (not (buffer-modified-p buf))
                 ;; Solve the "cloned indirect buffer" problem
                 ;; (diff-hl-mode could be non-nil there, even if
                 ;; buffer-file-name is nil):
                 (buffer-file-name buf)
                 (file-in-directory-p (buffer-file-name buf) topdir)
                 (file-exists-p (buffer-file-name buf)))
        (with-current-buffer buf
          (let* ((file buffer-file-name)
                 (backend (vc-backend file)))
            (when backend
              (cond
               ((member file modified-files)
                (when (memq (vc-state file) unmodified-states)
                  (vc-state-refresh file backend))
                (diff-hl-update))
               ((not (memq (vc-state file backend) unmodified-states))
                (vc-state-refresh file backend)
                (diff-hl-update)))))))))))

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

(defun diff-hl-make-temp-file-name (file rev &optional manual)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned."
  (let* ((auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
         (buffer-file-name file))
    (expand-file-name
     (concat (make-auto-save-file-name)
             ".~" (subst-char-in-string
                   ?/ ?_ rev)
             (unless manual ".") "~")
     temporary-file-directory)))

(defun diff-hl-create-revision (file revision)
  "Read REVISION of FILE into a buffer and return the buffer."
  (let ((automatic-backup (diff-hl-make-temp-file-name file revision))
        (filebuf (get-file-buffer file))
        (filename (diff-hl-make-temp-file-name file revision 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
          (rename-file automatic-backup filename nil)
        (with-current-buffer filebuf
          (let ((coding-system-for-read 'no-conversion)
                (coding-system-for-write 'no-conversion))
            (condition-case nil
                (with-temp-file filename
                  (let ((outbuf (current-buffer)))
                    ;; Change buffer to get local value of
                    ;; vc-checkout-switches.
                    (with-current-buffer filebuf
                      (vc-call find-revision file revision outbuf))))
              (error
               (when (file-exists-p filename)
                 (delete-file filename))))))))
    filename))

(defun diff-hl-working-revision (file &optional backend)
  "Like vc-working-revision, but always up-to-date"
  (vc-file-setprop file 'vc-working-revision
                   (vc-call-backend (or backend (vc-backend file))
                                    'working-revision file)))

(declare-function diff-no-select "diff")

(defun diff-hl-diff-buffer-with-reference (file &optional dest-buffer backend context-lines)
  "Compute the diff between the current buffer contents and reference in BACKEND.
The diffs are computed in the buffer DEST-BUFFER. This requires
the `diff-program' to be in your `exec-path'.
CONTEXT-LINES is the size of the unified diff context, defaults to 0."
  (require 'diff)
  (vc-ensure-vc-buffer)
  (save-current-buffer
    (let* ((dest-buffer (or dest-buffer "*diff-hl-diff-buffer-with-reference*"))
           (backend (or backend (vc-backend file)))
           (temporary-file-directory
            (if (and (eq system-type 'gnu/linux) (file-directory-p "/dev/shm/"))
                "/dev/shm/"
              temporary-file-directory))
           (rev
            (if (and (eq backend 'Git)
                     (not diff-hl-reference-revision)
                     (not diff-hl-show-staged-changes))
                (diff-hl-git-index-revision
                 file
                 (diff-hl-git-index-object-name file))
              (diff-hl-create-revision
               file
               (or diff-hl-reference-revision
                   (diff-hl-working-revision file backend)))))
           (switches (format "-U %d --strip-trailing-cr" (or context-lines 0))))
      (diff-no-select rev (current-buffer) switches 'noasync
                      (get-buffer-create dest-buffer))
      (with-current-buffer dest-buffer
        (let ((inhibit-read-only t))
          ;; Function `diff-sentinel' adds a final line, so remove it
          (delete-matching-lines "^Diff finished.*")))
      (get-buffer-create dest-buffer))))

;; TODO: Cache based on .git/index's mtime, maybe.
(defun diff-hl-git-index-object-name (file)
  (with-temp-buffer
    (vc-git-command (current-buffer) 0 file "ls-files" "-s")
    (and
     (goto-char (point-min))
     (re-search-forward "^[0-9]+ \\([0-9a-f]+\\)")
     (match-string-no-properties 1))))

(defun diff-hl-git-index-revision (file object-name)
  (let ((filename (diff-hl-make-temp-file-name file
                                               (concat ";" object-name)
                                               'manual))
        (filebuf (get-file-buffer file)))
    (unless (file-exists-p filename)
      (with-current-buffer filebuf
        (let ((coding-system-for-read 'no-conversion)
              (coding-system-for-write 'no-conversion))
          (condition-case nil
              (with-temp-file filename
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to be inside the repo.
                  (with-current-buffer filebuf
                    (vc-git-command outbuf 0 nil
                                    "cat-file" "blob" object-name))))
            (error
             (when (file-exists-p filename)
               (delete-file filename)))))))
    filename))

;;;###autoload
(defun turn-on-diff-hl-mode ()
  "Turn on `diff-hl-mode' or `diff-hl-dir-mode' in a buffer if appropriate."
  (cond
   (buffer-file-name
    (unless (and diff-hl-disable-on-remote
                 (file-remote-p buffer-file-name))
      (diff-hl-mode 1)))
   ((eq major-mode 'vc-dir-mode)
    (diff-hl-dir-mode 1))))

;;;###autoload
(defun diff-hl--global-turn-on ()
  "Call `turn-on-diff-hl-mode' if the current major mode is applicable."
  (when (cond ((eq diff-hl-global-modes t)
               t)
              ((eq (car-safe diff-hl-global-modes) 'not)
               (not (memq major-mode (cdr diff-hl-global-modes))))
              (t (memq major-mode diff-hl-global-modes)))
    (turn-on-diff-hl-mode)))

(declare-function vc-annotate-extract-revision-at-line "vc-annotate")
(declare-function diff-hl-amend-mode "diff-hl-amend")

;;;###autoload
(defun diff-hl-set-reference-rev (rev)
  "Set the reference revision globally to REV.
When called interactively, REV read with completion.

The default value chosen using one of methods below:

- In a log view buffer, it uses the revision of current entry.
Call `vc-print-log' or `vc-print-root-log' first to open a log
view buffer.
- In a VC annotate buffer, it uses the revision of current line.
- In other situations, it uses the symbol at point.

Notice that this sets the reference revision globally, so in
files from other repositories, `diff-hl-mode' will not highlight
changes correctly, until you run `diff-hl-reset-reference-rev'.

Also notice that this will disable `diff-hl-amend-mode' in
buffers that enables it, since `diff-hl-amend-mode' overrides its
effect."
  (interactive
   (let* ((def (or (and (equal major-mode 'vc-annotate-mode)
                        (car (vc-annotate-extract-revision-at-line)))
                   (log-view-current-tag)
                   (thing-at-point 'symbol t)))
          (prompt (if def
                      (format "Reference revision (default %s): " def)
                    "Reference revision: ")))
     (list (vc-read-revision prompt nil nil def))))
  (if rev
      (message "Set reference revision to %s" rev)
    (user-error "No reference revision specified"))
  (setq diff-hl-reference-revision rev)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when diff-hl-mode
        (when (bound-and-true-p diff-hl-amend-mode)
          (diff-hl-amend-mode -1))
        (diff-hl-update)))))

;;;###autoload
(defun diff-hl-reset-reference-rev ()
  "Reset the reference revision globally to the most recent one."
  (interactive)
  (setq diff-hl-reference-revision nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when diff-hl-mode
        (diff-hl-update)))))

;;;###autoload
(define-globalized-minor-mode global-diff-hl-mode diff-hl-mode
  diff-hl--global-turn-on :after-hook (diff-hl-global-mode-change))

(defun diff-hl-global-mode-change ()
  (unless global-diff-hl-mode
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when diff-hl-dir-mode
          (diff-hl-dir-mode -1))))))

(provide 'diff-hl)

;;; diff-hl.el ends here
