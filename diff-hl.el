(require 'diff-mode)
(require 'vc)

(defface diff-hl-insert
  '((default :inherit diff-added)
    (((class color) (min-colors 88)) :background "#33dd33"))
  "Face used to highlight inserted lines."
  :group 'diff-hl)

(defface diff-hl-delete
  '((default :inherit diff-removed)
    (((class color) (min-colors 88)) :background "#dd3333"))
  "Face used to highlight deleted lines."
  :group 'diff-hl)

(defface diff-hl-change
  '((default :inherit diff-changed)
    (((class color) (min-colors 88)) :background "#3333dd"))
  "Face used to highlight changed lines."
  :group 'diff-hl)

(defun diff-hl-changes ()
  (let* ((buf-name " *vc-bg-diff* ")
         (vc-git-diff-switches nil)
         (vc-hg-diff-switches nil)
         (vc-diff-switches '("-U0"))
         (file (buffer-file-name))
         (vc-handled-backends (default-value 'vc-handled-backends))
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

(defmacro diff-hl-defspec (symbol)
  (let* ((type-str (symbol-name type))
         (spec-sym (intern (concat "diff-hl-" type-str "-spec")))
         (face-sym (intern (concat "diff-hl-" type-str))))
    `(defconst ,spec-sym ,(propertize " " 'display
                                      `((margin left-margin)
                                        ,(propertize " " 'face face-sym))))))

(mapc (lambda (type) (diff-hl-defspec type)) '(insert delete change))

(defun diff-hl-update ()
  (let ((changes (diff-hl-changes))
        (current-line 1))
    (save-excursion
      (set-window-margins nil 1 (cdr (window-margins)))
      (goto-char (point-min))
      (mapc (lambda (o) (when (overlay-get o 'diff-hl) (delete-overlay o)))
            (overlays-in (point-min) (point-max)))
      (dolist (c changes)
        (destructuring-bind (line len type) c
          (when (eq type 'delete)
            (setq len 1))
          (forward-line (- line current-line))
          (setq current-line line)
          (while (plusp len)
            (let ((o (make-overlay (point) (line-end-position))))
              (overlay-put o 'diff-hl t)
              (overlay-put o 'before-string
                           (case type
                             ('insert diff-hl-insert-spec)
                             ('delete diff-hl-delete-spec)
                             ('change diff-hl-change-spec))))
            (forward-line 1)
            (incf current-line)
            (decf len)))))))

(provide 'diff-hl)
