;;; diff-hl-test.el --- tests for diff-hl -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.

;; Author:   Nathan Moreau <nathan.moreau@m4x.org>

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

;;; Code:

(require 'diff-hl)
(require 'subr-x) ;; string-trim
(require 'ert)
(require 'vc-git)

(defvar diff-hl-test-source-file
  (expand-file-name (concat (file-name-directory (locate-library "diff-hl"))
                            "test/empty")))

(defvar diff-hl-test-initial-content nil)

(defmacro diff-hl-test-in-source (&rest body)
  `(save-window-excursion
     (find-file diff-hl-test-source-file)
     ,@body))
(put 'diff-hl-test-in-source 'lisp-indent-function 0)

(defun diff-hl-test-init ()
  (diff-hl-test-in-source
    (setq diff-hl-test-initial-content (buffer-string)))
  t)

(defun diff-hl-test-teardown ()
  (diff-hl-test-in-source
    (erase-buffer)
    (insert diff-hl-test-initial-content)
    (save-buffer)
    (pcase (vc-backend buffer-file-name)
      (`Git
       (vc-git-command nil 0 buffer-file-name "reset"))
      (`Hg
       (vc-hg-command nil 0 buffer-file-name "revert")))))

(defun diff-hl-test-compute-diff-lines ()
  (diff-hl-test-in-source
    (save-buffer)
    (let ((vc-diff-switches "-w"))
      (diff-hl-diff-goto-hunk))
    (switch-to-buffer "*vc-diff*")
    (let ((lines nil)
          (previous-line (point-min)))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-line 1)
        (push (string-trim (buffer-substring-no-properties previous-line (point))) lines)
        (setq previous-line (point)))
      (delq nil (nreverse lines)))))

(defmacro diff-hl-deftest (name &rest body)
  `(ert-deftest ,name ()
     (diff-hl-test-init)
     (unwind-protect
         (progn ,@body)
       (diff-hl-test-teardown))))
(put 'diff-hl-deftest 'lisp-indent-function 'defun)

(diff-hl-deftest diff-hl-insert ()
  (diff-hl-test-in-source
    (goto-char (point-max))
    (insert "added\n")
    (should (equal "+added"
                   (car (last (diff-hl-test-compute-diff-lines)))))))

(diff-hl-deftest diff-hl-remove ()
  (diff-hl-test-in-source
    (delete-region (point-min) (point-max))
    (should (equal "-last line"
                   (car (last (diff-hl-test-compute-diff-lines)))))))

(diff-hl-deftest diff-hl-indirect-buffer-insert ()
  (diff-hl-test-in-source
    (narrow-to-region (point-min) (point-max))
    (goto-char (point-max))
    (insert "added\n")
    (should (equal "+added"
                   (car (last (diff-hl-test-compute-diff-lines)))))))

(diff-hl-deftest diff-hl-indirect-buffer-remove ()
  (diff-hl-test-in-source
    (narrow-to-region (point-min) (point-max))
    (goto-char (point-min))
    (delete-region (point) (point-max))
    (should (equal "-last line"
                   (car (last (diff-hl-test-compute-diff-lines)))))))

(diff-hl-deftest diff-hl-indirect-buffer-move ()
  (diff-hl-test-in-source
    (narrow-to-region (point-min) (point-max))
    (goto-char (point-min))
    (kill-whole-line 3)
    (goto-char (point-max))
    (insert "added\n")
    (save-buffer)
    (diff-hl-mode 1)
    (diff-hl-update)
    (diff-hl-previous-hunk)
    (should (looking-at "added"))
    (diff-hl-previous-hunk)
    (should (looking-at "function2"))
    (should-error (diff-hl-previous-hunk) :type 'user-error)
    (diff-hl-next-hunk)
    (should (looking-at "added"))
    (should-error (diff-hl-next-hunk) :type 'user-error)))

(diff-hl-deftest diff-hl-indirect-buffer-move-async ()
  (skip-unless (>= emacs-major-version 27)) ;No `main-thread'.
  (diff-hl-test-in-source
   (let ((diff-hl-update-async t))
     (narrow-to-region (point-min) (point-max))
     (goto-char (point-min))
     (kill-whole-line 3)
     (goto-char (point-max))
     (insert "added\n")
     (save-buffer)
     (diff-hl-mode 1)
     (diff-hl-update)

     (while (or (process-live-p
                  (get-buffer-process " *diff-hl* "))
                (process-live-p
                  (get-buffer-process " *diff-hl-reference* ")))
       (accept-process-output nil 0.05))

     (diff-hl-previous-hunk)
     (should (looking-at "added"))
     (diff-hl-previous-hunk)
     (should (looking-at "function2"))
     (should-error (diff-hl-previous-hunk) :type 'user-error)
     (diff-hl-next-hunk)
     (should (looking-at "added"))
     (should-error (diff-hl-next-hunk) :type 'user-error))))

(diff-hl-deftest diff-hl-can-ignore-staged-changes ()
  (diff-hl-test-in-source
    (goto-char (point-min))
    (insert "new line 1\n")
    (save-buffer)
    (vc-git-command nil 0 buffer-file-name "add")
    (goto-char (point-max))
    (insert "new line 2\n")
    (save-buffer)
    (let ((diff-hl-show-staged-changes t))
      (should
       (null
        (assoc-default :reference (diff-hl-changes)))))
    (let* ((diff-hl-show-staged-changes nil)
           (res-buf (assoc-default :reference (diff-hl-changes))))
      (should
       (equal
        (diff-hl-changes-from-buffer res-buf)
        '((1 1 0 insert)))))))

(diff-hl-deftest diff-hl-flydiff-can-ignore-staged-changes ()
  (diff-hl-test-in-source
    (goto-char (point-min))
    (insert "new line 1\n")
    (save-buffer)
    (vc-git-command nil 0 buffer-file-name "add")
    (goto-char (point-max))
    (insert "new line 2\n")
    (let ((diff-hl-show-staged-changes t))
      (should
       (equal (diff-hl-changes-from-buffer
               (diff-hl-diff-buffer-with-reference buffer-file-name))
              '((1 1 0 insert)
                (12 1 0 insert)))))
    (let ((diff-hl-show-staged-changes nil))
      (should
       (equal (diff-hl-changes-from-buffer
               (diff-hl-diff-buffer-with-reference buffer-file-name))
              '((12 1 0 insert)))))))

(diff-hl-deftest diff-hl-can-split-away-no-trailing-newline ()
  (diff-hl-test-in-source
    (goto-char (point-max))
    (delete-char -1)
    (search-backward "}")
    (insert " ")
    (save-buffer)
    (let ((file buffer-file-name)
          (dest-buffer (get-buffer-create " *diff-hl-test*")))
      (diff-hl-diff-buffer-with-reference file dest-buffer nil 3)
      (with-current-buffer dest-buffer
        (with-no-warnings
          (let (diff-auto-refine-mode)
            (diff-hl-diff-skip-to 10)))
        (let ((inhibit-read-only t))
          (diff-hl-split-away-changes 3))
        (should (string-prefix-p
                 "@@ -9,2 +9,2 @@
\x20
-last line
+last line
\\ No newline at end of file

Diff finished."
                 (buffer-substring (point) (point-max))))))))

(diff-hl-deftest diff-hl-resolved-reference-revision-buffer-local-hg ()
  (diff-hl-test-in-source
   (setq-local diff-hl-reference-revision "test-rev")
   (setq-local vc-hg-program "chg")
   (condition-case err
       (progn
         (diff-hl-resolved-reference-revision 'Hg)
         (ert-fail "Expected an error to be signaled but none was."))
     ;; We don't have a hg repo, but we can use the error message to verify the
     ;; underlying command.
     (error
      (should (string-match-p
               "chg .*identify -r test-rev -i"
               (error-message-string err)))))))

(provide 'diff-hl-test)

;;; diff-hl-test.el ends here
