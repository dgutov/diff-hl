;;; diff-hl-test.el --- tests for diff-hl -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

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
    (save-buffer)))

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
    (diff-hl-previous-hunk)
    (should (looking-at "added"))
    (diff-hl-previous-hunk)
    (should (looking-at "function2"))
    (should-error (diff-hl-previous-hunk) :type 'user-error)
    (diff-hl-next-hunk)
    (should (looking-at "added"))
    (should-error (diff-hl-next-hunk) :type 'user-error)))

(defun diff-hl-run-tests ()
  (ert-run-tests-batch))

(provide 'diff-hl-test)

;;; diff-hl-test.el ends here
