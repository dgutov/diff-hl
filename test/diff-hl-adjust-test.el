;;; diff-hl-adjust-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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
(require 'ert)

(ert-deftest diff-hl-adjust-no-intersection ()
  (should
   (equal
    (diff-hl-adjust-changes
     (copy-tree '((1 2 0) (5 3 0)))
     (list '(3 4 2) '(11 3 0)))
    '((1 2 0) (7 3 0)))))

(ert-deftest diff-hl-adjust-overlap-front ()
  (should
   (equal
    (diff-hl-adjust-changes
     (copy-tree '((1 2 0) (5 3 0) (11 0 2)))
     (list '(3 6 4)))
    '((1 2 0) (9 1 0) (13 0 2)))))

(ert-deftest diff-hl-adjust-overlap-back ()
  (should
   (equal
    (diff-hl-adjust-changes
     (copy-tree '((1 2 0) (11 0 2)))
     (list '(2 6 4)))
    '((1 4 0) (13 0 2)))))

(ert-deftest diff-hl-adjust-overlap-multiple ()
  (should
   (equal
    (diff-hl-adjust-changes
     (copy-tree '((1 2 0) (4 3 2)))
     (list '(2 6 4)))
    '((1 4 0) (8 1 2)))))

(ert-deftest diff-hl-adjust-multiple-new ()
  (should
   (equal
    (diff-hl-adjust-changes
     (copy-tree '((1 2 0) (5 3 0)))
     (list '(2 1 0) '(4 2 1)))
    '((1 3 0) (7 3 0)))))

(provide 'diff-hl-adjust-test)

;;; diff-hl-adjust-test.el ends here
