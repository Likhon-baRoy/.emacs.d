;;; company-statistics-tests.el --- company-statistics tests  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; Author: Ingo Lohmar

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
;; emacs -batch -L . -L ../company-mode/ -l ert -l company-statistics-tests.el  -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(require 'company-statistics)
(setq company-statistics-auto-restore nil
      company-statistics-auto-save nil)

(company-statistics-mode)

;;; Core

(defun my/hash-compare (h1 h2 &optional pred)
  "Check that hashes H1 and H2 use the same test, contain the same keys (as
per that test), and that their stored values agree (as per PRED, which
defaults to `equal')."
  (let ((key-test (hash-table-test h1))
        (pred (or pred 'equal)))
    (and (eq key-test (hash-table-test h2))
         (eq (hash-table-count h1) (hash-table-count h2))
         (let ((keys nil))
           (maphash (lambda (k v) (push k keys)) h1) ;get keys
           (null                                     ;expect no mismatch
            (catch 'mismatch
              (while keys               ;if this finishes, it's nil
                (let* ((k (car keys))
                       (v1 (gethash k h1))
                       (v2 (gethash k h2)))
                  (setq keys (cdr keys))
                  (unless (funcall pred v1 v2)
                    (throw 'mismatch k))))))))))

(defun my/vector-slice-compare (v1 i1 v2 i2 count &optional pred)
  "Check that COUNT vector entries of V1 (starting at index I1) and
V2 (starting at index I2) satisfy the binary predicate PRED, default
`equal'.  Wraps around if index exceeds corresponding vector length."
  (let ((pred (or pred 'equal)))
    (null
     (let ((l1 (length v1))
           (l2 (length v2)))
       (catch 'mismatch
         (dolist (i (number-sequence 0 (1- count)))
           (unless (funcall pred
                            (aref v1 (mod (+ i1 i) l1))
                            (aref v2 (mod (+ i2 i) l2)))
             (throw 'mismatch t))))))))

(defmacro cs-fixture (&rest body)
  "Set up a completion history."
  `(unwind-protect
       ;; some setup to get a completion history
       (let ((company-statistics-size 5))
         (company-statistics--init)
         (let ((major-mode 'foo-mode)
               (company-statistics--context
                '((:keyword "if")
                  (:symbol "parent")
                  (:file "foo-file"))))
           (company-statistics--finished "foo"))
         (let ((major-mode 'foo-mode)
               (company-statistics--context
                '((:symbol "statistics")
                  (:file "bar-file"))))
           (company-statistics--finished "bar"))
         (let ((major-mode 'baz-mode)
               (company-statistics--context
                '((:keyword "unless")
                  (:symbol "company"))))
           (company-statistics--finished "baz"))
         (let ((major-mode 'baz-mode)
               (company-statistics--context
                '((:keyword "when")
                  (:file "quux-file"))))
           (company-statistics--finished "quux"))
         ,@body)
     ;; tear down to clean slate
     (company-statistics--init)))

(defmacro cs-persistence-fixture (&rest body)
  "Check and prepare for persistence, clean up."
  `(let ((company-statistics-file "./cs-test-tmp"))
     (when (and (file-exists-p company-statistics-file)
                (file-writable-p company-statistics-file))
       (unwind-protect
           (progn ,@body)
         ;; clean up file system
         (when (file-exists-p company-statistics-file)
           (delete-file company-statistics-file))))))

;; tests themselves

(ert-deftest c-s-history-resize ()
  "Test history-resize for shrinking and enlarging."
  (cs-fixture
   ;; resize several times
   (let ((cs-scores (copy-tree company-statistics--scores))
         (cs-history (copy-tree company-statistics--log 'vecp)))
     (company-statistics--log-resize 'dummy 10)
     ;; scores unaffected?
     (should (my/hash-compare company-statistics--scores cs-scores))
     ;; find all 4 old entries
     (should (my/vector-slice-compare company-statistics--log
                                      (- company-statistics--index 4)
                                      cs-history 0
                                      4))
     ;; index at "old-size"
     (should (equal company-statistics--index 5))
     (company-statistics--log-resize 'dummy 5)
     (should (my/hash-compare company-statistics--scores cs-scores))
     (should (my/vector-slice-compare company-statistics--log
                                      (- company-statistics--index 4)
                                      cs-history 0
                                      4))
     ;; after shrink: index at 0
     (should (equal company-statistics--index 0))
     ;; lose oldest entry "foo"
     (company-statistics--log-resize 'dummy 3)
     ;; score should be removed
     (should-not (gethash "foo" company-statistics--scores))
     ;; find *3* latest entries
     (should (my/vector-slice-compare company-statistics--log
                                      (- company-statistics--index 3)
                                      cs-history 1
                                      3))
     (should (equal company-statistics--index 0)))))

(ert-deftest c-s-persistence ()
  "Test that all statistics are properly saved and restored."
  (cs-persistence-fixture
   (cs-fixture
    (let ((cs-scores (copy-sequence company-statistics--scores))
          (cs-history (copy-sequence company-statistics--log))
          (cs-index company-statistics--index))
      (company-statistics--save)
      (company-statistics--init)        ;hence shallow copies suffice
      (company-statistics--load)
      ;; (should (equal company-statistics--scores cs-scores))
      (should (my/hash-compare company-statistics--scores cs-scores))
      (should (equal company-statistics--log cs-history))
      (should (equal company-statistics--index cs-index))))))

(ert-deftest c-s-score-change-light ()
  "Test a few things about the default score updates."
  (let ((major-mode 'foobar-mode))
    (should (equal (company-statistics-score-change-light "dummy")
                   '((nil . 1) (foobar-mode . 1))))))

(ert-deftest c-s-score-calc-light ()
  "Test score calculation default."
  (cs-fixture
   ;; FIXME assumes that light context is a subset of the heavy context?
   (let ((major-mode 'foo-mode))
     (should (eq (company-statistics-score-calc-light "foo") 2))
     (should (eq (company-statistics-score-calc-light "bar") 2))
     (should (eq (company-statistics-score-calc-light "baz") 1))
     (should (eq (company-statistics-score-calc-light "quux") 1)))
   (let ((major-mode 'baz-mode))
     (should (eq (company-statistics-score-calc-light "foo") 1))
     (should (eq (company-statistics-score-calc-light "bar") 1))
     (should (eq (company-statistics-score-calc-light "baz") 2))
     (should (eq (company-statistics-score-calc-light "quux") 2)))))

(ert-deftest c-s-score-change-heavy ()
  "Test a few things about the heavy score updates."
  (let ((major-mode 'foobar-mode))
    (should (equal (company-statistics-score-change-heavy "dummy")
                   '((nil . 1) (foobar-mode . 1))))
    (let ((company-statistics--context
           '((:keyword "kwd")
             nil                        ;deliberately omit parent symbol
             (:file "test-file.XYZ"))))
      (should (equal (company-statistics-score-change-heavy "dummy")
                     '((nil . 1) (foobar-mode . 1)
                       ((:keyword "kwd") . 1)
                       ((:file "test-file.XYZ") . 1)))))))

(ert-deftest c-s-score-calc-heavy ()
  "Test heavy score calculation."
  (cs-fixture
   (let ((major-mode 'foo-mode)
         (company-statistics--context
          '((:symbol "company")
            (:file "foo-file"))))
     (should (eq (company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (company-statistics-score-calc-heavy "foo") 3))
     (should (eq (company-statistics-score-calc-heavy "bar") 2))
     (should (eq (company-statistics-score-calc-heavy "baz") 2))
     (should (eq (company-statistics-score-calc-heavy "quux") 1)))
   (let ((major-mode 'foo-mode)
         (company-statistics--context
          '((:keyword "unless")
            (:symbol "parent")
            (:file "quux-file"))))
     (should (eq (company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (company-statistics-score-calc-heavy "foo") 3))
     (should (eq (company-statistics-score-calc-heavy "bar") 2))
     (should (eq (company-statistics-score-calc-heavy "baz") 2))
     (should (eq (company-statistics-score-calc-heavy "quux") 2)))
   (let ((major-mode 'baz-mode)
         (company-statistics--context
          '((:keyword "when")
            (:file "baz-file"))))
     (should (eq (company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (company-statistics-score-calc-heavy "foo") 1))
     (should (eq (company-statistics-score-calc-heavy "bar") 1))
     (should (eq (company-statistics-score-calc-heavy "baz") 2))
     (should (eq (company-statistics-score-calc-heavy "quux") 3)))
   (let ((major-mode 'baz-mode)
         (company-statistics--context
          '((:keyword "if")
            (:symbol "statistics")
            (:file "quux-file"))))
     (should (eq (company-statistics-score-calc-heavy "dummy") 0))
     (should (eq (company-statistics-score-calc-heavy "foo") 2))
     (should (eq (company-statistics-score-calc-heavy "bar") 2))
     (should (eq (company-statistics-score-calc-heavy "baz") 2))
     (should (eq (company-statistics-score-calc-heavy "quux") 3)))))

(ert-deftest c-s-alist-update ()
  "Test central helper function for context/score alist update."
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (company-statistics--alist-update alist updates '+)
                   '((nil . 0) ("a" . 2) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  ;; filter only checks on merged, so nil entry remains, and symbol should not pose a problem:
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (company-statistics--alist-update alist updates '+ 'zerop)
                   '((nil . 0) ("a" . 2) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (company-statistics--alist-update alist updates '-)
                   '((nil . 0) ("a" . 0) ("b" . 2) ("d" . some-symbol) ("c" . 3)))))
  (let ((alist '((nil . 0) ("a" . 1) ("b" . 2) ("d" . some-symbol)))
        (updates '(("a" . 1) ("c" . 3))))
    (should (equal (company-statistics--alist-update alist updates '- 'zerop)
                   '((nil . 0) ("b" . 2) ("d" . some-symbol) ("c" . 3))))))

(ert-deftest c-s-scores-add ()
  "Test adding scores."
  (cs-fixture
   ;; new entry
   (company-statistics--scores-add "zufpah" '((nil . 27)))
   (should (equal (gethash "zufpah" company-statistics--scores)
                  '((nil . 27))))
   ;; update existing entry
   (company-statistics--scores-add "foo" '((nil . 2)))
   (let ((h (gethash "foo" company-statistics--scores)))
     (should (equal (assoc nil h) '(nil . 3)))
     (should (equal (assoc 'foo-mode h) '(foo-mode . 1))))))

(ert-deftest c-s-history-revert ()
  "Test reverting a score update stored in history."
  ;; deep copies throughout!
  (cs-fixture
   ;; pointing to nil, should not change anything
   (let ((cs-scores (copy-tree company-statistics--scores))
         (cs-history (copy-tree company-statistics--log 'vecp))
         (cs-index company-statistics--index))
     (company-statistics--log-revert)
     (should (my/hash-compare company-statistics--scores cs-scores))
     (should (equal company-statistics--log cs-history))
     (should (equal company-statistics--index cs-index))))
  (cs-fixture
   ;; remove existing item 2: should vanish from scores
   (let ((cs-scores (copy-tree company-statistics--scores))
         (cs-history (copy-tree company-statistics--log 'vecp))
         (cs-index company-statistics--index))
     (company-statistics--log-revert 2)
     (should-not (gethash "baz" company-statistics--scores))
     (should (equal company-statistics--log cs-history))
     (should (equal company-statistics--index cs-index))))
  (cs-fixture
   ;; remove just inserted item 3 (scores should be same)
   (let ((cs-scores (copy-tree company-statistics--scores))
         (cs-history (copy-tree company-statistics--log 'vecp))
         (cs-index company-statistics--index))
     (let ((major-mode 'extra-mode))
       (company-statistics--finished "foo")) ;adds to scores, history, index
     (company-statistics--log-revert 4) ;reverts scores only, so...
     (aset cs-history 4 '("foo" (nil . 1) (extra-mode . 1)))
     (setq cs-index (mod (1+ cs-index) company-statistics-size))
     (should (my/hash-compare company-statistics--scores cs-scores))
     (should (equal company-statistics--log cs-history))
     (should (equal company-statistics--index cs-index)))))

(ert-deftest c-s-history-store ()
  "Test insert/overwrite of history item."
  (cs-fixture
   (let ((cs-history (copy-tree company-statistics--log 'vecp))
         (cs-index company-statistics--index))
     ;; only changes history and index
     (company-statistics--log-store "foo" '((nil . 27)))
     (aset cs-history cs-index '("foo" (nil . 27)))
     (setq cs-index 0)                  ;wraps around
     (should (equal company-statistics--log cs-history))
     (should (equal company-statistics--index cs-index))
     ;; now wrap around to overwrite an entry
     (company-statistics--log-store "tagyok" '((bla . 42)))
     (aset cs-history cs-index '("tagyok" (bla . 42)))
     (setq cs-index 1)
     (should (equal company-statistics--log cs-history))
     (should (equal company-statistics--index cs-index)))))

;; test finished and sort functions?  if the above is ok, they are trivial...
