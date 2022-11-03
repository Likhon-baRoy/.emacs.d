;;; uniquify-files-test.el - Test functions in uniquify-files.el -*- lexical-binding:t no-byte-compile:t -*-
;;
;; Copyright (C) 2017, 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;; This is not a complete test of the completion style; the way the
;; completion functions interact with completing-read is not fully
;; tested. The following table gives useful test cases for a manual
;; interactive test (copy it to an org-mode buffer).

;; See `test-uniquify-file-all-completions-face' below for an
;; explanation of `no-byte-compile'.

(require 'ert)
(require 'uniquify-files)

(defconst uft-root
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   ;; We deliberately leave out the trailing '/' here, because users
   ;; often do; the code must cope.
   "uniquify-files-resources"))

(defconst uft-alice1 (concat uft-root "/Alice/alice-1"))
(defconst uft-alice2 (concat uft-root "/Alice/alice-2"))
(defconst uft-Alice-alice3 (concat uft-root "/Alice/alice-3"))
(defconst uft-Bob-alice3 (concat uft-root "/Bob/alice-3"))
(defconst uft-bob1 (concat uft-root "/Bob/bob-1"))
(defconst uft-bob2 (concat uft-root "/Bob/bob-2"))

(defconst uft-path
   (list uft-root
	 (concat uft-root "/Alice")
	 uft-alice1
	 uft-alice2
	 uft-Alice-alice3
	 (concat uft-root "/Bob")
	 uft-Bob-alice3
	 uft-bob1
	 uft-bob2))

(defun uft-table ()
  (let (files)
    (dolist (dir uft-path)
      (mapc
       (lambda (absfile)
	 (when (and (not (string-equal "." (substring absfile -1)))
		    (not (string-equal ".." (substring absfile -2)))
		    (not (file-directory-p absfile)))
	   (push absfile files)))
       (directory-files dir t)))
    (apply-partially 'uniq-file-completion-table (uniq-file-uniquify files))))

(ert-deftest test-uniq-file-test-completion ()
  (let ((table (uft-table)))
    (should (equal (test-completion "foo-fi" table)
		   nil))

    (should (equal (test-completion "f-fi<dir" table)
		   nil))

    (should (equal (test-completion "foo-file1.text<>" table)
		   t))

    (should (equal (test-completion "foo-file1.text" table)
		   nil))

    (should (equal (test-completion "foo-file1.text<Alice/alice-1/>" table)
		   t))

    (should (equal (test-completion "foo-file3.tex" table) ;; partial file name
		   nil))

    (should (equal (test-completion "foo-file3.texts2" table)
		   t))

    (should (equal (test-completion "bar-file2.text<Alice/alice-" table)
		   nil))
    ))

(ert-deftest test-uniq-file-all-completions-noface ()
  (let ((table (uft-table))
	(completion-ignore-case nil))
    (should (equal
	     (sort (uniq-file-all-completions "" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      "foo-file1.text<>"
	      "foo-file1.text<Alice/alice-1/>"
	      "foo-file1.text<Alice/alice-2/>"
	      "foo-file1.text<Bob/bob-1/>"
	      "foo-file1.text<Bob/bob-2/>"
	      "foo-file2.text<Alice/alice-1/>"
	      "foo-file2.text<Bob/bob-1/>"
	      "foo-file3.text"
	      "foo-file3.texts"
	      "foo-file3.texts2"
	      "foo-file4.text<Alice/alice-3/>"
	      "foo-file4.text<Bob/alice-3/>"
	      "foo-file5.text"
              "wisitoken-generate-packrat-test.text"
              "wisitoken-syntax_trees-test.text"
              "wisitoken-text_io_trace.text"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "*-fi" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      "foo-file1.text<>"
	      "foo-file1.text<Alice/alice-1/>"
	      "foo-file1.text<Alice/alice-2/>"
	      "foo-file1.text<Bob/bob-1/>"
	      "foo-file1.text<Bob/bob-2/>"
	      "foo-file2.text<Alice/alice-1/>"
	      "foo-file2.text<Bob/bob-1/>"
	      "foo-file3.text"
	      "foo-file3.texts"
	      "foo-file3.texts2"
	      "foo-file4.text<Alice/alice-3/>"
	      "foo-file4.text<Bob/alice-3/>"
	      "foo-file5.text"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "a" table nil nil) #'string-lessp)
	     ;; Should _not_ match directory names
	     nil))

    (should (equal
	     (sort (uniq-file-all-completions "b" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "foo" table nil nil) #'string-lessp)
	     (list
	      "foo-file1.text<>"
	      "foo-file1.text<Alice/alice-1/>"
	      "foo-file1.text<Alice/alice-2/>"
	      "foo-file1.text<Bob/bob-1/>"
	      "foo-file1.text<Bob/bob-2/>"
	      "foo-file2.text<Alice/alice-1/>"
	      "foo-file2.text<Bob/bob-1/>"
	      "foo-file3.text"
	      "foo-file3.texts"
	      "foo-file3.texts2"
	      "foo-file4.text<Alice/alice-3/>"
	      "foo-file4.text<Bob/alice-3/>"
	      "foo-file5.text"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "f-file2" table nil nil) #'string-lessp)
	     (list
	      "foo-file2.text<Alice/alice-1/>"
	      "foo-file2.text<Bob/bob-1/>"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "b-fi<" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "f-file<" table nil nil) #'string-lessp)
	     (list
	      "foo-file1.text<>"
	      "foo-file1.text<Alice/alice-1/>"
	      "foo-file1.text<Alice/alice-2/>"
	      "foo-file1.text<Bob/bob-1/>"
	      "foo-file1.text<Bob/bob-2/>"
	      "foo-file2.text<Alice/alice-1/>"
	      "foo-file2.text<Bob/bob-1/>"
	      "foo-file3.text"
	      "foo-file3.texts"
	      "foo-file3.texts2"
	      "foo-file4.text<Alice/alice-3/>"
	      "foo-file4.text<Bob/alice-3/>"
	      "foo-file5.text"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a-" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a-1" table nil nil) #'string-lessp)
	     (list "bar-file1.text<alice-1/>"
		   "bar-file2.text<alice-1/>")))

    (should (equal (uniq-file-all-completions "f-file1.text<a-1" table nil nil)
		   (list "foo-file1.text<Alice/alice-1/>")))

    (should (equal (sort (uniq-file-all-completions "f-file1.text<al" table nil nil) #'string-lessp)
		   (list
		    "foo-file1.text<Alice/alice-1/>"
		    "foo-file1.text<Alice/alice-2/>")))

    (should (equal (sort (uniq-file-all-completions "f-file4.text<a-3" table nil nil) #'string-lessp)
		   (list
		    "foo-file4.text<Alice/alice-3/>"
		    "foo-file4.text<Bob/alice-3/>")))

    (should (equal (sort (uniq-file-all-completions "foo-file4.text<Bob" table nil nil) #'string-lessp)
		   (list
		    "foo-file4.text<Bob/alice-3/>")))

    (should (equal (uniq-file-all-completions "f-file5" table nil nil)
		   (list "foo-file5.text")))

    (should (equal (uniq-file-all-completions "foo-file1.text<Alice/alice-1/>" table nil nil)
		   (list "foo-file1.text<Alice/alice-1/>")))

    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a>" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<alice-1/>"
	      "bar-file1.text<alice-2/>"
	      "bar-file2.text<alice-1/>"
	      "bar-file2.text<alice-2/>"
	      )))

    (should (equal
	     (sort (uniq-file-all-completions "foo-file1.text<>" table nil nil) #'string-lessp)
	     ;; This is complete but not unique, because the directory part matches multiple directories.
	     (list
	      "foo-file1.text<>"
	      "foo-file1.text<Alice/alice-1/>"
	      "foo-file1.text<Alice/alice-2/>"
	      "foo-file1.text<Bob/bob-1/>"
	      "foo-file1.text<Bob/bob-2/>"
	      )))
    ))

(defun test-uniq-file-hilit (pos-list string)
  "Set 'face text property to 'completions-first-difference at
all positions in POS-LIST in STRING; return new string."
  (while pos-list
    (let ((pos (pop pos-list)))
      (put-text-property pos (1+ pos) 'face 'completions-first-difference string)))
  string)

(ert-deftest test-uniq-file-all-completions-face ()
  ;; `all-completions' tested above without considering face text
  ;; properties; here we test just those properties. Test cases are
  ;; the same as above.
  ;;
  ;; WORKAROUND: byte-compiling this test makes it fail; it appears to be
  ;; sharing strings that should not be shared because they have
  ;; different text properties.
  (let ((table (uft-table))
	(completion-ignore-case nil))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "b" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(8) "bar-file1.text<alice-1/>")
	      (test-uniq-file-hilit '(8) "bar-file1.text<alice-2/>")
	      (test-uniq-file-hilit '(8) "bar-file2.text<alice-1/>")
	      (test-uniq-file-hilit '(8) "bar-file2.text<alice-2/>")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "foo" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(8) "foo-file1.text<>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Alice/alice-2/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Bob/bob-2/>")
	      (test-uniq-file-hilit '(8) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(8) "foo-file2.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(8) "foo-file3.text")
	      (test-uniq-file-hilit '(8) "foo-file3.texts")
	      (test-uniq-file-hilit '(8) "foo-file3.texts2")
	      (test-uniq-file-hilit '(8) "foo-file4.text<Alice/alice-3/>")
	      (test-uniq-file-hilit '(8) "foo-file4.text<Bob/alice-3/>")
	      (test-uniq-file-hilit '(8) "foo-file5.text")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "f-file2" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(15) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(15) "foo-file2.text<Bob/bob-1/>")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "foo-file3.text" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '()   "foo-file3.text")
	      (test-uniq-file-hilit '(14) "foo-file3.texts")
	      (test-uniq-file-hilit '(14) "foo-file3.texts2")
	      )))

    ;; Two places for possible completion, with different intervening text
    (should (equal-including-properties
	     (sort (uniq-file-all-completions "wisi-te" table nil 5) #'string-lessp)
	     (list                         ;; 0         10        20        30
	      (test-uniq-file-hilit '(10 18) "wisitoken-generate-packrat-test.text")
	      (test-uniq-file-hilit '(10 25) "wisitoken-syntax_trees-test.text")
	      (test-uniq-file-hilit '(10 12) "wisitoken-text_io_trace.text")
	      )))
    ))

(ert-deftest test-uniq-file-try-completion ()
  (let ((table (uft-table))
	(completion-ignore-case nil)
        string)

    (setq string "fo")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file" . 8)))

    (setq string "b")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file" . 8)))

    (setq string "fo<al")
    (should (equal (uniq-file-try-completion string table nil 2)
		   '("foo-file.text<alice-" . 8)))
    (should (equal (uniq-file-try-completion string table nil 5)
		   '("foo-file<alice-" . 15)))

    (let ((completion-ignore-case t))
      (setq string "fo<al")
      (should (equal (uniq-file-try-completion string table nil 2)
		     '("foo-file.text<alice" . 8)))
      (should (equal (uniq-file-try-completion string table nil 5)
		     '("foo-file<alice" . 14)))
      )

    (setq string "foo-file3") ;; not unique, not valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file3.text" . 14)))

    (setq string "f-file1.text<a-1")
    ;; Not unique, because "a" accidentally matches "packages" in
    ;; uft-root-dir, and "-" covers "/".  Also not valid.
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file1.text<Alice/alice-1/>" . 30)))

    (setq string "foo-file1.text") ;; valid but not unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<" 15)))

    (setq string "foo-file1<") ;; not valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<" 15)))

    (setq string "foo-file1.text<>") ;; valid but not unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<>" 15)))

    (setq string "foo-file1.text<Alice/alice-1/>") ;; valid and unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   t))

    (setq string "foo-file3.texts") ;; not unique, valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file3.texts" . 15)))

    (setq string "foo-file3.texts2") ;; unique and valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   t))

    (setq string "fil2") ;; misspelled
    (should (equal (uniq-file-try-completion string table nil (length string))
		   nil))

    (setq string "b-file2")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; prev + <tab>; input is prev output
    (setq string "bar-file2.text<alice-")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; prev + <tab>; input is prev output
    (setq string "bar-file2.text<alice-")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; completion-try-completion called from icomplete-completions with
    ;; result of all-completions instead of table function.
    (setq string "f-file<")
    (let ((comps (uniq-file-all-completions string table nil nil)))
      (should (equal (uniq-file-try-completion string comps nil (length string))
		     (cons "foo-file" 8))))
    ))

(ert-deftest test-uniq-file-uniquify ()
  (should (equal (uniq-file-uniquify
		  '("/Alice/alice1/file1.text"
                    "/Alice/alice1/file2.text"
		    "/Alice/alice2/file1.text"
                    "/Alice/alice2/file3.text"
		    "/Bob/bob1/file1.text"))
		 (list
		  '("file3.text"                . "/Alice/alice2/file3.text")
		  '("file2.text"                . "/Alice/alice1/file2.text")
		  '("file1.text<Bob/bob1/>"     . "/Bob/bob1/file1.text")
		  '("file1.text<Alice/alice2/>" . "/Alice/alice2/file1.text")
                  '("file1.text<Alice/alice1/>" . "/Alice/alice1/file1.text")
                  )))

  (should (equal (uniq-file-uniquify
		  (list
		   (concat uft-alice1 "/foo-file1.text")
		   (concat uft-alice2 "/foo-file1.text")
		   (concat uft-bob1 "/foo-file1.text")
		   (concat uft-bob2 "/foo-file1.text")
		   (concat uft-root "/foo-file1.text")
		   ))
		 (list
		  (cons "foo-file1.text<>"               (concat uft-root "/foo-file1.text"))
		  (cons "foo-file1.text<Bob/bob-2/>"     (concat uft-bob2 "/foo-file1.text"))
                  (cons "foo-file1.text<Bob/bob-1/>"     (concat uft-bob1 "/foo-file1.text"))
                  (cons "foo-file1.text<Alice/alice-2/>" (concat uft-alice2 "/foo-file1.text"))
		  (cons "foo-file1.text<Alice/alice-1/>" (concat uft-alice1 "/foo-file1.text"))
                  )))

  (should (equal (uniq-file-uniquify
		  (list
		   (concat uft-alice1 "/bar-file1.c")
		   (concat uft-alice1 "/bar-file2.c")
		   (concat uft-alice2 "/bar-file1.c")
		   (concat uft-alice2 "/bar-file2.c")
		   (concat uft-bob1 "/foo-file1.c")
		   (concat uft-bob1 "/foo-file2.c")
		   (concat uft-bob2 "/foo-file1.c")
		   (concat uft-bob2 "/foo-file5.c")
		   ))
		 (list
                  (cons "foo-file5.c"           (concat uft-bob2 "/foo-file5.c"))
		  (cons "foo-file2.c"           (concat uft-bob1 "/foo-file2.c"))
                  (cons "foo-file1.c<bob-2/>"   (concat uft-bob2 "/foo-file1.c"))
                  (cons "foo-file1.c<bob-1/>"   (concat uft-bob1 "/foo-file1.c"))
                  (cons "bar-file2.c<alice-2/>" (concat uft-alice2 "/bar-file2.c"))
                  (cons "bar-file2.c<alice-1/>" (concat uft-alice1 "/bar-file2.c"))
                  (cons "bar-file1.c<alice-2/>" (concat uft-alice2 "/bar-file1.c"))
                  (cons "bar-file1.c<alice-1/>" (concat uft-alice1 "/bar-file1.c"))
                  )))
  )

(provide 'uniquify-files-test)
;;; uniquify-files-test.el ends here
