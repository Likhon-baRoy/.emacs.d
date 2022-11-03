;;; uniquify-files.el --- Completion style for files, minimizing directories  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019, 2020, 2022  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: completion table
;;   uniquify
;; Version: 1.0.4
;; package-requires: ((emacs "25.0"))
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

;;; Commentary

;; A file completion style in which the completion string displayed to
;; the user consists of the file basename followed by enough of the
;; directory part to make the string identify a unique file.
;;
;; We accomplish this by preprocessing the list of absolute file names
;; to be in that style, in an alist with the original absolute file
;; names, and do completion on that alist.
;;
;; To use it with `project-find-file', customize
;; `project-read-file-name-function':
;;
;; (setq project-read-file-name-function 'uniq-file-read)

(require 'cl-lib)
(require 'files)
(require 'project)

(defconst uniq-file--regexp "^\\(.*\\)<\\([^>]*\\)>?$"
  ;; The trailing '>' is optional so the user can type "<dir" in the
  ;; input buffer to complete directories.
  "Regexp matching uniqufied file name.
Match 1 is the filename, match 2 is the relative directory.")

(defun uniq-file-conflicts (conflicts)
  "Subroutine of `uniq-file-uniquify'."
  (let ((common-root ;; shared prefix of dirs in conflicts - may be nil
	 (fill-common-string-prefix (file-name-directory (nth 0 conflicts)) (file-name-directory (nth 1 conflicts)))))

    (let ((temp (cddr conflicts)))
      (while (and common-root
		  temp)
	(setq common-root (fill-common-string-prefix common-root (file-name-directory (pop temp))))))

    (when common-root
      ;; Trim `common-root' back to last '/'
      (let ((i (1- (length common-root))))
	(while (and (> i 0)
		    (not (= (aref common-root i) ?/)))
	  (setq i (1- i)))
	(setq common-root (substring common-root 0 (1+ i)))))

    (cl-mapcar
     (lambda (name)
	 (cons (concat (file-name-nondirectory name)
                       "<"
                       (substring (file-name-directory name) (length common-root))
                       ">")
               name))
     conflicts)
    ))

(defun uniq-file-uniquify (names)
  "Return an alist of uniquified names built from NAMES.
NAMES is a list containing absolute file names.

The result contains file basenames with partial directory paths
appended."
  (let ((case-fold-search completion-ignore-case)
        result
	conflicts ;; list of names where all non-directory names are the same.
	)

    ;; Sort names on basename so duplicates are grouped together
    (setq names (sort names (lambda (a b)
			      (string< (file-name-nondirectory a) (file-name-nondirectory b)))))

    (while names
      (setq conflicts (list (pop names)))
      (while (and names
		  (string= (file-name-nondirectory (car conflicts)) (file-name-nondirectory (car names))))
	(push (pop names) conflicts))

      (if (= 1 (length conflicts))
	  (push (cons
		 (concat (file-name-nondirectory (car conflicts)))
		 (car conflicts))
		result)

        (setq result (append (uniq-file-conflicts conflicts) result)))
      )
    result))

(defun uniq-file--pcm-pat (string point)
  "Return a pcm pattern that matches STRING (a uniquified file name)."
  (let* ((completion-pcm--delim-wild-regex
	  (concat "[" completion-pcm-word-delimiters "<>*]"))
	 ;; If STRING ends in an empty directory part, some valid
	 ;; completions won't have any directory part.
	 (trimmed-string
	  (if (and (< 0 (length string))
		   (= (aref string (1- (length string))) ?<))
	      (substring string 0 -1)
	    string))
	 dir-start
	 (pattern (completion-pcm--string->pattern trimmed-string point)))

    ;; If trimmed-string has a directory part, allow uniquifying
    ;; directories.
    (when (and (setq dir-start (string-match "<" trimmed-string))
	       (< dir-start (1- (length trimmed-string))))
      (let (new-pattern
	    item)
	(while pattern
	  (setq item (pop pattern))
	  (push item new-pattern)
	  (when (equal item "<")
	    (setq item (pop pattern))
	    (if (eq item 'any-delim)
		(push 'any new-pattern)
	      (push item new-pattern))))
	(setq pattern (nreverse new-pattern))))
    pattern))

(defun uniq-file--pcm-merged-pat (string all point)
  "Return a pcm pattern that is the merged completion of STRING in ALL.
ALL must be a list of uniquified file names.
Pattern is in reverse order."
  (let* ((pattern (uniq-file--pcm-pat string point)))
    (completion-pcm--merge-completions all pattern)))

(defun uniq-file-try-completion (user-string table pred point)
  "Implement `completion-try-completion' for uniquify-file."
  (let (result
	uniq-all
	done)

    ;; Compute result or uniq-all, set done.
    (cond
     ((functionp table) ;; TABLE is a wrapper function that calls uniq-file-completion-table.

      (setq uniq-all (uniq-file-all-completions user-string table pred point))

      (cond
       ((null uniq-all) ;; No matches.
	(setq result nil)
	(setq done t))

       ((= 1 (length uniq-all)) ;; One match; unique.
	(setq done t)

	;; Check for valid completion
	(if (string-equal user-string (car uniq-all))
	    (setq result t)

	  (setq result (car uniq-all))
	  (setq result (cons result (length result)))))

       (t ;; Multiple matches
	(setq done nil))
       ))

     ;; The following cases handle being called from
     ;; icomplete-completions with the result of `all-completions'
     ;; instead of the real table function. TABLE is a list of
     ;; uniquified file names.

     ((null table) ;; No matches.
      (setq result nil)
      (setq done t))

     (t ;; TABLE is a list of uniquified file names
      (setq uniq-all table)
      (setq done nil))
     )

    (if done
	result

      ;; Find merged completion of uniqified file names
      (let* ((merged-pat (uniq-file--pcm-merged-pat user-string uniq-all point))

	     ;; `merged-pat' is in reverse order.  Place new point at:
	     (point-pat (or (memq 'point merged-pat) ;; the old point
			    (memq 'any   merged-pat) ;; a place where there's something to choose
			    (memq 'star  merged-pat) ;; ""
			    merged-pat))             ;; the end

	     ;; `merged-pat' does not contain 'point when the field
	     ;; containing 'point is fully completed.

	     (new-point (length (completion-pcm--pattern->string point-pat)))

	     ;; Compute this after `new-point' because `nreverse'
	     ;; changes `point-pat' by side effect.
	     (merged (completion-pcm--pattern->string (nreverse merged-pat))))

	(cons merged new-point)))
    ))

(defun uniq-file--hilit (string all point)
  "Apply face text properties to each element of ALL.
STRING is the current user input.
ALL is a list of strings in user format.
POINT is the position of point in STRING.
Returns new list.

Adds the face `completions-first-difference' to the first
character after each completion field."
  (let* ((merged-pat (nreverse (uniq-file--pcm-merged-pat string all point)))
	 (field-count 0)
	 (regex (completion-pcm--pattern->regex merged-pat '(any star any-delim point)))
	 )
    (dolist (x merged-pat)
      (when (not (stringp x))
	(setq field-count (1+ field-count))))

    (mapcar
     (lambda (str)
       ;; First remove previously applied face; `str' may be a reference
       ;; to a list used in a previous completion.
       (remove-text-properties 0 (length str) '(face completions-first-difference) str)
       (when (string-match regex str)
	 (cl-loop
	  for i from 1 to field-count
	  do
	  (when (and
		 (match-beginning i)
		 (<= (1+ (match-beginning i)) (length str)))
	    (put-text-property (match-beginning i) (1+ (match-beginning i)) 'face 'completions-first-difference str))
	  ))
       str)
     all)))

(defun uniq-file-all-completions (string table pred point)
  "Implement `completion-all-completions' for uniquify-file."
  ;; Returns list of data format strings (abs file names).
  (let ((all (all-completions string table pred)))
    (when all
      (uniq-file--hilit string all point))
    ))

(defun uniq-file-completion-table (files string pred action)
  "Implement a completion table for uniquified file names in FILES.
FILES is an alist of (UNIQIFIED-NAME . ABS-NAME).  Completion is
done on UNIQIFIED-NAME, PRED is called with ABS-NAME."
  (cond
   ((eq action 'alist)
    (cdr (assoc string files)))

   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
           ;; category controls what completion styles are appropriate.
	   '(category . uniquify-file)
	   )))

   ((memq action
	  '(nil    ;; Called from `try-completion'
	    lambda ;; Called from `test-completion'
	    t))    ;; Called from all-completions

    (let ((regex (completion-pcm--pattern->regex
                  (uniq-file--pcm-pat string (length string))))
	  (case-fold-search completion-ignore-case)
	  (result nil))
      (dolist (pair files)
	(when (and
	       (string-match regex (car pair))
	       (or (null pred)
		   (funcall pred (cdr pair))))
	  (push (car pair) result)))

      (cond
       ((null action)
	(try-completion string result))

       ((eq 'lambda action)
	(test-completion string files pred))

       ((eq t action)
	result)
       )))
   ))

(add-to-list 'completion-styles-alist
	     '(uniquify-file
	       uniq-file-try-completion
	       uniq-file-all-completions
	       "display uniquified file names."))

;;; Integration with emacs 27 project.el

;;;###autoload
(defun uniq-file-read (prompt all-files &optional predicate hist default)
  "For `project-read-file-name-function'."
  (let* ((alist (uniq-file-uniquify all-files))
         (table (apply-partially #'uniq-file-completion-table alist))
         (found (project--completing-read-strict
                 prompt table predicate hist default)))
    (cdr (assoc found alist))))

(provide 'uniquify-files)
;;; uniquify-files.el ends here
