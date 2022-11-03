;;; compat-27.el --- Compatibility Layer for Emacs 27.1  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Compat Development <~pkal/compat-devel@lists.sr.ht>
;; URL: https://git.sr.ht/~pkal/compat/
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Find here the functionality added in Emacs 27.1, needed by older
;; versions.
;;
;; Only load this library if you need to use one of the following
;; functions or macros:
;;
;; - `compat-recenter'
;; - `compat-lookup-key'
;; - `compat-setq-local'
;; - `compat-assoc-delete-all'
;; - `compat-file-size-human-readable'
;; - `compat-executable-find'
;; - `compat-regexp-opt'
;; - `compat-dired-get-marked-files'

;;; Code:

(require 'compat-macs "compat-macs.el")

(compat-declare-version "27.1")

;;;; Defined in fns.c

(compat-defun proper-list-p (object)
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  :min-version "26.1"
  :max-version "26.3"
  :realname compat--proper-list-p-length-signal
  (condition-case nil
      (and (listp object) (length object))
    (wrong-type-argument nil)
    (circular-list nil)))

(compat-defun proper-list-p (object)
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  :max-version "25.3"
  :realname compat--proper-list-p-tortoise-hare
  (when (listp object)
    (catch 'cycle
      (let ((hare object) (tortoise object)
            (max 2) (q 2))
        (while (consp hare)
          (setq hare (cdr hare))
          (when (and (or (/= 0 (setq q (1- q)))
                         (ignore
                          (setq max (ash max 1)
                                q max
                                tortoise hare)))
                     (eq hare tortoise))
            (throw 'cycle nil)))
        (and (null hare) (length object))))))

(compat-defun string-distance (string1 string2 &optional bytecompare)
  "Return Levenshtein distance between STRING1 and STRING2.
The distance is the number of deletions, insertions, and substitutions
required to transform STRING1 into STRING2.
If BYTECOMPARE is nil or omitted, compute distance in terms of characters.
If BYTECOMPARE is non-nil, compute distance in terms of bytes.
Letter-case is significant, but text properties are ignored."
  ;; https://en.wikipedia.org/wiki/Levenshtein_distance
  (let ((s1 (if bytecompare
                (encode-coding-string string1 'raw-text)
              (concat string1 "")))
        (s2 (if bytecompare
                (encode-coding-string string2 'raw-text)
              string2)))
    (let* ((len1 (length s1))
           (len2 (length s2))
           (column (make-vector (1+ len1) 0)))
      (dotimes (y len1)
        (setf (aref column (1+ y)) y))
      (dotimes (x len2)
        (setf (aref column 0) (1+ x))
        (let ((lastdiag x) olddiag)
          (dotimes (y len1)
            (setf olddiag (aref column (1+ y))
                  (aref column (1+ y))
                  (min (+ (if (= (aref s1 y) (aref s2 x)) 0 1)
                          lastdiag)
                       (1+ (aref column (1+ y)))
                       (1+ (aref column y)))
                  lastdiag olddiag))))
      (aref column len1))))

;;;; Defined in window.c

(compat-defun recenter (&optional arg redisplay)
  "Handle optional argument REDISPLAY."
  :prefix t
  (recenter arg)
  (when (and redisplay recenter-redisplay)
    (redisplay)))

;;;; Defined in keymap.c

(compat-defun lookup-key (keymap key &optional accept-default)
  "Allow for KEYMAP to be a list of keymaps."
  :prefix t
  (cond
   ((keymapp keymap)
    (lookup-key keymap key accept-default))
   ((listp keymap)
    (catch 'found
      (dolist (map keymap)
        (let ((fn (lookup-key map key accept-default)))
          (when fn (throw 'found fn))))))
   ((signal 'wrong-type-argument (list 'keymapp keymap)))))

;;;; Defined in json.c

(declare-function json-parse-string nil (string &rest args))
(declare-function json-encode "json" (object))
(declare-function json-read-from-string "json" (string))
(declare-function json-read "json" ())
(defvar json-encoding-pretty-print)
(defvar json-object-type)
(defvar json-array-type)
(defvar json-false)
(defvar json-null)

;; The function is declared to satisfy the byte compiler while testing
;; if native JSON parsing is available.;
(declare-function json-serialize nil (object &rest args))
(compat-defun json-serialize (object &rest args)
  "Return the JSON representation of OBJECT as a string.

OBJECT must be t, a number, string, vector, hashtable, alist, plist,
or the Lisp equivalents to the JSON null and false values, and its
elements must recursively consist of the same kinds of values.  t will
be converted to the JSON true value.  Vectors will be converted to
JSON arrays, whereas hashtables, alists and plists are converted to
JSON objects.  Hashtable keys must be strings without embedded null
characters and must be unique within each object.  Alist and plist
keys must be symbols; if a key is duplicate, the first instance is
used.

The Lisp equivalents to the JSON null and false values are
configurable in the arguments ARGS, a list of keyword/argument pairs:

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'.

In you specify the same value for `:null-object' and `:false-object',
a potentially ambiguous situation, the JSON output will not contain
any JSON false values."
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  :realname compat--json-serialize
  (require 'json)
  (letrec ((fix (lambda (obj)
                  (cond
                   ((hash-table-p obj)
                    (let ((ht (copy-hash-table obj)))
                      (maphash
                       (lambda (key val)
                         (unless (stringp key)
                           (signal
                            'wrong-type-argument
                            (list 'stringp key)))
                         (puthash key (funcall fix val) ht))
                       obj)
                      ht))
                   ((and (listp obj) (consp (car obj))) ;alist
                    (mapcar
                     (lambda (ent)
                       (cons (symbol-name (car ent))
                             (funcall fix (cdr ent))))
                     obj))
                   ((listp obj) ;plist
                    (let (alist)
                      (while obj
                        (push (cons (cond
                                     ((keywordp (car obj))
                                      (substring
                                       (symbol-name (car obj))
                                       1))
                                     ((symbolp (car obj))
                                      (symbol-name (car obj)))
                                     ((signal
                                       'wrong-type-argument
                                       (list 'symbolp (car obj)))))
                                    (funcall fix (cadr obj)))
                              alist)
                        (unless (consp (cdr obj))
                          (signal 'wrong-type-argument '(consp nil)))
                        (setq obj (cddr obj)))
                      (nreverse alist)))
                   ((vectorp obj)
                    (let ((vec (make-vector (length obj) nil)))
                      (dotimes (i (length obj))
                        (aset vec i (funcall fix (aref obj i))))
                      vec))
                   (obj))))
           (json-encoding-pretty-print nil)
           (json-false (or (plist-get args :false-object) :false))
           (json-null (or (plist-get args :null-object) :null)))
    (json-encode (funcall fix object))))

(compat-defun json-insert (object &rest args)
  "Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT."
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (insert (apply #'compat--json-serialize object args)))

(compat-defun json-parse-string (string &rest args)
  "Parse the JSON STRING into a Lisp object.
This is essentially the reverse operation of `json-serialize', which
see.  The returned object will be the JSON null value, the JSON false
value, t, a number, a string, a vector, a list, a hashtable, an alist,
or a plist.  Its elements will be further objects of these types.  If
there are duplicate keys in an object, all but the last one are
ignored.  If STRING doesn't contain a valid JSON object, this function
signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (require 'json)
  (condition-case err
      (let ((json-object-type (or (plist-get args :object-type) 'hash-table))
            (json-array-type (or (plist-get args :array-type) 'vector))
            (json-false (or (plist-get args :false-object) :false))
            (json-null (or (plist-get args :null-object) :null)))
        (when (eq json-array-type 'array)
          (setq json-array-type 'vector))
        (json-read-from-string string))
    (json-error (signal 'json-parse-error err))))

(compat-defun json-parse-buffer (&rest args)
  "Read JSON object from current buffer starting at point.
Move point after the end of the object if parsing was successful.
On error, don't move point.

The returned object will be a vector, list, hashtable, alist, or
plist.  Its elements will be the JSON null value, the JSON false
value, t, numbers, strings, or further vectors, lists, hashtables,
alists, or plists.  If there are duplicate keys in an object, all
but the last one are ignored.

If the current buffer doesn't contain a valid JSON object, the
function signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (require 'json)
  (condition-case err
      (let ((json-object-type (or (plist-get args :object-type) 'hash-table))
            (json-array-type (or (plist-get args :array-type) 'vector))
            (json-false (or (plist-get args :false-object) :false))
            (json-null (or (plist-get args :null-object) :null)))
        (when (eq json-array-type 'array)
          (setq json-array-type 'vector))
        (json-read))
    (json-error (signal 'json-parse-buffer err))))

;;;; Defined in timefns.c

(compat-defun time-equal-p (t1 t2)
  "Return non-nil if time value T1 is equal to time value T2.
A nil value for either argument stands for the current time."
  :note "This function is not as accurate as the actual `time-equal-p'."
  (cond
   ((eq t1 t2))
   ((and (consp t1) (consp t2))
    (equal t1 t2))
   ((let ((now (current-time)))
      ;; Due to inaccuracies and the relatively slow evaluating of
      ;; Emacs Lisp compared to C, we allow for slight inaccuracies
      ;; (less than a millisecond) when comparing time values.
      (< (abs (- (float-time (or t1 now))
                 (float-time (or t2 now))))
         1e-5)))))

;;;; Defined in fileio.c

(compat-defun file-name-absolute-p (filename)
  "Return t if FILENAME is an absolute file name.
On Unix, absolute file names start with `/'.  In Emacs, an absolute
file name can also start with an initial `~' or `~USER' component,
where USER is a valid login name."
  ;; See definitions in filename.h
  (let ((seperator
         (eval-when-compile
           (if (memq system-type '(cygwin windows-nt ms-dos))
               "[\\/]" "/")))
        (drive
         (eval-when-compile
           (cond
            ((memq system-type '(windows-nt ms-dos))
             "\\`[A-Za-z]:[\\/]")
            ((eq system-type 'cygwin)
             "\\`\\([\\/]\\|[A-Za-z]:\\)")
            ("\\`/"))))
        (home
         (eval-when-compile
           (if (memq system-type '(cygwin windows-nt ms-dos))
               "\\`~[\\/]" "\\`~/")))
        (user-home
         (eval-when-compile
           (format "\\`\\(~.*?\\)\\(%s.*\\)?$"
                   (if (memq system-type '(cygwin windows-nt ms-dos))
                       "[\\/]" "/")))))
    (or (and (string-match-p drive filename) t)
        (and (string-match-p home filename) t)
        (save-excursion
          (when (string-match user-home filename)
            (let ((init (match-string 1 filename)))
              (not (string=
                    (file-name-base (expand-file-name init))
                    init))))))))

;;;; Defined in subr.el

(compat-defmacro setq-local (&rest pairs)
  "Handle multiple assignments."
  :prefix t
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let (body)
    (while pairs
      (let* ((sym (pop pairs))
             (val (pop pairs)))
        (unless (symbolp sym)
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        (push `(set (make-local-variable ,sym) ,val)
              body)))
    (cons 'progn (nreverse body))))

(compat-defun provided-mode-derived-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
  :realname compat--provided-mode-derived-p
  ;; If MODE is an alias, then look up the real mode function first.
  (let ((alias (symbol-function mode)))
    (when (and alias (symbolp alias))
      (setq mode alias)))
  (while
      (and
       (not (memq mode modes))
       (let* ((parent (get mode 'derived-mode-parent))
              (parentfn (symbol-function parent)))
         (setq mode (if (and parentfn (symbolp parentfn)) parentfn parent)))))
  mode)

;;* UNTESTED
(defun derived-mode-p (&rest modes)
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (apply #'compat--provided-mode-derived-p major-mode modes))

;;* UNTESTED
(compat-defmacro ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

;;* UNTESTED
(compat-defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body)
  "Loop over a list and report progress in the echo area.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((prep (make-symbol "--dolist-progress-reporter--"))
        (count (make-symbol "--dolist-count--"))
        (list (make-symbol "--dolist-list--")))
    `(let ((,prep ,reporter-or-message)
           (,count 0)
           (,list ,(cadr spec)))
       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 (1- (length ,list)))))
       (dolist (,(car spec) ,list)
         ,@body
         (progress-reporter-update ,prep (setq ,count (1+ ,count))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))

(compat-defun flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(compat-defun xor (cond1 cond2)
  "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
  (declare (pure t) (side-effect-free error-free))
  (cond ((not cond1) cond2)
        ((not cond2) cond1)))

(compat-defvar regexp-unmatchable "\\`a\\`"
  "Standard regexp guaranteed not to match any string at all."
  :constant t)

(compat-defun assoc-delete-all (key alist &optional test)
  "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  :prefix t
  (unless test (setq test #'equal))
  (while (and (consp (car alist))
	      (funcall test (caar alist) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (funcall test (caar tail-cdr) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

;;;; Defined in simple.el

;;* UNTESTED
(compat-defun decoded-time-second (time)
  "The seconds in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 60 (inclusive).  (60 is a leap
second, which only some operating systems support.)"
  (nth 0 time))

;;* UNTESTED
(compat-defun decoded-time-minute (time)
  "The minutes in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 59 (inclusive)."
  (nth 1 time))

;;* UNTESTED
(compat-defun decoded-time-hour (time)
  "The hours in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 23 (inclusive)."
  (nth 2 time))

;;* UNTESTED
(compat-defun decoded-time-day (time)
  "The day-of-the-month in TIME, which is a value returned by `decode-time'.
This is an integer between 1 and 31 (inclusive)."
  (nth 3 time))

;;* UNTESTED
(compat-defun decoded-time-month (time)
  "The month in TIME, which is a value returned by `decode-time'.
This is an integer between 1 and 12 (inclusive).  January is 1."
  (nth 4 time))

;;* UNTESTED
(compat-defun decoded-time-year (time)
  "The year in TIME, which is a value returned by `decode-time'.
This is a four digit integer."
  (nth 5 time))

;;* UNTESTED
(compat-defun decoded-time-weekday (time)
  "The day-of-the-week in TIME, which is a value returned by `decode-time'.
This is a number between 0 and 6, and 0 is Sunday."
  (nth 6 time))

;;* UNTESTED
(compat-defun decoded-time-dst (time)
  "The daylight saving time in TIME, which is a value returned by `decode-time'.
This is t if daylight saving time is in effect, and nil if not."
  (nth 7 time))

;;* UNTESTED
(compat-defun decoded-time-zone (time)
  "The time zone in TIME, which is a value returned by `decode-time'.
This is an integer indicating the UTC offset in seconds, i.e.,
the number of seconds east of Greenwich."
  (nth 8 time))

;; TODO define gv-setters

;;;; Defined in files.el

(compat-defun file-size-human-readable (file-size &optional flavor space unit)
  "Handle the optional third and forth argument:

Optional third argument SPACE is a string put between the number and unit.
It defaults to the empty string.  We recommend a single space or
non-breaking space, unless other constraints prohibit a space in that
position.

Optional fourth argument UNIT is the unit to use.  It defaults to \"B\"
when FLAVOR is `iec' and the empty string otherwise.  We recommend \"B\"
in all cases, since that is the standard symbol for byte."
  :prefix t
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                 1000.0))
        (prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr prefixes))
      (setq file-size (/ file-size power)
            prefixes (cdr prefixes)))
    (let* ((prefix (car prefixes))
           (prefixed-unit (if (eq flavor 'iec)
                              (concat
                               (if (string= prefix "k") "K" prefix)
                               (if (string= prefix "") "" "i")
                               (or unit "B"))
                            (concat prefix unit))))
      (format (if (and (>= (mod file-size 1.0) 0.05)
                       (< (mod file-size 1.0) 0.95))
                  "%.1f%s%s"
                "%.0f%s%s")
              file-size
              (if (string= prefixed-unit "") "" (or space ""))
              prefixed-unit))))

(declare-function compat--file-name-quote "compat-26"
                  (name &optional top))

;;*UNTESTED
(compat-defun exec-path ()
  "Return list of directories to search programs to run in remote subprocesses.
The remote host is identified by `default-directory'.  For remote
hosts that do not support subprocesses, this returns nil.
If `default-directory' is a local directory, this function returns
the value of the variable `exec-path'."
  :realname compat--exec-path
  (cond
   ((let ((handler (find-file-name-handler default-directory 'exec-path)))
      ;; FIXME: The handler was added in 27.1, and this compatibility
      ;; function only applies to versions of Emacs before that.
      (when handler
        (condition-case nil
            (funcall handler 'exec-path)
          (error nil)))))
   ((file-remote-p default-directory)
    ;; TODO: This is not completely portable, even if "sh" and
    ;; "getconf" should be provided on every POSIX system, the chance
    ;; of this not working are greater than zero.
    ;;
    ;; FIXME: This invokes a shell process every time exec-path is
    ;; called.  It should instead be cached on a host-local basis.
    (with-temp-buffer
      (if (condition-case nil
              (zerop (process-file "sh" nil t nil "-c" "getconf PATH"))
            (file-missing t))
          (list "/bin" "/usr/bin")
        (let (path)
          (while (re-search-forward "\\([^:]+?\\)[\n:]" nil t)
            (push (match-string 1) path))
          (nreverse path)))))
   (exec-path)))

(declare-function compat--file-local-name "compat-26"
                  (file))

;;*UNTESTED
(compat-defun executable-find (command &optional remote)
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'.  If
REMOTE is non-nil, search on the remote host indicated by
`default-directory' instead."
  :prefix t
  (if (and remote (file-remote-p default-directory))
      (let ((res (locate-file
                  command
                  (mapcar
                   (apply-partially
                    #'concat (file-remote-p default-directory))
                   (compat--exec-path))
                  exec-suffixes 'file-executable-p)))
        (when (stringp res) (compat--file-local-name res)))
    (executable-find command)))

;; TODO provide advice for directory-files-recursively

;;;; Defined in format-spec.el

;; TODO provide advice for format-spec

;;;; Defined in regexp-opt.el

(compat-defun regexp-opt (strings &optional paren)
  "Handle an empty list of strings."
  :prefix t
  (if (null strings)
      (let ((re "\\`a\\`"))
        (cond ((null paren)
               (concat "\\(?:" re "\\)"))
              ((stringp paren)
               (concat paren re "\\)"))
              ((eq paren 'words)
               (concat "\\<\\(" re "\\)\\>"))
              ((eq paren 'symbols)
               (concat "\\_\\(<" re "\\)\\_>"))
              ((concat "\\(" re "\\)"))))
    (regexp-opt strings paren)))

;;;; Defined in package.el

(declare-function lm-header "lisp-mnt")

;;* UNTESTED
(compat-defun package-get-version ()
  "Return the version number of the package in which this is used.
Assumes it is used from an Elisp file placed inside the top-level directory
of an installed ELPA package.
The return value is a string (or nil in case we can’t find it)."
  ;; In a sense, this is a lie, but it does just what we want: precompute
  ;; the version at compile time and hardcodes it into the .elc file!
  (declare (pure t))
  ;; Hack alert!
  (let ((file
         (or (and (boundp 'byte-compile-current-file) byte-compile-current-file)
             load-file-name
             buffer-file-name)))
    (cond
     ((null file) nil)
     ;; Packages are normally installed into directories named "<pkg>-<vers>",
     ;; so get the version number from there.
     ((string-match
       "/[^/]+-\\([0-9]\\(?:[0-9.]\\|pre\\|beta\\|alpha\\|snapshot\\)+\\)/[^/]+\\'"
       file)
      (match-string 1 file))
     ;; For packages run straight from the an elpa.git clone, there's no
     ;; "-<vers>" in the directory name, so we have to fetch the version
     ;; the hard way.
     ((let* ((pkgdir (file-name-directory file))
             (pkgname (file-name-nondirectory (directory-file-name pkgdir)))
             (mainfile (expand-file-name (concat pkgname ".el") pkgdir)))
        (when (file-readable-p mainfile)
          (require 'lisp-mnt)
          (with-temp-buffer
            (insert-file-contents mainfile)
            (or (lm-header "package-version")
                (lm-header "version")))))))))


;;;; Defined in dired.el

(declare-function
 dired-get-marked-files "dired.el"
 (&optional localp arg filter distinguish-one-marked error))

;;* UNTESTED
(compat-defun dired-get-marked-files
    (&optional localp arg filter distinguish-one-marked error)
  "Return the marked files’ names as list of strings."
  :feature 'dired
  :prefix t
  (let ((result (dired-get-marked-files localp arg filter distinguish-one-marked)))
    (if (and (null result) error)
        (user-error (if (stringp error) error "No files specified"))
      result)))

;;;; Defined in time-date.el

(compat-defun date-days-in-month (year month)
  "The number of days in MONTH in YEAR."
  :feature 'time-date
  (unless (and (numberp month)
               (<= 1 month)
               (<= month 12))
    (error "Month %s is invalid" month))
  (if (= month 2)
      (if (date-leap-year-p year)
          29
        28)
    (if (memq month '(1 3 5 7 8 10 12))
        31
      30)))

(compat--inhibit-prefixed (provide 'compat-27))
;;; compat-27.el ends here
