;;; compat-26.el --- Compatibility Layer for Emacs 26.1  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 26.1, needed by older
;; versions.
;;
;; Only load this library if you need to use one of the following
;; functions:
;;
;; - `compat-sort'
;; - `line-number-at-pos'
;; - `compat-alist-get'
;; - `string-trim-left'
;; - `string-trim-right'
;; - `string-trim'

;;; Code:

(require 'compat-macs "compat-macs.el")

(compat-declare-version "26.1")

;;;; Defined in eval.c

(compat-defun func-arity (func)
  "Return minimum and maximum number of args allowed for FUNC.
FUNC must be a function of some kind.
The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number, or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form."
  :realname compat--func-arity
  (cond
   ((or (null func) (and (symbolp func) (not (fboundp func))))
    (signal 'void-function func))
   ((and (symbolp func) (not (null func)))
    (compat--func-arity (symbol-function func)))
   ((eq (car-safe func) 'macro)
    (compat--func-arity (cdr func)))
   ((subrp func)
    (subr-arity func))
   ((memq (car-safe func) '(closure lambda))
    ;; See lambda_arity from eval.c
    (when (eq (car func) 'closure)
      (setq func (cdr func)))
    (let ((syms-left (if (consp func)
                         (car func)
                       (signal 'invalid-function func)))
          (min-args 0) (max-args 0) optional)
      (catch 'many
        (dolist (next syms-left)
          (cond
           ((not (symbolp next))
            (signal 'invalid-function func))
           ((eq next '&rest)
            (throw 'many (cons min-args 'many)))
           ((eq next '&optional)
            (setq optional t))
           (t (unless optional
                (setq min-args (1+ min-args)))
              (setq max-args (1+ max-args)))))
        (cons min-args max-args))))
   ((and (byte-code-function-p func) (numberp (aref func 0)))
    ;; See get_byte_code_arity from bytecode.c
    (let ((at (aref func 0)))
      (cons (logand at 127)
            (if (= (logand at 128) 0)
                (ash at -8)
              'many))))
   ((and (byte-code-function-p func) (numberp (aref func 0)))
    ;; See get_byte_code_arity from bytecode.c
    (let ((at (aref func 0)))
      (cons (logand at 127)
            (if (= (logand at 128) 0)
                (ash at -8)
              'many))))
   ((and (byte-code-function-p func) (listp (aref func 0)))
    ;; Based on `byte-compile-make-args-desc', this is required for
    ;; old versions of Emacs that don't use a integer for the argument
    ;; list description, per e2abe5a13dffb08d6371b6a611bc39c3a9ac2bc6.
    (let ((arglist (aref func 0)) (mandatory 0) nonrest)
      (while (and arglist (not (memq (car arglist) '(&optional &rest))))
        (setq mandatory (1+ mandatory))
        (setq arglist (cdr arglist)))
      (setq nonrest mandatory)
      (when (eq (car arglist) '&optional)
        (setq arglist (cdr arglist))
        (while (and arglist (not (eq (car arglist) '&rest)))
          (setq nonrest (1+ nonrest))
          (setq arglist (cdr arglist))))
      (cons mandatory (if arglist 'many nonrest))))
   ((autoloadp func)
    (autoload-do-load func)
    (compat--func-arity func))
   ((signal 'invalid-function func))))

;;;; Defined in fns.c

(compat-defun assoc (key alist &optional testfn)
  "Handle the optional argument TESTFN.
Equality is defined by the function TESTFN, defaulting to
`equal'.  TESTFN is called with 2 arguments: a car of an alist
element and KEY.  With no optional argument, the function behaves
just like `assoc'."
  :prefix t
  (if testfn
      (catch 'found
        (dolist (ent alist)
          (when (funcall testfn (car ent) key)
            (throw 'found ent))))
    (assoc key alist)))

(compat-defun mapcan (func sequence)
  "Apply FUNC to each element of SEQUENCE.
Concatenate the results by altering them (using `nconc').
SEQUENCE may be a list, a vector, a boolean vector, or a string."
  (apply #'nconc (mapcar func sequence)))

;;* UNTESTED
(compat-defun line-number-at-pos (&optional position absolute)
  "Handle optional argument ABSOLUTE:

If the buffer is narrowed, the return value by default counts the lines
from the beginning of the accessible portion of the buffer.  But if the
second optional argument ABSOLUTE is non-nil, the value counts the lines
from the absolute start of the buffer, disregarding the narrowing."
  :prefix t
  (if absolute
      (save-restriction
        (widen)
        (line-number-at-pos position))
    (line-number-at-pos position)))

;;;; Defined in subr.el

(declare-function compat--alist-get-full-elisp "compat-25"
                  (key alist &optional default remove testfn))
(compat-defun alist-get (key alist &optional default remove testfn)
  "Handle TESTFN manually."
  :realname compat--alist-get-handle-testfn
  :prefix t
  (if testfn
      (compat--alist-get-full-elisp key alist default remove testfn)
    (alist-get key alist default remove)))

(gv-define-expander compat-alist-get
  (lambda (do key alist &optional default remove testfn)
    (macroexp-let2 macroexp-copyable-p k key
      (gv-letplace (getter setter) alist
        (macroexp-let2 nil p `(if (and ,testfn (not (eq ,testfn 'eq)))
                                  (compat-assoc ,k ,getter ,testfn)
                                (assq ,k ,getter))
          (funcall do (if (null default) `(cdr ,p)
                        `(if ,p (cdr ,p) ,default))
                   (lambda (v)
                     (macroexp-let2 nil v v
                       (let ((set-exp
                              `(if ,p (setcdr ,p ,v)
                                 ,(funcall setter
                                           `(cons (setq ,p (cons ,k ,v))
                                                  ,getter)))))
                         `(progn
                            ,(cond
                              ((null remove) set-exp)
                              ((or (eql v default)
                                   (and (eq (car-safe v) 'quote)
                                        (eq (car-safe default) 'quote)
                                        (eql (cadr v) (cadr default))))
                               `(if ,p ,(funcall setter `(delq ,p ,getter))))
                              (t
                               `(cond
                                 ((not (eql ,default ,v)) ,set-exp)
                                 (,p ,(funcall setter
                                               `(delq ,p ,getter))))))
                            ,v))))))))))

(compat-defun string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  :realname compat--string-trim-left
  :prefix t
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(compat-defun string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  :realname compat--string-trim-right
  :prefix t
  (let ((i (string-match-p
            (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
            string)))
    (if i (substring string 0 i) string)))

(compat-defun string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading with and trailing matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  :prefix t
  ;; `string-trim-left' and `string-trim-right' were moved from subr-x
  ;; to subr in Emacs 27, so to avoid loading subr-x we use the
  ;; compatibility function here:
  (compat--string-trim-left
   (compat--string-trim-right
    string
    trim-right)
   trim-left))

(compat-defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car x))))

(compat-defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr x))))

(compat-defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (car x))))

(compat-defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr x))))

(compat-defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car x))))

(compat-defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr x))))

(compat-defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car x))))

(compat-defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr x))))

(compat-defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car (car x)))))

(compat-defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (car (cdr x)))))

(compat-defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (car (cdr (car x)))))

(compat-defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr (cdr x)))))

(compat-defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (car (cdr (car (car x)))))

(compat-defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (car (cdr x)))))

(compat-defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (cdr (car x)))))

(compat-defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr (cdr x)))))

(compat-defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car (car x)))))

(compat-defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (car (cdr x)))))

(compat-defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (car (cdr (car x)))))

(compat-defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr (cdr x)))))

(compat-defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car (car x)))))

(compat-defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (car (cdr x)))))

(compat-defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (cdr (car x)))))

(compat-defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr (cdr x)))))

(compat-defvar gensym-counter 0
  "Number used to construct the name of the next symbol created by `gensym'.")

(compat-defun gensym (&optional prefix)
  "Return a new uninterned symbol.
The name is made by appending `gensym-counter' to PREFIX.
PREFIX is a string, and defaults to \"g\"."
  (let ((num (prog1 gensym-counter
               (setq gensym-counter
                     (1+ gensym-counter)))))
    (make-symbol (format "%s%d" (or prefix "g") num))))

;;;; Defined in files.el

(declare-function temporary-file-directory nil)

;;* UNTESTED
(compat-defun make-nearby-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file as close as possible to `default-directory'.
If PREFIX is a relative file name, and `default-directory' is a
remote file name or located on a mounted file systems, the
temporary file is created in the directory returned by the
function `temporary-file-directory'.  Otherwise, the function
`make-temp-file' is used.  PREFIX, DIR-FLAG and SUFFIX have the
same meaning as in `make-temp-file'."
  (let ((handler (find-file-name-handler
                  default-directory 'make-nearby-temp-file)))
    (if (and handler (not (file-name-absolute-p default-directory)))
        (funcall handler 'make-nearby-temp-file prefix dir-flag suffix)
      (let ((temporary-file-directory (temporary-file-directory)))
        (make-temp-file prefix dir-flag suffix)))))

(compat-defvar mounted-file-systems
    (eval-when-compile
      (if (memq system-type '(windows-nt cygwin))
          "^//[^/]+/"
        (concat
         "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/")))))
  "File systems that ought to be mounted.")

(compat-defun file-local-name (file)
  "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
  :realname compat--file-local-name
  (or (file-remote-p file 'localname) file))

(compat-defun file-name-quoted-p (name &optional top)
  "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name and TOP is nil, check the local part of NAME."
  :realname compat--file-name-quoted-p
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (string-prefix-p "/:" (compat--file-local-name name))))

(compat-defun file-name-quote (name &optional top)
  "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name and TOP is nil, the local part of
NAME is quoted.  If NAME is already a quoted file name, NAME is
returned unchanged."
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (if (compat--file-name-quoted-p name top)
        name
      (concat (file-remote-p name) "/:" (compat--file-local-name name)))))

;;* UNTESTED
(compat-defun temporary-file-directory ()
  "The directory for writing temporary files.
In case of a remote `default-directory', this is a directory for
temporary files on that remote host.  If such a directory does
not exist, or `default-directory' ought to be located on a
mounted file system (see `mounted-file-systems'), the function
returns `default-directory'.
For a non-remote and non-mounted `default-directory', the value of
the variable `temporary-file-directory' is returned."
  (let ((handler (find-file-name-handler
                  default-directory 'temporary-file-directory)))
    (if handler
        (funcall handler 'temporary-file-directory)
      (if (string-match mounted-file-systems default-directory)
          default-directory
        temporary-file-directory))))

;;* UNTESTED
(compat-defun file-attribute-type (attributes)
  "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
  (nth 0 attributes))

;;* UNTESTED
(compat-defun file-attribute-link-number (attributes)
  "Return the number of links in ATTRIBUTES returned by `file-attributes'."
  (nth 1 attributes))

;;* UNTESTED
(compat-defun file-attribute-user-id (attributes)
  "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 2 attributes))

;;* UNTESTED
(compat-defun file-attribute-group-id (attributes)
  "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 3 attributes))

;;* UNTESTED
(compat-defun file-attribute-access-time (attributes)
  "The last access time in ATTRIBUTES returned by `file-attributes'.
This a Lisp timestamp in the style of `current-time'."
  (nth 4 attributes))

;;* UNTESTED
(compat-defun file-attribute-modification-time (attributes)
  "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the style of `current-time'."
  (nth 5 attributes))

;;* UNTESTED
(compat-defun file-attribute-status-change-time (attributes)
  "The status modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of last change to the file's attributes: owner
and group, access mode bits, etc., and is a Lisp timestamp in the
style of `current-time'."
  (nth 6 attributes))

;;* UNTESTED
(compat-defun file-attribute-size (attributes)
  "The integer size (in bytes) in ATTRIBUTES returned by `file-attributes'."
  (nth 7 attributes))

;;* UNTESTED
(compat-defun file-attribute-modes (attributes)
  "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
  (nth 8 attributes))

;;* UNTESTED
(compat-defun file-attribute-inode-number (attributes)
  "The inode number in ATTRIBUTES returned by `file-attributes'.
It is a nonnegative integer."
  (nth 10 attributes))

;;* UNTESTED
(compat-defun file-attribute-device-number (attributes)
  "The file system device number in ATTRIBUTES returned by `file-attributes'.
It is an integer."
  (nth 11 attributes))

(compat-defun file-attribute-collect (attributes &rest attr-names)
  "Return a sublist of ATTRIBUTES returned by `file-attributes'.
ATTR-NAMES are symbols with the selected attribute names.

Valid attribute names are: type, link-number, user-id, group-id,
access-time, modification-time, status-change-time, size, modes,
inode-number and device-number."
  (let ((idx '((type . 0)
               (link-number . 1)
               (user-id . 2)
               (group-id . 3)
               (access-time . 4)
               (modification-time . 5)
               (status-change-time . 6)
               (size . 7)
               (modes . 8)
               (inode-number . 10)
               (device-number . 11)))
        result)
    (while attr-names
      (let ((attr (pop attr-names)))
        (if (assq attr idx)
            (push (nth (cdr (assq attr idx))
                       attributes)
                  result)
          (error "Wrong attribute name '%S'" attr))))
    (nreverse result)))

;;;; Defined in subr-x.el

(compat-defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and evaluate THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  :realname compat--if-let*
  :feature 'subr-x
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(or (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,then ,@else))))

(compat-defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
This is like `when-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  ;; :feature 'subr-x
  (declare (indent 1) (debug if-let*))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(or (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (when ,(caar list) ,@body))))

(compat-defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  :feature 'subr-x
  (declare (indent 1) (debug if-let*))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(or (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,(macroexp-progn (or body '(t)))))))

;;;; Defined in image.el

;;* UNTESTED
(compat-defun image-property (image property)
  "Return the value of PROPERTY in IMAGE.
Properties can be set with

  (setf (image-property IMAGE PROPERTY) VALUE)

If VALUE is nil, PROPERTY is removed from IMAGE."
  (plist-get (cdr image) property))

;;* UNTESTED
(unless (get 'image-property 'gv-expander)
  (gv-define-setter image-property (image property value)
    (let ((image* (make-symbol "image"))
          (property* (make-symbol "property"))
          (value* (make-symbol "value")))
      `(let ((,image* ,image)
             (,property* ,property)
             (,value* ,value))
         (if
             (null ,value*)
             (while
                 (cdr ,image*)
               (if
                   (eq
                    (cadr ,image*)
                    ,property*)
                   (setcdr ,image*
                           (cdddr ,image*))
                 (setq ,image*
                       (cddr ,image*))))
           (setcdr ,image*
                   (plist-put
                    (cdr ,image*)
                    ,property* ,value*)))))))

;;;; Defined in rmc.el

;;*UNTESTED
(compat-defun read-multiple-choice
    (prompt choices &optional _help-string _show-help long-form)
  "Ask user to select an entry from CHOICES, promting with PROMPT.
This function allows to ask the user a multiple-choice question.

CHOICES should be a list of the form (KEY NAME [DESCRIPTION]).
KEY is a character the user should type to select the entry.
NAME is a short name for the entry to be displayed while prompting
\(if there's no room, it might be shortened).

If LONG-FORM, do a `completing-read' over the NAME elements in
CHOICES instead."
  :note "This is a partial implementation of `read-multiple-choice', that
among other things doesn't offer any help and ignores the
optional DESCRIPTION field."
  (if long-form
      (let ((options (mapconcat #'cadr choices "/"))
            choice)
        (setq prompt (concat prompt " (" options "): "))
        (setq choice (completing-read prompt (mapcar #'cadr choices) nil t))
        (catch 'found
          (dolist (option choices)
            (when (string= choice (cadr option))
              (throw 'found option)))
          (error "Invalid choice")))
    (let ((options
           (mapconcat
            (lambda (opt)
              (format
               "[%s] %s"
               (key-description (string (car opt)))
               (cadr opt)))
            choices " "))
          choice)
      (setq prompt (concat prompt " (" options "): "))
      (while (not (setq choice (assq (read-char prompt) choices)))
        (message "Invalid choice")
        (sit-for 1))
      choice)))

(compat--inhibit-prefixed (provide 'compat-26))
;;; compat-26.el ends here
