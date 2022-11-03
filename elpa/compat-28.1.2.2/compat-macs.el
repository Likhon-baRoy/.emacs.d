;;; compat-macs.el --- Compatibility Macros           -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
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

;; These macros are used to define compatibility functions, macros and
;; advice.

;;; Code:

(defmacro compat--ignore (&rest _)
  "Ignore all arguments."
  nil)

(defvar compat--inhibit-prefixed nil
  "Non-nil means that prefixed definitions are not loaded.
A prefixed function is something like `compat-assoc', that is
only made visible when the respective compatibility version file
is loaded (in this case `compat-26').")

(defmacro compat--inhibit-prefixed (&rest body)
  "Ignore BODY unless `compat--inhibit-prefixed' is true."
  `(unless (bound-and-true-p compat--inhibit-prefixed)
     ,@body))

(defvar compat-current-version nil
  "Default version to use when no explicit version was given.")

(defmacro compat-declare-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  ;; FIXME: Avoid setting the version for any definition that might
  ;; follow, but try to restrict it to the current file/buffer.
  (setq compat-current-version version)
  nil)

(defvar compat--generate-function #'compat--generate-default
  "Function used to generate compatibility code.
The function must take six arguments: NAME, DEF-FN, INSTALL-FN,
CHECK-FN, ATTR and TYPE.  The resulting body is constructed by
invoking the functions DEF-FN (passed the \"realname\" and the
version number, returning the compatibility definition), the
INSTALL-FN (passed the \"realname\" and returning the
installation code), CHECK-FN (passed the \"realname\" and
returning a check to see if the compatibility definition should
be installed).  ATTR is a plist used to modify the generated
code.  The following attributes are handled, all others are
ignored:

- :min-version :: Prevent the compatibility definition from begin
  installed in versions older than indicated (string).

- :max-version :: Prevent the compatibility definition from begin
  installed in versions newer than indicated (string).

- :feature :: The library the code is supposed to be loaded
  with (via `eval-after-load').

- :cond :: Only install the compatibility code, iff the value
  evaluates to non-nil.

  For prefixed functions, this can be interpreted as a test to
  `defalias' an existing definition or not.

- :no-highlight :: Do not highlight this definition as
  compatibility function.

- :version :: Manual specification of the version the compatee
  code was defined in (string).

- :realname :: Manual specification of a \"realname\" to use for
  the compatibility definition (symbol).

- :notes :: Additional notes that a developer using this
  compatibility function should keep in mind.

- :prefix :: Add a `compat-' prefix to the name, and define the
  compatibility code unconditionally.

TYPE is used to set the symbol property `compat-type' for NAME.")

(defun compat--generate-default (name def-fn install-fn check-fn attr type)
  "Generate a leaner compatibility definition.
See `compat-generate-function' for details on the arguments NAME,
DEF-FN, INSTALL-FN, CHECK-FN, ATTR and TYPE."
  (let* ((min-version (plist-get attr :min-version))
         (max-version (plist-get attr :max-version))
         (feature (plist-get attr :feature))
         (cond (plist-get attr :cond))
         (version (or (plist-get attr :version)
                      compat-current-version))
         (realname (or (plist-get attr :realname)
                       (intern (format "compat--%S" name))))
         (check (cond
                 ((or (and min-version
                           (version< emacs-version min-version))
                      (and max-version
                           (version< max-version emacs-version)))
                  '(compat--ignore))
                 ((plist-get attr :prefix)
                  '(compat--inhibit-prefixed))
                 ((and version (version<= version emacs-version) (not cond))
                  '(compat--ignore))
                 (`(when (and ,(if cond cond t)
                              ,(funcall check-fn)))))))
    (cond
     ((and (plist-get attr :prefix) (memq type '(func macro))
           (string-match "\\`compat-\\(.+\\)\\'" (symbol-name name))
           (let* ((actual-name (intern (match-string 1 (symbol-name name))))
                  (body (funcall install-fn actual-name version)))
             (when (and (version<= version emacs-version)
                        (fboundp actual-name))
               `(,@check
                 ,(if feature
                      ;; See https://nullprogram.com/blog/2018/02/22/:
                      `(eval-after-load ,feature `(funcall ',(lambda () ,body)))
                    body))))))
     ((plist-get attr :realname)
      `(progn
         ,(funcall def-fn realname version)
         (,@check
          ,(let ((body (funcall install-fn realname version)))
             (if feature
                 ;; See https://nullprogram.com/blog/2018/02/22/:
                 `(eval-after-load ,feature `(funcall ',(lambda () ,body)))
               body)))))
     ((let* ((body (if (eq type 'advice)
                       `(,@check
                         ,(funcall def-fn realname version)
                         ,(funcall install-fn realname version))
                     `(,@check ,(funcall def-fn name version)))))
        (if feature
            ;; See https://nullprogram.com/blog/2018/02/22/:
            `(eval-after-load ,feature `(funcall ',(lambda () ,body)))
          body))))))

(defun compat-generate-common (name def-fn install-fn check-fn attr type)
  "Common code for generating compatibility definitions.
See `compat-generate-function' for details on the arguments NAME,
DEF-FN, INSTALL-FN, CHECK-FN, ATTR and TYPE."
  (when (and (plist-get attr :cond) (plist-get attr :prefix))
    (error "A prefixed function %s cannot have a condition" name))
  (funcall compat--generate-function
           name def-fn install-fn check-fn attr type))

(defun compat-common-fdefine (type name arglist docstring rest)
  "Generate compatibility code for a function NAME.
TYPE is one of `func', for functions and `macro' for macros, and
`advice' ARGLIST is passed on directly to the definition, and
DOCSTRING is prepended with a compatibility note.  REST contains
the remaining definition, that may begin with a property list of
attributes (see `compat-generate-common')."
  (let ((oldname name) (body rest))
    (while (keywordp (car body))
      (setq body (cddr body)))
    ;; It might be possible to set these properties otherwise.  That
    ;; should be looked into and implemented if it is the case.
    (when (and (listp (car-safe body)) (eq (caar body) 'declare))
      (when (version<= emacs-version "25")
        (delq (assq 'side-effect-free (car body)) (car body))
        (delq (assq 'pure (car body)) (car body))))
    ;; Check if we want an explicitly prefixed function
    (when (plist-get rest :prefix)
      (setq name (intern (format "compat-%s" name))))
    (compat-generate-common
     name
     (lambda (realname version)
       `(,(cond
           ((memq type '(func advice)) 'defun)
           ((eq type 'macro) 'defmacro)
           ((error "Unknown type")))
         ,realname ,arglist
         ;; Prepend compatibility notice to the actual
         ;; documentation string.
         ,(let ((type (cond
                       ((eq type 'func) "function")
                       ((eq type 'macro) "macro")
                       ((eq type 'advice) "advice")
                       ((error "Unknown type")))))
            (if version
                (format
                 "[Compatibility %s for `%S', defined in Emacs %s]\n\n%s"
                 type oldname version docstring)
              (format
               "[Compatibility %s for `%S']\n\n%s"
               type oldname docstring)))
         ;; Advice may use the implicit variable `oldfun', but
         ;; to avoid triggering the byte compiler, we make
         ;; sure the argument is used at least once.
         ,@(if (eq type 'advice)
               (cons '(ignore oldfun) body)
             body)))
     (lambda (realname _version)
       (cond
        ((memq type '(func macro))
         ;; Functions and macros are installed by
         ;; aliasing the name of the compatible
         ;; function to the name of the compatibility
         ;; function.
         `(defalias ',name #',realname))
        ((eq type 'advice)
         `(advice-add ',name :around #',realname))))
     (lambda ()
       (cond
        ((memq type '(func macro))
         `(not (fboundp ',name)))
        ((eq type 'advice) t)))
     rest type)))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility function.
The function must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by the macro but not passed on
to the actual function.  See `compat-generate-common' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat-common-fdefine 'func name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility macro.
The macro must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by this macro but not passed on
to the actual macro.  See `compat-generate-common' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat-common-fdefine 'macro name arglist docstring rest))

(defmacro compat-advise (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility advice.
The advice function must be documented in DOCSTRING.  REST may
begin with a plist, that is interpreted by this macro but not
passed on to the actual advice function.  See
`compat-generate-common' for a listing of attributes.  The advice
wraps the old definition, that is accessible via using the symbol
`oldfun'.

The advice will only be installed, if the version this function
was defined in, as indicated by the `:version' attribute, is
greater than the current Emacs version."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat-common-fdefine 'advice name (cons 'oldfun arglist) docstring rest))

(defmacro compat-defvar (name initval docstring &rest attr)
  "Declare compatibility variable NAME with initial value INITVAL.
The obligatory documentation string DOCSTRING must be given.

The remaining arguments ATTR form a plist, modifying the
behaviour of this macro.  See `compat-generate-common' for a
listing of attributes.  Furthermore, `compat-defvar' also handles
the attribute `:local' that either makes the variable permanent
local with a value of `permanent' or just buffer local with any
non-nil value."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  ;; Check if we want an explicitly prefixed function
  (let ((oldname name))
    (when (plist-get attr :prefix)
      (setq name (intern (format "compat-%s" name))))
    (compat-generate-common
     name
     (lambda (realname version)
       (let ((localp (plist-get attr :local)))
         `(progn
            (,(if (plist-get attr :constant) 'defconst 'defvar)
             ,realname ,initval
             ;; Prepend compatibility notice to the actual
             ;; documentation string.
             ,(if version
                  (format
                   "[Compatibility variable for `%S', defined in Emacs %s]\n\n%s"
                   oldname version docstring)
                (format
                 "[Compatibility variable for `%S']\n\n%s"
                 oldname docstring)))
            ;; Make variable as local if necessary
            ,(cond
              ((eq localp 'permanent)
               `(put ',realname 'permanent-local t))
              (localp
               `(make-variable-buffer-local ',realname))))))
     (lambda (realname _version)
       `(defvaralias ',name ',realname))
     (lambda ()
       `(not (boundp ',name)))
     attr 'variable)))

(provide 'compat-macs)
;;; compat-macs.el ends here
