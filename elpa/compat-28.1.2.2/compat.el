;;; compat.el --- Emacs Lisp Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Compat Development <~pkal/compat-devel@lists.sr.ht>
;; Version: 28.1.2.2
;; URL: https://sr.ht/~pkal/compat
;; Package-Requires: ((emacs "24.3") (nadvice "0.3"))
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

;; To allow for the usage of Emacs functions and macros that are
;; defined in newer versions of Emacs, compat.el provides definitions
;; that are installed ONLY if necessary.  These reimplementations of
;; functions and macros are at least subsets of the actual
;; implementations.  Be sure to read the documentation string to make
;; sure.
;;
;; Not every function provided in newer versions of Emacs is provided
;; here.  Some depend on new features from the core, others cannot be
;; implemented to a meaningful degree.  Please consult the Compat
;; manual for details.  The main audience for this library are not
;; regular users, but package maintainers.  Therefore commands and
;; user options are usually not implemented here.

;;; Code:

(defvar compat--inhibit-prefixed)
(let ((compat--inhibit-prefixed (not (bound-and-true-p compat-testing))))
  ;; Instead of using `require', we manually check `features' and call
  ;; `load' to avoid the issue of not using `provide' at the end of
  ;; the file (which is disabled by `compat--inhibit-prefixed', so
  ;; that the file can be loaded again at some later point when the
  ;; prefixed definitions are needed).
  (dolist (vers '(24 25 26 27 28))
    (unless (memq (intern (format "compat-%d" vers)) features)
      (load (format "compat-%d%s" vers
                    (if (bound-and-true-p compat-testing)
                        ".el" ""))
            nil t))))

(provide 'compat)
;;; compat.el ends here
