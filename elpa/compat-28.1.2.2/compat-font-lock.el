;;; compat-font-lock.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Keywords:

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

;; Optional font-locking for `compat' definitions.  Every symbol with
;; an active compatibility definition will be highlighted.
;;
;; Load this file to enable the functionality.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'compat-macs))

(defvar compat-generate-common-fn)
(let ((compat-generate-common-fn
       (lambda (name _def-fn _install-fn check-fn attr _type)
         (unless (and (plist-get attr :no-highlight)
                      (funcall check-fn))
           `(font-lock-add-keywords
             'emacs-lisp-mode
             ',`((,(concat "\\_<\\("
                           (regexp-quote (symbol-name name))
                           "\\)\\_>")
                  1 font-lock-preprocessor-face prepend)))))))
  (load "compat"))

(provide 'compat-font-lock)
;;; compat-font-lock.el ends here
