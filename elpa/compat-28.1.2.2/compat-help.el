;;; compat-help.el --- Documentation for compat functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>

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

;; Load this file to insert `compat'-relevant documentation next to
;; the regular documentation of a symbol.

;;; Code:

(defun compat---describe (symbol)
  "Insert documentation for SYMBOL if it has compatibility code."
  (let ((compat (get symbol 'compat-def)))
    (when compat
      (let ((doc (get compat 'compat-doc))
            (start (point)))
        (when doc
          (insert "There is a ")
          (insert-button
           "compatibility notice"
           'action (let ((type (get compat 'compat-type)))
                     (cond
                      ((memq type '(func macro advice))
                       #'find-function)
                      ((memq type '(variable))
                       #'find-variable)
                      ((error "Unknown type"))))
           'button-data compat)
          (insert (format " for %s (for versions of Emacs before %s):"
                          (symbol-name symbol)
                          (get compat 'compat-version)))
          (add-text-properties start (point) '(face bold))
          (newline 2)
          (insert (substitute-command-keys doc))
          (fill-region start (point))
          (newline 2))))))

(add-hook 'help-fns-describe-function-functions #'compat---describe)

(provide 'compat-help)
;;; compat-help.el ends here
