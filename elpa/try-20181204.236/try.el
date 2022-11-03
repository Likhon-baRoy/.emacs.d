;;; try.el --- Try out Emacs packages. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; URL: http://github.com/larstvei/try
;; Package-Version: 20181204.236
;; Package-Commit: 8831ded1784df43a2bd56c25ad3d0650cdb9df1d
;; Created: 13th November 2014
;; Keywords: packages
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; Try is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; Try is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with Try. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Try is a package that allows you to try out Emacs packages without
;; installing them. If you pass a URL to a plain text `.el`-file it evaluates
;; the content, without storing the file.

;; For more info see https://github.com/larstvei/Try

;;; Code:
(require 'package)
(require 'url)

(defun try-raw-link-p (url)
  "Returns non-nil if this looks like an URL to a .el file."
  (string-match-p "[^:]*://\\([^?\r\n]+\\).*\.el?$" url))

(defun try-raw-link (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (condition-case nil
        (progn
          (eval-region (search-forward-regexp "^$") (point-max))
          (let ((str (car (last (split-string url "/")))))
            (message "Trying %s!" str)))
      ((debug error)
       (message "Could not parse %s" url) nil))))

(defun try-package-exists-p (package-name)
  "Returns non-nil if the package is available for download."
  (assq package-name package-archive-contents))

(defun try-compose (f g)
  "Compose two functions."
  (lambda (&rest x) (funcall f (apply g x))))

(defun try-complete (archive)
  "Complete from available package names."
  (let* ((f (try-compose #'symbol-name #'car))
         (pkgs (mapcar f archive)))
    (completing-read "url or package: " pkgs)))

;;;###autoload
(defun try-and-refresh (&optional url-or-package)
  "Refreshes package-list before calling `try'."
  (interactive)
  (package-refresh-contents) (try url-or-package))

;;;###autoload
(defun try (&optional url-or-package)
  "Try out a package from your `package-archives' or pass a URL
to a raw .el file. Packages are stored in `try-tmp-dir' and raw
.el files are not stored at all."
  (interactive)
  ;; Completions for packages.
  (let* ((url-or-package (or (if (and url-or-package (symbolp url-or-package))
                                 (symbol-name url-or-package)
                               url-or-package)
                             (try-complete package-archive-contents)))
         (package-symbol (intern url-or-package)))
    (cond ((try-raw-link-p url-or-package) (try-raw-link url-or-package))
          ((try-package-exists-p package-symbol)
           (let* ((tmp-dir (make-temp-file (concat url-or-package "-") t))
                  (package-user-dir tmp-dir)
                  (package-alist nil))
             (if (version< emacs-version "25.1")
                 (package-install package-symbol)
               (package-install package-symbol 'dont-select))
             (message "Trying %s!" url-or-package)))
          (t (message (concat "Couldn't find a sensible way to try this. "
                              "Try running `package-refresh-contents'!"))))))

(provide 'try)

;;; try.el ends here.
