;;; try-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "try" "try.el" (0 0 0 0))
;;; Generated autoloads from try.el

(autoload 'try-and-refresh "try" "\
Refreshes package-list before calling `try'.

\(fn &optional URL-OR-PACKAGE)" t nil)

(autoload 'try "try" "\
Try out a package from your `package-archives' or pass a URL
to a raw .el file. Packages are stored in `try-tmp-dir' and raw
.el files are not stored at all.

\(fn &optional URL-OR-PACKAGE)" t nil)

(register-definition-prefixes "try" '("try-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; try-autoloads.el ends here
