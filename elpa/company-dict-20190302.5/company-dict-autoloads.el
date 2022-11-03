;;; company-dict-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-dict" "company-dict.el" (0 0 0 0))
;;; Generated autoloads from company-dict.el

(autoload 'company-dict-refresh "company-dict" "\
Refresh all loaded dictionaries." t nil)

(autoload 'company-dict "company-dict" "\
`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-dict" '("company-dict-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-dict-autoloads.el ends here
