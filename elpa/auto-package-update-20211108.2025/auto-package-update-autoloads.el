;;; auto-package-update-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-package-update" "auto-package-update.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-package-update.el

(autoload 'auto-package-update-now "auto-package-update" "\
Update installed Emacs packages.

\(fn &optional ASYNC)" t nil)

(autoload 'auto-package-update-now-async "auto-package-update" "\
Update installed Emacs packages with an async manner.
If FORCE is non-nil, kill the update thread anyway.

\(fn &optional FORCE)" t nil)

(autoload 'auto-package-update-at-time "auto-package-update" "\
Try to update every day at the specified TIME.

\(fn TIME)" nil nil)

(autoload 'auto-package-update-maybe "auto-package-update" "\
Update installed Emacs packages if at least
`auto-package-update-interval' days have passed since the last
update." nil nil)

(register-definition-prefixes "auto-package-update" '("apu--" "auto-package-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-package-update-autoloads.el ends here
