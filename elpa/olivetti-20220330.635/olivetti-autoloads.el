;;; olivetti-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "olivetti" "olivetti.el" (0 0 0 0))
;;; Generated autoloads from olivetti.el

(autoload 'olivetti-mode "olivetti" "\
Olivetti provides a nice writing environment.
Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

This is a minor mode.  If called interactively, toggle the `Olivetti mode' mode.
If the prefix argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the mode if ARG
is nil, omitted, or is a positive number.  Disable the mode if ARG is a negative
number.

To check whether the minor mode is enabled in the current buffer, evaluate
`olivetti-mode'.

The mode's hook is called both when the mode is enabled and when it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "olivetti" '("olivetti-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; olivetti-autoloads.el ends here
