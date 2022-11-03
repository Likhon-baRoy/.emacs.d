;;; rainbow-delimiters-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rainbow-delimiters" "rainbow-delimiters.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "\
Highlight nested parentheses, brackets, and braces according to their depth.

This is a minor mode.  If called interactively, toggle the
`Rainbow-Delimiters mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rainbow-delimiters-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "\
Enable `rainbow-delimiters-mode'." nil nil)

(autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "\
Disable `rainbow-delimiters-mode'." nil nil)

(register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-delimiters-autoloads.el ends here
