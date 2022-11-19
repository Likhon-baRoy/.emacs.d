;;; flycheck-popup-tip-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-popup-tip" "flycheck-popup-tip.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-popup-tip.el

(autoload 'flycheck-popup-tip-mode "flycheck-popup-tip" "\
A minor mode to show Flycheck error messages in a popup.

This is a minor mode.  If called interactively, toggle the
`Flycheck-Popup-Tip mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `flycheck-popup-tip-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "flycheck-popup-tip" '("flycheck-popup-tip-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-popup-tip-autoloads.el ends here
