;;; minions-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "minions" "minions.el" (0 0 0 0))
;;; Generated autoloads from minions.el

(defvar minions-mode nil "\
Non-nil if Minions mode is enabled.
See the `minions-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `minions-mode'.")

(custom-autoload 'minions-mode "minions" nil)

(autoload 'minions-mode "minions" "\
Display a minor-mode menu in the mode line.

This is a minor mode.  If called interactively, toggle the
`Minions mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='minions-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This replaces the likely incomplete and possibly cut off list of
minor-modes that is usually displayed directly in the mode line.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "minions" '("minions-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; minions-autoloads.el ends here
