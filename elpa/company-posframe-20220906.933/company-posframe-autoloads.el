;;; company-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-posframe" "company-posframe.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-posframe.el

(defvar company-posframe-mode nil "\
Non-nil if Company-Posframe mode is enabled.
See the `company-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-posframe-mode'.")

(custom-autoload 'company-posframe-mode "company-posframe" nil)

(autoload 'company-posframe-mode "company-posframe" "\
company-posframe minor mode.

This is a minor mode.  If called interactively, toggle the
`Company-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='company-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-posframe" '("company-posframe-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-posframe-autoloads.el ends here
