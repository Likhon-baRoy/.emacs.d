;;; ivy-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-posframe" "ivy-posframe.el" (0 0 0 0))
;;; Generated autoloads from ivy-posframe.el

(defvar ivy-posframe-mode nil "\
Non-nil if Ivy-Posframe mode is enabled.
See the `ivy-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-posframe-mode'.")

(custom-autoload 'ivy-posframe-mode "ivy-posframe" nil)

(autoload 'ivy-posframe-mode "ivy-posframe" "\
Display ivy via posframe.

This is a minor mode.  If called interactively, toggle the
`Ivy-Posframe mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ivy-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-posframe" '("ivy-posframe-"))

;;;***

;;;### (autoloads nil nil ("ivy-posframe-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-posframe-autoloads.el ends here
