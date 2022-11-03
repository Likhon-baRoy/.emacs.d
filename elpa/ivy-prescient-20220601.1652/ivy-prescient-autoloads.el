;;; ivy-prescient-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-prescient" "ivy-prescient.el" (0 0 0 0))
;;; Generated autoloads from ivy-prescient.el

(defvar ivy-prescient-mode nil "\
Non-nil if Ivy-Prescient mode is enabled.
See the `ivy-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-prescient-mode'.")

(custom-autoload 'ivy-prescient-mode "ivy-prescient" nil)

(autoload 'ivy-prescient-mode "ivy-prescient" "\
Minor mode to use prescient.el in Ivy menus.

This is a minor mode.  If called interactively, toggle the
`Ivy-Prescient mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ivy-prescient-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-prescient" '("ivy-prescient-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-prescient-autoloads.el ends here
