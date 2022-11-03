;;; aggressive-indent-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "aggressive-indent" "aggressive-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from aggressive-indent.el

(autoload 'aggressive-indent-indent-defun "aggressive-indent" "\
Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun.

\(fn &optional L R)" t nil)

(autoload 'aggressive-indent-indent-region-and-on "aggressive-indent" "\
Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens.

\(fn L R)" t nil)

(autoload 'aggressive-indent-mode "aggressive-indent" "\
Minor mode to keep your code always indented.

This is a minor mode.  If called interactively, toggle the
`Aggressive-Indent mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `aggressive-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-aggressive-indent-mode 'globalized-minor-mode t)

(defvar global-aggressive-indent-mode nil "\
Non-nil if Global Aggressive-Indent mode is enabled.
See the `global-aggressive-indent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-aggressive-indent-mode'.")

(custom-autoload 'global-aggressive-indent-mode "aggressive-indent" nil)

(autoload 'global-aggressive-indent-mode "aggressive-indent" "\
Toggle Aggressive-Indent mode in all buffers.
With prefix ARG, enable Global Aggressive-Indent mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Aggressive-Indent mode is enabled in all buffers where `aggressive-indent-mode'
would do it.

See `aggressive-indent-mode' for more information on Aggressive-Indent mode.

\(fn &optional ARG)" t nil)

(defalias 'aggressive-indent-global-mode #'global-aggressive-indent-mode)

(register-definition-prefixes "aggressive-indent" '("aggressive-indent-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aggressive-indent-autoloads.el ends here
