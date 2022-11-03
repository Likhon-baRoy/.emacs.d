;;; ivy-rich-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-rich" "ivy-rich.el" (0 0 0 0))
;;; Generated autoloads from ivy-rich.el

(defvar ivy-rich-mode nil "\
Non-nil if Ivy-Rich mode is enabled.
See the `ivy-rich-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-rich-mode'.")

(custom-autoload 'ivy-rich-mode "ivy-rich" nil)

(autoload 'ivy-rich-mode "ivy-rich" "\
Toggle ivy-rich mode globally.

This is a minor mode.  If called interactively, toggle the
`Ivy-Rich mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ivy-rich-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'ivy-rich-reload "ivy-rich" nil nil nil)

(defvar ivy-rich-project-root-cache-mode nil "\
Non-nil if Ivy-Rich-Project-Root-Cache mode is enabled.
See the `ivy-rich-project-root-cache-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-rich-project-root-cache-mode'.")

(custom-autoload 'ivy-rich-project-root-cache-mode "ivy-rich" nil)

(autoload 'ivy-rich-project-root-cache-mode "ivy-rich" "\
Toggle ivy-rich-root-cache-mode globally.

This is a minor mode.  If called interactively, toggle the
`Ivy-Rich-Project-Root-Cache mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ivy-rich-project-root-cache-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-rich" '("ivy-rich-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-rich-autoloads.el ends here
