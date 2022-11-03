;;; company-statistics-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-statistics" "company-statistics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-statistics.el

(defvar company-statistics-mode nil "\
Non-nil if Company-Statistics mode is enabled.
See the `company-statistics-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-statistics-mode'.")

(custom-autoload 'company-statistics-mode "company-statistics" nil)

(autoload 'company-statistics-mode "company-statistics" "\
Statistical sorting for company-mode.  Ranks completion candidates by
the frequency with which they have been chosen in recent (as given by
`company-statistics-size') history.

This is a minor mode.  If called interactively, toggle the
`Company-Statistics mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='company-statistics-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Turning this mode on and off preserves the statistics.  They are also
preserved automatically between Emacs sessions in the default
configuration.  You can customize this behavior with
`company-statistics-auto-save', `company-statistics-auto-restore' and
`company-statistics-file'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "company-statistics" '("company-s"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-statistics-autoloads.el ends here
