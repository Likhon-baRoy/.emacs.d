;;; fancy-battery-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fancy-battery" "fancy-battery.el" (0 0 0 0))
;;; Generated autoloads from fancy-battery.el

(defvar fancy-battery-mode nil "\
Non-nil if Fancy-Battery mode is enabled.
See the `fancy-battery-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fancy-battery-mode'.")

(custom-autoload 'fancy-battery-mode "fancy-battery" nil)

(autoload 'fancy-battery-mode "fancy-battery" "\
Display battery status in the mode line.

Like `display-battery-mode', but fancier, and with more
customization options.

With prefix argument ARG, enable Fancy Battery Mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If `battery-status-function' is nil, the mode is not enabled.

The text in the mode line is controlled by
`fancy-battery-mode-line'.  Battery information is obtained from
`battery-status-function', and updated every
`battery-update-interval' seconds.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "fancy-battery" '("fancy-battery-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fancy-battery-autoloads.el ends here
