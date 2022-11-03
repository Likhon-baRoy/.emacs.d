;;; all-the-icons-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "all-the-icons" "all-the-icons.el" (0 0 0 0))
;;; Generated autoloads from all-the-icons.el

(autoload 'all-the-icons-icon-for-dir "all-the-icons" "\
Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'.

\(fn DIR &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-file "all-the-icons" "\
Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn FILE &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-mode "all-the-icons" "\
Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn MODE &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-icon-for-url "all-the-icons" "\
Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

\(fn URL &rest ARG-OVERRIDES)" nil nil)

(autoload 'all-the-icons-install-fonts "all-the-icons" "\
Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

\(fn &optional PFX)" t nil)

(autoload 'all-the-icons-insert "all-the-icons" "\
Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

\(fn &optional ARG FAMILY)" t nil)

(register-definition-prefixes "all-the-icons" '("all-the-icons-"))

;;;***

;;;### (autoloads nil nil ("all-the-icons-faces.el" "all-the-icons-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-the-icons-autoloads.el ends here
