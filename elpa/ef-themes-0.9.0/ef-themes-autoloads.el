;;; ef-themes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ef-themes" "ef-themes.el" (0 0 0 0))
;;; Generated autoloads from ef-themes.el

(autoload 'ef-themes-select "ef-themes" "\
Load an Ef THEME using minibuffer completion.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, THEME is the symbol of a theme.  VARIANT
is ignored in this scenario.

\(fn THEME &optional VARIANT)" t nil)

(autoload 'ef-themes-toggle "ef-themes" "\
Toggle between the two `ef-themes-to-toggle'.
If `ef-themes-to-toggle' does not specify two Ef themes, inform
the user about it while prompting with completion for a theme
among our collection (this is practically the same as the
`ef-themes-select' command).

Run `ef-themes-post-load-hook' after loading the theme." t nil)

(autoload 'ef-themes-load-random "ef-themes" "\
Load an Ef theme at random, excluding the current one.

With optional VARIANT as a prefix argument, prompt to limit the
set of themes to either dark or light variants.

Run `ef-themes-post-load-hook' after loading the theme.

When called from Lisp, VARIANT is either the `dark' or `light'
symbol.

\(fn &optional VARIANT)" t nil)

(autoload 'ef-themes-preview-colors "ef-themes" "\
Preview palette of the Ef THEME of choice.

\(fn THEME)" t nil)

(autoload 'ef-themes-preview-colors-current "ef-themes" "\
Call `ef-themes-preview-colors' for the current Ef theme." t nil)

(autoload 'ef-themes-theme "ef-themes" "\
Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `ef-themes-faces' and
`ef-themes-custom-variables' respectively.

\(fn NAME PALETTE)" nil t)

(function-put 'ef-themes-theme 'lisp-indent-function '0)

(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (file-equal-p dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))

(register-definition-prefixes "ef-themes" '("ef-themes-"))

;;;***

;;;### (autoloads nil nil ("ef-autumn-theme.el" "ef-bio-theme.el"
;;;;;;  "ef-cherie-theme.el" "ef-cyprus-theme.el" "ef-dark-theme.el"
;;;;;;  "ef-day-theme.el" "ef-deuteranopia-dark-theme.el" "ef-deuteranopia-light-theme.el"
;;;;;;  "ef-duo-dark-theme.el" "ef-duo-light-theme.el" "ef-frost-theme.el"
;;;;;;  "ef-light-theme.el" "ef-night-theme.el" "ef-spring-theme.el"
;;;;;;  "ef-summer-theme.el" "ef-themes-pkg.el" "ef-trio-dark-theme.el"
;;;;;;  "ef-trio-light-theme.el" "ef-tritanopia-dark-theme.el" "ef-tritanopia-light-theme.el"
;;;;;;  "ef-winter-theme.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ef-themes-autoloads.el ends here
