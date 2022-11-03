;;; add-hooks-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "add-hooks" "add-hooks.el" (0 0 0 0))
;;; Generated autoloads from add-hooks.el

(autoload 'add-hooks-pair "add-hooks" "\
Call `add-hook' for each combined pair of items in HOOKS and FUNCTIONS.

HOOKS can be a symbol or a list of symbols representing hook
variables (the `-hook' suffix is implied).  FUNCTIONS can be a
symbol, a lambda, or a list of either representing hook
functions.  If lists are used, a function can be added to
multiple hooks and/or multiple functions can be added to a hook.

Example:

  ELISP> (add-hooks-pair '(css-mode sgml-mode) 'emmet-mode)
  nil
  ELISP> css-mode-hook
  (emmet-mode)
  ELISP> sgml-mode-hook
  (emmet-mode)

\(fn HOOKS FUNCTIONS)" nil nil)

(autoload 'add-hooks "add-hooks" "\
Call `add-hooks-pair' on each cons pair in PAIRS.

Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Pair values are passed to the
HOOKS and FUNCTIONS arguments of `add-hooks-pair', respectively.

Usage:

  (add-hooks ((HOOKS . FUNCTIONS)...))

Example:

  ELISP> (add-hooks '(((css-mode sgml-mode) . emmet-mode)))
  nil
  ELISP> css-mode-hook
  (emmet-mode)
  ELISP> sgml-mode-hook
  (emmet-mode)

\(fn PAIRS)" nil nil)

(register-definition-prefixes "add-hooks" '("add-hooks-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; add-hooks-autoloads.el ends here
