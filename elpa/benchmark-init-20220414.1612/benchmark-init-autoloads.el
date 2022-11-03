;;; benchmark-init-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "benchmark-init" "benchmark-init.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from benchmark-init.el

(autoload 'benchmark-init/activate "benchmark-init" "\
Activate benchmark-init and start collecting data." t nil)

(register-definition-prefixes "benchmark-init" '("benchmark-init/"))

;;;***

;;;### (autoloads nil "benchmark-init-modes" "benchmark-init-modes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from benchmark-init-modes.el

(autoload 'benchmark-init/show-durations-tabulated "benchmark-init-modes" "\
Show the benchmark results in a sorted table." t nil)

(autoload 'benchmark-init/show-durations-tree "benchmark-init-modes" "\
Show durations in call-tree." t nil)

(register-definition-prefixes "benchmark-init-modes" '("benchmark-init/"))

;;;***

;;;### (autoloads nil nil ("benchmark-init-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; benchmark-init-autoloads.el ends here
