;;; early-init.el --- Early initialization file -*- lexical-binding: t; -*-
;;
;; Filename: early-init.el
;; Description: Early initialization
;; Author: Likhon Sapiens
;; Copyright © 2022 Likhon Sapiens
;; Created: Fri Nov 11 10:15:28 2022 (-0400)
;; Version: 0.1
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Keywords: Emacs .emacs.d init early-init
;; Compatibility: emacs-version >= 28.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq
 package-enable-at-startup nil ; don't auto-initialize!
 package--init-file-ensured t ; don't add that `custom-set-variables' block to init
 package-quickstart nil ; prevent `package.el' loading packages prior to their init-file
 package-archives nil)


;; It will cause you to have a separate elpa directory for each Emacs version.
;; (setq package-user-dir (locate-user-emacs-file
;;                         (concat
;;                          (file-name-as-directory "elpa")
;;                          emacs-version)))

;; Or if you're using straight.el, use this instead:
;; (setq straight-build-dir (format "build-%s" emacs-version))

;; run `byte-recompile' command over `.emacs.d' directory if you get error on `re-installing'.

(setq gc-cons-threshold (expt 2 32)) ; you can remove it

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(setq safe-local-variable-values
      '((org-src-preserve-indentation . t)
        (eval add-hook 'after-save-hook
              '(lambda nil
                 (org-babel-tangle))
              nil t)))

(setq
 site-run-file nil ; unset SRF
 use-file-dialog nil
 mode-line-format nil ; don't want a mode line while loading init
 load-prefer-newer nil
 create-lockfiles nil ; disable lockfiles
 make-backup-files nil ; disable backup files
 auto-save-list-file-prefix nil ; disable auto-save
 auto-mode-case-fold nil ; use case-sensitive `auto-mode-alist' for performance
 default-input-method nil
 utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
 initial-scratch-message nil ; empty the initial *scratch* buffer.
 command-line-x-option-alist nil ; remove irreleant command line options for faster startup
 use-short-answers t ; y/n for yes/no
 vc-follow-symlinks t ; Do not ask about symlink following
 inhibit-default-init t
 inhibit-splash-screen t
 inhibit-startup-screen t 		; do not show the startup message
 inhibit-startup-message t      ; reduce noise at startup
 inhibit-startup-buffer-menu t  ; stop `list-buffers' from showing when opening multiple files
 fast-but-imprecise-scrolling t ; more performant rapid scrolling over unfontified regions
 frame-inhibit-implied-resize t ; do not resize the frame at this early stage
 ffap-machine-p-known 'reject   ; don't ping things that look like domain names
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 idle-update-delay 1.0 ; slow down UI updates down
 select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
 ad-redefinition-action 'accept ; disable warnings from legacy advice system
 initial-major-mode 'fundamental-mode
 inhibit-startup-echo-area-message user-login-name)

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) initial-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
;; (setq comp-deferred-compilation nil)
(setq native-comp-deferred-compilation nil)
;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
