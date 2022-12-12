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


(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)
(setq package-archives nil)

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

(defun config:defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun config:-do-restore-gc ()
  (setq gc-cons-threshold 16777216))
(defun config:restore-gc ()
  (run-at-time 1 nil #'config:-do-restore-gc))

(add-hook 'minibuffer-setup #'config:defer-gc)
(add-hook 'minibuffer-exit #'config:restore-gc)

(setq safe-local-variable-values
      '((org-src-preserve-indentation . t)
        (eval add-hook 'after-save-hook
              '(lambda nil
                 (org-babel-tangle))
              nil t)))

(setq
 load-prefer-newer nil
 default-input-method nil
 utf-translate-cjk-mode nil          ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
 initial-major-mode 'fundamental-mode
 inhibit-default-init t
 inhibit-startup-screen t 			 ; Do not show the startup message.
 inhibit-startup-buffer-menu t       ; stop `list-buffers' from showing when opening multiple files.
 my-computer-has-smaller-memory-p t) ; computers with smaller memory. Not sure if it works or not!

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
;; (setq comp-deferred-compilation nil)
(setq native-comp-deferred-compilation nil)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
