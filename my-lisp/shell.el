;;; shell.el --- Tweaks for shell configurations -*- lexical-binding: t -*-
;;; Created on: 25 November 2022

;; Copyright (C) 2021-2022 Likhon Sapins <likhonhere007@gmail.com>

;; Author: Likhon Sapins <likhonhere007@gmail.com>
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my tweaks for Org that are meant for use in my
;; Emacs setup: https://github.com/Likhon-baRoy/.emacs.d.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:


;; ──────────────────────────────────── SHELL ───────────────────────────────────
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize "[" 'face '(:foreground "red" :weight bold))
         (propertize (user-login-name) 'face '(:foreground "DarkGoldenrod1" :weight bold))
         (propertize "@" 'face '(:foreground "DarkOliveGreen2" :weight bold))
         (propertize (system-name) 'face '(:foreground "CadetBlue3" :weight bold))" "
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face '(:foreground "" :weight bold))
           (propertize (eshell/basename (eshell/pwd)) 'face '(:foreground "DarkRed")))
         (propertize "]" 'face '(:foreground "red" :weight bold))
         (if (= (user-uid) 0) "# "
           (concat  "$ " )))))

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
         current buffer's file. The eshell is renamed to match
         that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (insert (concat "ls"))
    (eshell-send-input)))

(defun my-eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (ignore-errors
          (delete-window)))
    (delete-forward-char arg)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (bind-keys :map eshell-mode-map
                       ("C-d" . my-eshell-quit-or-delete-char))))

;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell

;; Eshell is an elisp shell. It has its own configuration parameters, distinct from those of shell or ansi-terminal.
;; Eshell Settings
(setq eshell-directory-name (concat user-emacs-directory "etc/eshell")
      eshell-history-file-name (concat user-emacs-directory "etc/eshell/history")
      eshell-aliases-file (concat user-emacs-directory "etc/eshell/alias")
      eshell-last-dir-ring-file-name (concat user-emacs-directory "etc/eshell/lastdir")
      eshell-prefer-lisp-functions nil
      eshell-highlight-prompt nil
      eshell-buffer-shorthand t
      eshell-cmpl-ignore-case t
      eshell-history-size 10000
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-cmpl-cycle-completions t
      eshell-destroy-buffer-when-process-dies t
      ;; auto truncate after 20k lines
      eshell-buffer-maximum-lines 20000
      eshell-error-if-no-glob t
      eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'all
      eshell-destroy-buffer-when-process-dies t
      eshell-list-files-after-cd t)

;; Visual commands
(setq eshell-visual-commands
      '("ranger" "vi" "screen" "top" "less" "more" "lynx"
        "ncftp" "pine" "tin" "trn" "elm" "vim"
        "nmtui" "alsamixer" "htop" "el" "elinks"
        ))
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  "Company for terminal."
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :config
  (require 'company)
  (add-hook 'shell-mode-hook 'shell-mode-company-init))

;;; Finish up
(provide 'shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
