;;; global-keys.el --- Tweaks for Global Key-bindings configurations -*- lexical-binding: t -*-
;;; Created on: 2022 Nov 25

;; Copyright (C) 2021-2022 Likhon Sapiens <likhonhere007@gmail.com>

;; Author: Likhon Sapiens <likhonhere007@gmail.com>
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


;; ───────────────────────── Generic Modified Functions ────────────────────────
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

;; (defun hs-mode-and-hide ()
;;   "Turn on code folding and folds all code blocks."
;;   (interactive)
;;   (hs-minor-mode)
;;   (hs-hide-all))

;; (defun switch-to-buffer-force (&optional args)
;;   (interactive)
;;   (switch-to-buffer args))

;; (defun kill-buffer-force (&optional args)
;;   (interactive)
;;   (kill-buffer))

;; ───────────────────────────── Global Unbind Key ─────────────────────────────
(unbind-key "C-z")
;; (bind-key "C-z"               'call-last-kbd-macro) ; call-last-kbd-macro frequently used key on a double key sequence (I think original is ^Xe)

;; ──────────────────────── Make Escape Key Greate again ───────────────────────
;; (unbind-key "<escape>")
;; (bind-key "<escape>" "C-g")

;; ;;; Cursor Movement
;; (bind-key "C-x C-x"           'exchange-point-and-mark)
;; (bind-key "A-SPC"             'pop-to-mark-command)
;; (bind-key " "                 'pop-to-mark-command)
;; (bind-key "A-C-SPC"           'pop-to-mark-command)
;; (bind-key "C-x C-."           'pop-global-mark)
;; (bind-key "M-SPC"             'pop-global-mark)
;; (bind-key "C-c C-n"           'next-error)
;; (bind-key "C-c C-p"           'previous-error)
;; (bind-key "A-j"               'forward-paragraph)
;; (bind-key "A-k"               'backward-paragraph)
;; (bind-key "A-h"               'mark-paragraph)
;; (bind-key "C-c C-n"           'forward-page)
;; (bind-key "C-c C-p"           'backward-page)
;; (bind-key "M-a"               'backward-up-list)

;; ;;; Selection
;; (bind-key "C-x C-l"           'select-current-line)

;; ;;; Search
;; (bind-key "C-A-g"             'custom-grep-find)
;; (bind-key "A-f"               'find-name-dired)
;; (bind-key "C-h C-w"           'find-function-on-key)
;; (bind-key "C-h C-f"           'find-function-at-point)

;; (bind-key "<M-up>"            'search-word-backward)
;; (bind-key "<M-down>"          'search-word-forward)
;; (bind-key "M-p"               'search-word-backward)
;; (bind-key "M-n"               'search-word-forward)
;; (bind-key "A-M-f"             'find-dired)
;; (bind-key "A-l"               'locate)

;; ;;; Replace
(define-key esc-map "&"       'query-replace-regexp) ; redefined ESC-&.
(bind-key "M-<tab>"           'company-complete-common-or-cycle)
(bind-key "M-#"               'query-replace-regexp)
(bind-key "M-\""              'insert-pair)	; wrap text in quotes.
;; (bind-key "TAB"               'self-insert-command) ; make sure that emacs is actually using `TABS' instead of `SPACES'.

;; ;;; Text Modification
(bind-key "M-Q"               'unfill-paragraph)
(bind-key "C-x C-l"           'toggle-truncate-lines) ; long lines go off the screen
(bind-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") ; duplicate whole line
(bind-key "C-S-R"             'rename-file)
(bind-key "C-c D"             'delete-current-file-and-buffer)
;; (bind-key "C-x M-$"           'ispell-buffer)
;; (bind-key "M-;"               'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-;"               'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-o"               'open-line)
;; (bind-key "M-|"               'align-regexp)
;; (bind-key "RET"               'newline-and-indent)
;; (bind-key "M-c"               'duplicate-current-line-or-region)
;; (bind-key "A-C-<backspace>"     'delete-trailing-whitespace)

;; ;;; Killing
(bind-key "C-w"               'backward-kill-word)
;; instead of `backspace' I use `C-h' and moved `help-command' elsewhere:
(bind-key "C-h"               'backward-delete-char)
(bind-key "C-S-H"             'kill-whole-line)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(bind-key "C-q"               'kill-region)
(bind-key "C-S-q"             'quoted-insert)
;; (bind-key "\C-x\C-k"          'kill-region)
;; (bind-key "\C-c\C-k"          'kill-region)
;; (bind-key "C-M-S-k"           'backward-kill-sexp)
;; (bind-key "C-M-S-w"           'backward-kill-sexp)
;; (bind-key "C-S-l"             'append-next-kill)
;; (bind-key "C-S-k"             'kill-whole-line)
;; (bind-key "A-C-d A-C-m A-C-l" 'delete-matching-lines)
;; (bind-key "A-C-d A-C-n A-C-l" 'delete-non-matching-lines)
;; (bind-key "C-M-<backspace>"   'kill-back-to-indentation)

;; ;;; Buffers
;; (bind-key "C-="               'ediff-buffers)
;; (bind-key "C-M-S-l"           'switch-to-buffer-force)
;; (bind-key "C-x C-b"           'switch-to-buffer)
;; ;;;;; TODO: force git commit if killed buffer has uncommited changes
;; (bind-key "C-x C-k"           'kill-buffer-force)
;; (bind-key "C-z"               'ido-switch-buffer)
;; (bind-key "A-b"               'ido-switch-buffer)
;; (bind-key "C-A-b"             'switch-to-buffer-force)
;; (bind-key "C-A-l"             'switch-to-buffer-force)
;; (bind-key "A-l"               'switch-to-buffer-force)
(bind-key "M-n"               'next-buffer)
(bind-key "M-p"               'previous-buffer)
;; (bind-key "A-J"               'next-buffer)
;; (bind-key "A-K"               'previous-buffer)

;; ;;; Windows
(bind-key "C-,"               'prev-window)
(bind-key "C-."               'other-window)
(bind-key "C-x 3"             'split-and-follow-vertically)
(bind-key "C-x 2"             'split-and-follow-horizontally)
;; (bind-key "M-o"               'other-window)

;; (bind-key "C-^"               'enlarge-window)
;; (bind-key "C-x C--"           'split-window-vertically)
;; (bind-key "C-x C-\\"          'split-window-horizontally)
;; (bind-key "C-x l"             'balance-windows-area)
;; (bind-key "C-{"               'shrink-window-horizontally)
;; (bind-key "C-}"               'enlarge-window-horizontally)

;; (bind-key "C-A-0"             'delete-window-or-frame)
;; (bind-key "C-A-0"             'delete-window)
;; (bind-key "C-A-1"             'delete-other-windows)
;; (bind-key "C-A-2"             'split-window-vertically)
;; (bind-key "C-A-3"             'split-window-horizontally)
;; (bind-key "C-A-4"             'dired-jump)
;; (bind-key "C-A-5"             'delete-window-make-new-frame)
;; (bind-key "C-A-y"             'kill-whole-line-force)
;; (bind-key "C-A-9"             'kill-buffer-and-window)
;; (bind-key "C-A--"             'bury-buffer)
;; (bind-key "C-x 9"             'kill-buffer-and-window)
;; (bind-key "C-x C-9"           'kill-buffer-and-window)

;; (bind-key "C-'"               'winner-undo-redo)
;; (bind-key "C-c C-;"           'winner-undo-redo)
;; (bind-key "C-S-<iso-lefttab>" 'other-window-previous)
;; (bind-key "C-S-<tab>"         'other-window-previous)
;; (bind-key "C-x C-d"           'dired-other-window)

;; ;;; Frames
;; (bind-key "<C-menu>"          'toggle-menu-bar-mode-from-frame)
;; (bind-key "C-x C-;"           'delete-frame)

;; ;;; Fonts
;; (bind-key "A-="               'text-scale-increase)
;; (bind-key "A--"               'text-scale-decrease)
;; (bind-key "A-C-="             'text-scale-set)
;; (bind-key "A-C-+"             'text-scale-adjust)

;; ;;; Customization
;; (bind-key "C-h C-a"           'customize-apropos-all)
;; (bind-key "C-h C-a"           'customize-apropos)
;; (bind-key "C-h C-c"           'customize-apropos)
;; (bind-key "C-h C-r"           'customize-apropos)
;; (bind-key "C-h g"             'customize-group)
;; (bind-key "C-h C-v"           'customize-variable)

;; ;;; Packages
;; (unbind-key "C-h d")
;; (bind-key "C-h d i"           'list-packages)
;; (bind-key "C-h d l"           'list-packages)

;; ;;; Help Documentation
;; (bind-key "C-h A-v"           'apropos-value)
;; (bind-key "C-x v C-h"         'describe-prefix-bindings)
;; (bind-key "A-m"               'manual-entry)

;; ;;; Shell
(bind-key "C-!"               'eshell-here)
;; (bind-key "A-e"               'shell)
;; (bind-key "A-;"               'async-shell-command)
;; (bind-key "M-!"               'async-shell-command)
;; (bind-key "C-M-!"             'shell-command)

;; Toggle show-trailing-whitespace.
(bind-key "C-c M-w" (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

;; ;;; Resize Window
(bind-key "M-J" (lambda () (interactive) (enlarge-window 1)))
(bind-key "M-K" (lambda () (interactive) (enlarge-window -1)))
(bind-key "M-H" (lambda () (interactive) (enlarge-window -1 t)))
(bind-key "M-L" (lambda () (interactive) (enlarge-window 1 t)))

;; ;;; Move Window
(windmove-default-keybindings)
(bind-key "s-<left>"          'windmove-left)
(bind-key "s-<right>"         'windmove-right)
(bind-key "s-<down>"          'windmove-down)
(bind-key "s-<up>"            'windmove-up)

;; ;;; Swap Window
(bind-key "C-c <left>"        'windswap-left)
(bind-key "C-c <right>"       'windswap-right)
(bind-key "C-c <down>"        'windswap-down)
(bind-key "C-c <up>"          'windswap-up)
(bind-key "C-x <C-return>"    'window-swap-states)

;; ;;; Transpose
(bind-key "M-t" nil) ; remove the old keybinding
(bind-key "M-t c"             'transpose-chars)
(bind-key "M-t w"             'transpose-words)
(bind-key "M-t t"             'transpose-words)
(bind-key "M-t M-t"           'transpose-words)
(bind-key "M-t l"             'transpose-lines)
(bind-key "M-t e"             'transpose-sexps)
(bind-key "M-t s"             'transpose-sentences)
(bind-key "M-t p"             'transpose-paragraphs)

;; (bind-key "C-c C-\\"          'toggle-input-method)
;; (bind-key "C-x C-z"           'toggle-truncate-lines)
;; (bind-key "C-x C-4"           'set-selective-display)
;; (bind-key "C-h C-n"           'linum-mode)
;; (bind-key "A-q"               'quoted-insert)
;; (bind-key "C-c C-c"           'compile)
;; (bind-key "C-h o"             'list-processes)

;; ;;; Mac OS X
;; (bind-key "C-<f4>"            'other-frame)
;; (bind-key "A-`"               'other-frame)
;; (bind-key "A-h"               'ns-do-hide-emacs)

;; ;;; Hide/Show
(add-hook 'prog-mode-hook     'hs-minor-mode) ; hide-all on Startup in prog-mode
(bind-key "C-c h"             'hs-hide-all)
(bind-key "C-c s"             'hs-show-all)
(bind-key "C-<tab>"           'hs-toggle-hiding) ; fold the current section
(bind-key "<backtab>"         'hs-hide-level) ; fold the sub sections of the current section
(bind-key "S-<backspace>"     'hs-hide-block)
(bind-key "C-<backspace>"     'hs-show-block)

;; ;;; Goto-Changes
(bind-key "C-c r"             'config-reload)
(bind-key "C-;"               'goto-last-change)
(bind-key "C-c b ."           'goto-last-change-reverse)
;; (bind-key "C-c b ,"           'goto-last-change)

;; ;;; Emojis
(bind-key "M-<f1>"            'emojify-insert-emoji)

;; ;;; Misc
(bind-key "M-<f12>"           'proced)
(bind-key "C-c T"             'switch-theme)
(bind-key "C-c t"             'toggle-transparency)
(bind-key "C-c ;"             'comment-pretty)
(bind-key "C-a"               'smarter-move-beginning-of-line)
(bind-key "C-<f5>"            'menu-bar--display-line-numbers-mode-relative)


;; (bind-key "C-r" 'counsel-minibuffer-history)
(bind-key "C-r"
          #'(lambda () (interactive)
              (eval (car command-history))))

;; spell check for Bangla text
(bind-key "C-c B"
          (lambda () (interactive)
            (ispell-change-dictionary "bn_BD")
            (flyspell-buffer)))

;;; Finish up
(provide 'global-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global-keys.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
