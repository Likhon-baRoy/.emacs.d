;;; global-keys.el --- Tweaks for Global Key-bindings configurations -*- lexical-binding: t; -*-
;;
;; Filename: global-keys.el
;; Description: This file holds my GNU Emacs Keybindings
;; Author: Likhon Baroy
;; Copyright © 2022-present Likhon Baroy <likhonhere007@gmail.com>
;; Created: 2022 Nov 25
;; Version: 0.1
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Keywords: Zmacs .emacs.d keybindings
;; Compatibility: emacs-version >= 28.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the global-keys.el file for Zmacs
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.
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


;; ───────────────────────── Generic Modified Functions ────────────────────────
;;; custom-function
;;;; window
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

;; (defun switch-to-buffer-force (&optional args)
;;   (interactive)
;;   (switch-to-buffer args))

;; (defun kill-buffer-force (&optional args)
;;   (interactive)
;;   (kill-buffer))

;;----------------------------------------------------------------------
;;;; kill-region/quoted-insert
(global-set-key (kbd "C-q") 'kill-or-quoted-insert)

(defun kill-or-quoted-insert (arg)
  "Kill the region if it is active, otherwise fall back to the default behavior.
With a prefix argument ARG, insert the next ARG characters literally."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (quoted-insert (or arg 1))))

;;----------------------------------------------------------------------
;;;; duplicate lines
(defun duplicate-current-line()
  "Make duplicate of current line right next to it."
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

;;----------------------------------------------------------------------
;;;; duplicate word

(defun duplicate-current-word()
  "Duplicate a word before point."
  (interactive)
  (let ((word-to-duplicate (thing-at-point 'word)))
    (if word-to-duplicate
        (progn
          (backward-word)
          (insert word-to-duplicate))
      (message "No word found before point"))))

;; (defun duplicate-current-word()
;;   "Duplicate a word before point."
;;   (interactive)
;;   (beginning-of-sexp)
;;   (insert (word-at-point)))

;;----------------------------------------------------------------------
;;;; copy-current-line
(defun kill-ring-save-current-line()
  "Copy line on point."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (point) (mark))
    (kill-new (thing-at-point 'line))))

;;----------------------------------------------------------------------
;;;; copy-whole-buffer-don't-move-cursor
(defun copy-buffer-and-stay ()
  "Copy the entire buffer and stay in the same position."
  (interactive)
  (let ((current-point (point)))
    (set-mark (point-min))
    (goto-char (point-max))
    (kill-ring-save (region-beginning) (region-end))
    (goto-char current-point)))

(global-set-key (kbd "C-x M-w") 'copy-buffer-and-stay)

;;----------------------------------------------------------------------
;;; keybindings
;;;; unbind-key
(global-unset-key (kbd "C-z")) ; unbind (suspend-frame)
(global-unset-key (kbd "C-x C-z")) ; also this

;; normal undo and redo
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; ──────────────────────── Make Escape Key Greate again ───────────────────────
;; (unbind-key "<escape>")
;; (bind-key "<escape>" "C-g")

;; ;;; Cursor Movement
;; (bind-key "C-x C-x"           'exchange-point-and-mark)
;; (bind-key "A-C-SPC"           'pop-to-mark-command)
;; (bind-key "C-x C-."           'pop-global-mark)
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
;; (bind-key "M-<tab>"           'company-complete-common-or-cycle)
(bind-key "M-#"               'query-replace-regexp)
(bind-key "M-\""              'insert-pair)	; wrap text in quotes.
;; (bind-key "TAB"               'self-insert-command) ; make sure that emacs is actually using `TABS' instead of `SPACES'.

;;;; text-modification
(bind-key "M-Q"               'unfill-paragraph)
(bind-key "C-x C-z"           'toggle-truncate-lines) ; long lines go off the screen
(bind-key "C-S-R"             'rename-file)
(bind-key "C-c D"             'delete-current-file-and-buffer)
;; (bind-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") ; duplicate whole line
;; (global-set-key "\M-c" 'toggle-letter-case)
(global-set-key (kbd "C-`") 'duplicate-current-line)
(global-set-key (kbd "C-~") 'duplicate-current-word)
(global-set-key (kbd "C-c C-d") 'kill-ring-save-current-line) ; C-<insert>
;; (bind-key "C-x M-$"           'ispell-buffer)
;; (bind-key "M-;"               'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-o"               'open-line)
;; (bind-key "M-|"               'align-regexp)
;; (bind-key "RET"               'newline-and-indent)
;; (bind-key "A-C-<backspace>"     'delete-trailing-whitespace)

;; (bind-key "C-c C-\\"          'toggle-input-method)
;; (bind-key "C-x C-4"           'set-selective-display)
;; (bind-key "C-h C-n"           'linum-mode)
;; (bind-key "A-q"               'quoted-insert)
;; (bind-key "C-c C-c"           'compile)
;; (bind-key "C-h o"             'list-processes)

;;;;; transpose
(bind-key "M-t" nil) ; remove the old keybinding
(bind-key "M-t c"             'transpose-chars)
(bind-key "M-t w"             'transpose-words)
(bind-key "M-t t"             'transpose-words)
(bind-key "M-t M-t"           'transpose-words)
(bind-key "M-t l"             'transpose-lines)
(bind-key "M-t e"             'transpose-sexps)
(bind-key "M-t s"             'transpose-sentences)
(bind-key "M-t p"             'transpose-paragraphs)

;;;; Killing
;; backward kill like terminal
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") (kbd "C-<backspace>")) ; 'backward-kill-word
;; instead of `BSP' I use `C-h'
(global-set-key (kbd "C-h") (kbd "<backspace>")) ; 'backward-delete-char
(global-set-key (kbd "C-S-H") (kbd "C-S-<backspace>")) ; 'kill-whole-line

;; (bind-key "\C-x\C-k"          'kill-region)
;; (bind-key "\C-c\C-k"          'kill-region)
;; (bind-key "C-M-S-k"           'backward-kill-sexp)
;; (bind-key "C-M-S-w"           'backward-kill-sexp)
;; (bind-key "C-S-l"             'append-next-kill)
;; (bind-key "C-S-k"             'kill-whole-line)
;; (bind-key "A-C-d A-C-m A-C-l" 'delete-matching-lines)
;; (bind-key "A-C-d A-C-n A-C-l" 'delete-non-matching-lines)
;; (bind-key "C-M-<backspace>"   'kill-back-to-indentation)

;;;; Buffers
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
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-right] 'next-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)

;;;; window
(bind-key "C-,"               'prev-window)
(bind-key "C-."               'other-window)
(bind-key "C-x 3"             'split-and-follow-vertically)
(bind-key "C-x 2"             'split-and-follow-horizontally)
;; (bind-key "M-o"               'other-window)

;;;;; resize
(global-set-key (kbd "M-J") 'shrink-window) ; "C-M-S-j"
(global-set-key (kbd "M-K") 'enlarge-window)
(global-set-key (kbd "M-H") 'shrink-window-horizontally)
(global-set-key (kbd "M-L") 'enlarge-window-horizontally)
;; (bind-key "M-J" (lambda () (interactive) (enlarge-window 1)))
;; (bind-key "M-K" (lambda () (interactive) (enlarge-window -1)))
;; (bind-key "M-H" (lambda () (interactive) (enlarge-window -1 t)))
;; (bind-key "M-L" (lambda () (interactive) (enlarge-window 1 t)))

;;;;; windmove
(windmove-default-keybindings)
(bind-key "s-<left>"          'windmove-left)
(bind-key "s-<right>"         'windmove-right)
(bind-key "s-<down>"          'windmove-down)
(bind-key "s-<up>"            'windmove-up)

;;;;; swap
(bind-key "C-x <C-return>"    'window-swap-states)

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

;;;; Shell
(global-set-key (kbd "C-!")   'eshell-here) ; see this function in `shell.el'
;; (bind-key "A-e"               'shell)
;; (bind-key "A-;"               'async-shell-command)
;; (bind-key "M-!"               'async-shell-command)
;; (bind-key "C-M-!"             'shell-command)

;; ;;; Mac OS X
;; (bind-key "C-<f4>"            'other-frame)
;; (bind-key "A-`"               'other-frame)
;; (bind-key "A-h"               'ns-do-hide-emacs)

;;;; misc
(global-set-key (kbd "C-S-o") "\C-a\C-o")
(global-set-key (kbd "<S-return>") "\C-e\C-m")
(bind-key "C-c T"             'switch-theme)
(bind-key "C-c t"             'toggle-transparency)
(bind-key "C-c ;"             'comment-pretty)
(bind-key "C-a"               'smarter-move-beginning-of-line)
(bind-key "C-<f1>"            'global-display-line-numbers-mode)

;;; Interactive-bindings
;;;; resume/run previous cmnd
(bind-key "C-r"
          #'(lambda () (interactive)
              (eval (car command-history))))

;;;; toggle-trailing-whitespace
(bind-key "C-c M-w" (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

;;;; join-lines
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;;;; kill-line-backward
(defun backward-kill-line ()
  "Kill chars backward until encountering the end of a line."
  (interactive) (kill-line 0) )
(global-set-key (kbd "C-S-k") 'backward-kill-line)

;;;; occur-isearch
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;; Finish up
(provide 'global-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global-keys.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
