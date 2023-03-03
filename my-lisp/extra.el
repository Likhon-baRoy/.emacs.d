;;; extra.el --- Tweaks for some Additional Function configurations -*- lexical-binding: t; -*-
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


;; ─────────────────────── Focus on newly created windows ──────────────────────

;; (switch-to-buffer (other-buffer (current-buffer) t))
(defun switcheroo ()
  "Switch to the most recent other buffer, even if it's visible in another window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun split-and-follow-horizontally ()
  "Split the window horizontally and navigate to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (switcheroo))

(defun split-and-follow-vertically ()
  "Split the window vertically and navigate to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (switcheroo))

;; ──────────────────────────────── Switch Theme ───────────────────────────────
;; (defun switch-theme (theme)
;;   "Disable any currently active themes and load THEME."
;;   ;; This interactive call is taken from `load-theme'
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapc 'symbol-name
;;                                    (custom-available-themes))))))
;;   (let ((enabled-themes custom-enabled-themes))
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (load-theme theme t)))

;; (defun disable-active-themes ()
;;   "Disable any currently active themes listed in `custom-enabled-themes'."
;;   (interactive)
;;   (mapc #'disable-theme custom-enabled-themes))

(defun switch-theme (theme)
  "Disable any currently active themes and load THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; ──────────────────────────────── Transparency ───────────────────────────────
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

;; Use the following snippet after you’ve set the alpha value
(defun toggle-transparency ()
  "Crave for transparency!"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

;; ────────────────────────────── Prettify Symbols ─────────────────────────────
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (remove-hook 'web-mode 'prettify-symbols-mode)

;; Make some word or string show as pretty Unicode symbols.  See `https://unicodelookup.com' for more.
(setq-default prettify-symbols-alist
              '(("<-" . ?←)
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ;; ("/=" . ?≠)
                ;; ("!=" . ?≠)
                ;; ("==" . ?≡)
                ;; ("<=" . ?≤)
                ;; (">=" . ?≥)
                ("=<<" . (?= (Br . Bl) ?≪))
                (">>=" . (?≫ (Br . Bl) ?=))
                ("<=<" . ?↢)
                (">=>" . ?↣)
                ("lambda" . 955)
                ("delta" . 120517)
                ("epsilon" . 120518)
                ("<" . 10216)
                (">" . 10217)
                ;; ("[" . 10214)
                ;; ("]" . 10215)
                ("<<" . 10218)
                (">>" . 10219)
                ))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; ─────────────────── Added functionality (Generic usecases) ──────────────────
;; Unfill paragraph
;; Might be good. For instance for canceling all of the paragraph quickly or for commenting it away.
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
	(fill-paragraph nil)))

(defun comment-pretty ()
  "Comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
(insert comment-start))
(insert " ")
(dotimes (_ space-on-each-side) (insert comment-char))
(when (> comment-length 0) (insert " "))
(insert comment)
(when (> comment-length 0) (insert " "))
(dotimes (_ (if (= (% comment-length 2) 0)
                (- space-on-each-side 1)
              space-on-each-side))
  (insert comment-char))))))

;; ─────────────────────────────────── CURSOR ──────────────────────────────────
(set-mouse-color "white")
(blink-cursor-mode 1)
(setq x-stretch-cursor nil) ; make cursor the width of the character it is under i.e. full width of a TAB

(require 'mode-local)

(defun djcb-set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color "yellow")
    (setq cursor-type '(hbar . 3)))
   (overwrite-mode
    (set-cursor-color "red")
    (setq cursor-type 'hollow))
   (t
    (setq cursor-type 'box)
    (set-cursor-color "#ba55d3")
    (setq-mode-local prog-mode cursor-type 'bar)
    )
   ))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
;; (add-hook 'eshell-mode-hook (lambda () (interactive) (setq-local cursor-type '(hbar . 3))))

;; ───────────────────────────────── Smart Move ────────────────────────────────
;; <https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>
;; Actually there is M-m for back-to-indentation
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (- arg 1))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(define-key global-map
  [remap move-beginning-of-line]
  'smarter-move-beginning-of-line)

;; *Second one for `first' goto-begin-char than line.

;; (defun back-to-indentation/beginning-of-line ()
;;   "Move cursor back to the beginning of the line.
;; If it is at the beginning of the line it stays there."
;;   (interactive)
;;   (when (not (bolp))
;;     (let ((p (point)))
;;       (back-to-indentation)
;;       (when (= p (point))
;;         (beginning-of-line 1)))))

;; (global-set-key (kbd "C-a") #'back-to-indentation/beginning-of-line)

;; ────────────────────────────────── ibuffer ──────────────────────────────────
;; Use human readable Size column instead of original one
(eval-after-load 'ibuffer
  '(progn
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))))

;; modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))


;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer
    (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name.
   This advice sets the cursor position to the name of the most recently
   visited buffer when ibuffer is called. This makes it easier to quickly
   switch back to a recent buffer without having to search for it in the list."
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; ─────────────────────── Delete current file and buffer ──────────────────────
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-current-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; ─────────────────────────────────── Dired ───────────────────────────────────
;; http://whattheemacsd.com/
(require 'dired)
(defun dired-back-to-top ()
  "Step back 3 lines from the very top."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 3))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  "Step up 1 line from the end."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;;;; move-quickly
(defun my-next-lines ()
  "Move to the next 5 lines."
  (interactive)
  (forward-line 5))

(defun my-previous-lines ()
  "Move to the previous 5 lines."
  (interactive)
  (forward-line -5))

(defun my-forward-chars ()
  "Move to the forward 5 chars."
  (interactive)
  (forward-char 5))

(defun my-backward-chars ()
  "Move to the backward 5 chars."
  (interactive)
  (forward-char -5))

(define-key global-map (kbd "C-S-n") #'my-next-lines)
(define-key global-map (kbd "C-S-p") #'my-previous-lines)
(define-key global-map (kbd "C-S-f") #'my-forward-chars)
(define-key global-map (kbd "C-S-b") #'my-backward-chars)

;; (global-set-key (kbd "C-S-n")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-line 5))))

;; (global-set-key (kbd "C-S-p")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-line -5))))

;; (global-set-key (kbd "C-S-f")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-char 5))))

;; (global-set-key (kbd "C-S-b")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (backward-char 5))))

;; ───────────────────────── Show LineNumber Temporary ─────────────────────────
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (forward-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

;; ─────────────────────── Open Any File With LineNumber ───────────────────────
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

;; ───────────────────────────────── Copy line ─────────────────────────────────
;; (defun copy-line (arg)
;;   "Copy lines (as many as prefix argument) in the kill ring.
;;       Ease of use features:
;;       - Move to start of next line.
;;       - Appends the copy on sequential calls.
;;       - Use newline as last char even on the last line of the buffer.
;;       - If region is active, copy its lines."
;;   (interactive "p")
;;   (let ((beg (line-beginning-position))
;;         (end (line-end-position arg)))
;;     (when mark-active
;;       (if (> (point) (mark))
;;           (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
;;         (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
;;     (if (eq last-command 'copy-line)
;;         (kill-append (buffer-substring beg end) (< end beg))
;;       (kill-ring-save beg end)))
;;   (beginning-of-line (or (and arg (1+ arg)) 2))
;;   (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; (global-set-key (kbd "M-k") 'copy-line)

;; ────────────────────────────────── flyspell ─────────────────────────────────
;; (defun flyspell-check-next-highlighted-word ()
;;   "Custom function to spell check next highlighted word."
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word))

;; (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; (eval-after-load "flyspell"
;;   '(progn
;;      (defun flyspell-goto-next-and-popup ( )
;;        "Goto the next spelling error, popup menu, and stop when the end of buffer is reached."
;;        (interactive)
;;        (while (< (point) (point-max))
;;          (flyspell-goto-next-error)
;;          (redisplay)
;;          (flyspell-correct-word-before-point))
;;        (message "No more spelling errors in buffer.")
;;        )
;;      ))
;; (global-set-key (kbd "C-<f8>") 'flyspell-goto-next-and-popup)
;; (define-key flyspell-mode-map (kbd "C-<f8>") 'flyspell-goto-next-and-popup)


;;; Finish up
(provide 'extra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extra.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
