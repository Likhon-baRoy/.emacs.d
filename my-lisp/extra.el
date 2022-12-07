;;; extra.el --- Tweaks for some Additional Function configurations -*- lexical-binding: t -*-
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


;; ──────────────────────────── Reload Emacs `init.el' ───────────────────────────
(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

;; ─────────────────────── Focus on newly created windows ──────────────────────

;; (switch-to-buffer (other-buffer (current-buffer) t))
(defun switcheroo ()
  "Switch to the most recent other buffer, even if it's visible in another window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (switcheroo))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (switcheroo))

;; ──────────────────────────────── Switch Theme ───────────────────────────────
(defun switch-theme (theme)
  "Disable any currently active themes and load THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; ──────────────────────────── Toggle-Transparency ────────────────────────────
;; Use the following snippet after you’ve set the alpha value (C-c\C-t).
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
;; Beautify Org Checkbox Symbol
(defun ma/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  )
(add-hook 'org-mode-hook #'ma/org-buffer-setup)

(defun my/org-mode/load-prettify-symbols ()
  "Looking pretty good, so i adopted it."
  (interactive)
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . ?)
                  ("#+end_src" . ?)
                  ("#+begin_example" . ?)
                  ("#+end_example" . ?)
                  ("#+begin_quote" . ?❝)
                  ("#+end_quote" . ?―) ; ❟ ❠
                  ("#+header:" . ?)
                  ("#+name:" . ?﮸)
                  ("#+title:" . "◈")
                  ("#+results:" . ?)
                  ("#+call:" . ?)
                  (":properties:" . ?)
                  (":logbook:" . ?)))))
(add-hook 'org-mode-hook #'my/org-mode/load-prettify-symbols)


(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See `https://unicodelookup.com' for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("<" . 10216)
          (">" . 10217)
          ("[" . 10214)
          ("]" . 10215)
          ;; ("!=" . 8800)
          ("<<" . 10218)
          (">>" . 10219)
          ("->" . 8594)
          ;; ("<=" . 10877)
          ;; (">=" . 10878)
          )))

(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)
;; (remove-hook 'web-mode 'add-pretty-lambda)

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
(setq x-stretch-cursor t)		; make cursor the width of the character it is under i.e. full width of a TAB
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
    (set-cursor-color "#ba55d3")
    (setq cursor-type '(bar . 2)))))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
(blink-cursor-mode 1)

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

;; ─────────────────────────── Delete current file and buffer ──────────────────────────
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


;;; Finish up
(provide 'extra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extra.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
