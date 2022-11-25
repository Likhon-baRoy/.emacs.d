;;; compile-and-run.el --- Tweaks for program compiling and running -*- lexical-binding: t -*-
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


;; ─────────────────────────────────── C/C++ ───────────────────────────────────
(defun compile-and-run()
  "Run C++ program directly from within Emacs."
  (interactive)
  (save-buffer)
  (compile (concat "g++ " (file-name-nondirectory (buffer-file-name)) " -o "
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " && ./"
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) t )
  (other-window 1)
  (goto-char (point-max)))

(defun exit-after-compile-hook (cur-buffer msg)
  "When nil: Don't kill the window.  0: Affect visible and iconified frames."
  (when (y-or-n-p "Quit window? ")
    (quit-window nil (get-buffer-window cur-buffer 0))))
(add-hook 'compilation-finish-functions #'exit-after-compile-hook)

(with-eval-after-load "cc-mode" (define-key c++-mode-map [f5] #'compile-and-run))

;; (add-hook 'c++-mode-hook
;;           (lambda () (global-set-key (kbd "<f5>") #'compileandrun)))

(defun cpp ()
  "Compile output as `./a.out' and Run program directly from within Emacs Shell."
  (interactive)
  (async-shell-command (concat "g++ " (buffer-file-name) " && ./a.out") "*c++ output*")
  (other-window 1)
  (goto-char (point-max)))

(with-eval-after-load "cc-mode" (define-key c++-mode-map [f12] #'cpp))

(defun c-program ()
  "Compile with `./a.out' and Run program within `eshell'."
  (interactive)
  (async-shell-command (concat "gcc " (buffer-file-name) " && ./a.out") "*c output*")
  (other-window 1)
  (goto-char (point-max)))
(with-eval-after-load "cc-mode" (define-key c-mode-map [f5] #'c-program))

;;; Finish up
(provide 'compile-and-run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-and-run.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
