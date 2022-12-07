;;; quick-find.el --- Tweaks for my directory key-binding configurations -*- lexical-binding: t -*-
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


(use-package bind-key
  :ensure t)

(defun dired-timesort (filename &optional wildcards)
  (let ((dired-listing-switches "-lhat"))
    (dired filename wildcards)))

(defmacro quick-find (key file &optional path find-args)
  `(bind-key
    ,key
    (cond
     ((stringp ,find-args)
      '(lambda (&optional arg)
         (interactive)
         (find-dired (expand-file-name ,file ,path) ,find-args)))
     ((and
       ;; (not (tramp-tramp-file-p (expand-file-name ,file ,path)))
       (or (file-directory-p (expand-file-name ,file ,path))
           (not (file-exists-p (expand-file-name ,file ,path)))))
      '(lambda (&optional arg)
         (interactive)
         (dired-timesort (expand-file-name ,file ,path))))
     (t
      '(lambda (&optional arg)
         (interactive)
         (find-file (expand-file-name ,file ,path)))))))

;;; Files
(quick-find "C-c C-`"     "~/")
;; (quick-find "C-c C-e"     "/sudo::/home/raxit/")
(quick-find "C-c C-x w" "~/wrx")
(quick-find "C-c M-o" "~/wrx/org")
(quick-find "C-c e"     user-init-file)
;; (quick-find "C-c C-x C-k" user-keys-file)
(quick-find "C-c M-h" "~/.cache/zsh/history")
(quick-find "C-c M-p" "~/.config/mpv/")
(quick-find "C-c M-q" "my-lisp/quick-find.el" user-emacs-directory)
(quick-find "C-c M-y" "etc/yasnippet/snippets" user-emacs-directory)

;; (quick-find "C-h C-t"     "/tmp/")
;; (quick-find "C-h C-o"     "~/Documents")
;; (quick-find "C-h C-d"     "~/Downloads")
;; (quick-find "C-h C-x C-u" custom-file)
;; (quick-find "C-h C-x C-c" "Cask" user-emacs-directory)
;; (quick-find "C-h C-x C-e" (format ".cask/%s/elpa/" emacs-version) user-emacs-directory)
;; (quick-find "C-h C-x e"   "emacs" "~/workspace")
;; (quick-find "C-h C-x p"   "~/Pictures")
;; (quick-find "C-h C-x C-s" "~/.ssh/config")
;; (quick-find "C-h C-x C-b" "~/.bash_profile")
;; ;; (quick-find "C-h C-x C-b" (crux-find-shell-init-file))
;; (quick-find "C-h C-x C-s" "~/.ssh/config")
;; ;; (quick-find "C-h C-x C-h" "/sudo::/etc/hosts")
;; (quick-find "C-h C-x s" "settings.el" user-emacs-directory)

;;; Finish up
(provide 'quick-find)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quick-find.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
