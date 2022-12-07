;;; org-config.el --- Tweaks for my org-mode configurations -*- lexical-binding: t -*-
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


;; ─────────────────────────────────── *ORG* ───────────────────────────────────
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

(use-package org
  :after org
  ;; :delight " ⓔ"
  :config (setq org-ellipsis " ▾") ;; ↴, ▼, ▶, ⤵, ▾
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

;; Improve org mode looks
(setq
 org-roam-v2-ack t                 ; anonying startup message
 org-log-done 'time                ; I need to know when a task is done
 org-startup-folded t
 org-odd-levels-only t
 org-pretty-entities t
 org-startup-indented t
 org-adapt-indentation t
 org-hide-leading-stars t
 org-hide-macro-markers t
 org-hide-emphasis-markers t
 org-startup-with-inline-images t
 org-image-actual-width '(300))

;; ORG-TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d!)"  "MAYBE(m)"  "BLOCKED(b@)" "READ(r)" "ARCHIVED(a!)" "INPROGRESS(i)" "WAITING(w)" "NEXT(n)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "red" :weight bold))
        ("DOING"    . (:foreground "salmon" :weight bold))
        ("REVIEW"   . (:foreground "orange" :weight bold))
        ("TODO"     . (:foreground "HotPink3" :weight bold))
        ("BLOCKED"  . (:foreground "DeepPink" :weight bold))
        ("DONE"     . (:foreground "SeaGreen3" :weight bold))
        ("READ"     . (:foreground "SteelBlue2" :weight bold))
        ("ARCHIVED" . (:foreground "LightSlateGrey" :weight bold))
        ("MAYBE"    . (:foreground "LightSteelBlue4" :weight bold))
        ("NEXT"     . (:foreground "black" :background "yellow" :weight bold))
        ("WAITING"  . (:foreground "purple" :background "tomato" :weight bold))
        ("INPROGRESS" . (:foreground "yellow" :weight bold))))

(require 'org-tempo)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "✿" "✚" "✸" "❀" "○"))) ; "●" "▷" "🞛" "◈"
(use-package org-alert
  :defer t
  :after org
  :config
  (progn
    (setq alert-default-style 'libnotify)
    ))
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; (setq-default prettify-symbols-alist
;;               '(("#+begin_quote" . "ϰ")
;;                 ("#+end_quote" . "ϰ")))

;; Exported html should have no default style. I can style it myself:
(setq org-html-head-include-default-style nil
      org-html-htmlize-output-type 'css)

;; Execute Org src block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js .t )
   (shell . t)))
;; ────────────────────────────── Extra Functions ─────────────────────────────
(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasis markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))
  (define-key org-mode-map (kbd "C-c x") 'org-toggle-emphasis)

;;; Finish up
(provide 'org-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-config.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
