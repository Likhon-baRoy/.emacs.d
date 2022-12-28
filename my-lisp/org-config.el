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
  :bind
  (:map org-mode-map
        ("C-'"    . nil)
        ("<f5>"    . org-cycle-agenda-files))
  :config
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (variable-pitch-mode 1)
  (global-set-key (kbd "C-c a") 'org-agenda))

;; Improve org mode looks
(setq
 org-ellipsis " ▾"                 ; ↴, ▼, ▶, ⤵, ▾
 org-roam-v2-ack t                 ; anonying startup message
 org-log-done 'time                ; I need to know when a task is done
 org-startup-folded t
 org-odd-levels-only t
 org-pretty-entities t
 org-startup-indented t
 org-adapt-indentation t
 org-hide-leading-stars t
 org-hide-macro-markers t
 org-hide-block-startup nil
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-hide-emphasis-markers t
 org-cycle-separator-lines 2
 org-startup-folded 'content
 org-startup-with-inline-images t
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 2
 org-fontify-quote-and-verse-blocks t
 ;; org-clock-sound "~/.emacs.d/etc/bell.mp3" ; Bell ringtone for stopwatch
 org-image-actual-width '(300))

(setq org-modules
      '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

(setq org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 1)))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

;; Execute Org src block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js .t )
   (shell . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "✿" "✚" "✸" "❀" "○"))) ; "●" "▷" "🞛" "◈"

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Replace list plus with arrow
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "➤"))))))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.5)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; Exported html should have no default style. I can style it myself:
;; (setq org-html-head-include-default-style nil
;;       org-html-htmlize-output-type 'css)

;; This is needed as of Org 9.2
(require 'org-tempo)

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
