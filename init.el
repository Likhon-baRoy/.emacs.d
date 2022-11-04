;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
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

;; Unset file-name-handler-alist temporarily. Then restore it later...
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;;Load Path
;; Since all the configuration files are stored in a folder, they need to be added to `load-path' now.
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elpa" user-emacs-directory))

;; Config Edit/Re-load
(defun config-visit ()
  "Uncle dev created a function to find Emacs config."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'config-reload)

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

(global-set-key (kbd "C-c T") 'switch-theme)


;; ───────────────────────────────── DASHBOARD ─────────────────────────────────
;; A dashboard on startup can clean my mind
(use-package dashboard
  :demand t
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-image-banner-max-height 300)
  (dashboard-banner-logo-title "[Ποσειδον 🔱 εδιτορ]")
  (dashboard-startup-banner (concat user-emacs-directory "logos/whiteBeard.png"))
  :config
  (setq dashboard-footer-icon (all-the-icons-octicon "calendar"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "octoface" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/Likhon-baRoy/emacs")) nil "" " |")
           (,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
            "Update"
            "Update Megumacs"
            (lambda (&rest _) (update-packages)) warning "" " |")
           (,(all-the-icons-faicon "flag" :height 1.1 :v-adjust 0.0) nil
            "Report a BUG"
            (lambda (&rest _) (browse-url "https://github.com/b-coimbra/.emacs.d/issues/new")) error "" ""))
          ;; line 2
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "AlienFriend"
            "Browse Alien Page"
            (lambda (&rest _) (browse-url "https://github.com/b-coimbra/.emacs.d")) nil "" ""))
          ;; Empty line
          (("" "\n" "" nil nil "" ""))

          ;; Keybindings
          ((,(all-the-icons-octicon "search" :height 0.9 :v-adjust -0.1)
            " Find file" nil
            (lambda (&rest _) (counsel-find-file)) nil "" "            C-x C-f"))
          ((,(all-the-icons-octicon "file-directory" :height 1.0 :v-adjust -0.1)
            " Open project" nil
            (lambda (&rest _) (counsel-projectile-switch-project)) nil "" "         C-x p d"))
          ((,(all-the-icons-octicon "three-bars" :height 1.1 :v-adjust -0.1)
            " File explorer" nil
            (lambda (&rest _) (counsel-projectile-switch-project)) nil "" "        C-x p D"))
          ((,(all-the-icons-octicon "settings" :height 0.9 :v-adjust -0.1)
            " Open settings" nil
            (lambda (&rest _) (open-config-file)) nil "" "        C-c e  "))))

  (setq dashboard-projects-backend 'project-el
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-items '((recents        . 5)
                          (projects       . 5)
                          (bookmarks      . 5)
                          (agenda         . 5)
                          (registers      . 5)))
  :custom-face
  (dashboard-heading ((t (:weight bold)))) ; :foreground "#f1fa8c"
  :hook
  (after-init . dashboard-setup-startup-hook))

;; ────────────────────────────────── ENCODING ─────────────────────────────────
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;; ────────────────────────────────── ORG-MODE ─────────────────────────────────
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
(when window-system (global-prettify-symbols-mode t))

(use-package org
  :init (setq org-startup-folded t)
  :config
  (setq org-ellipsis " ▾") ;; ↴, ▼, ▶, ⤵, ▾
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

;; Improve org mode looks
(setq
 org-roam-v2-ack t                 ; anonying startup message
 org-log-done 'time                ; I need to know when a task is done
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
  (org-bullets-bullet-list '("☯" "✿" "✚" "◉" "❀"))) ; '("◉" "○" "●" "○" "●" "○" "●")))
(use-package org-alert
  :defer t
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

;; (use-package org-appear
;;   :hook (org-mode . org-appear-mode))

;; Beautify Org Checkbox Symbol
(defun teddy-ma/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  )
(add-hook 'org-mode-hook #'teddy-ma/org-buffer-setup)

(defun my/org-mode/load-prettify-symbols ()
  "Looking pretty good, so i adopted it."
  (interactive)
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . ?)
                  ("#+end_src" . ?)
                  ("#+begin_example" . ?)
                  ("#+end_example" . ?)
                  ("#+header:" . ?)
                  ("#+name:" . ?﮸)
                  ("#+title:" . "")
                  ("#+results:" . ?)
                  ("#+call:" . ?)
                  (":properties:" . ?)
                  (":logbook:" . ?)))))
(add-hook 'org-mode-hook #'my/org-mode/load-prettify-symbols)

;; Exported html should have no default style. I can style it myself:
(setq org-html-head-include-default-style nil
      org-html-htmlize-output-type 'css)

;; Execute Org src block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js .t )
   (shell . t)))

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("<" . 10216)
          (">" . 10217)
          ("[" . 10214)
          ("]" . 10215)
          ("!=" . 8800)
          ("<<" . 10218)
          (">>" . 10219)
          ("->" . 8594)
          ("<=" . 10877)
          (">=" . 10878))))

(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

;; ──────────────────────────── Startup Performance ────────────────────────────
;; make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time.
;; The default is 800 kilobytes.  Measured in bytes.
;; Garbage collection off during initialization (focus all memory on initialize)
(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.6)

;; Reset garbage collection after initialization (return deprecated memory to stack when idle)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; ─────────────────────────────── AUTO-COMPLETE ───────────────────────────────
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (setq ac-modes '(sh-mode lisp-mode c-mode c++-mode sql-mode html-mode)) ; you can specified only for some certain mode
;; ──────────────────────────────── COMPANY-MODE ───────────────────────────────
(use-package company-box
  :hook (company-mode . company-box-mode))
;; (set-face-background 'company-box--apply-color "#555555")
(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-h"        . nil)
        ("C-j"        . nil)
        ("C-k"        . nil)
        ("C-n"        . nil)
        ("C-p"        . nil)
        ("C-w"        . nil)
        ("RET"        . nil)
        ("<return>"   . nil)
        ("SPC"        . nil)    ; Prevent SPC from ever triggering a completion.
        ("M-n"        . nil)
        ("M-p"        . nil)
        ("M-."        . company-show-location)
        ("M-<"        . company-select-first)
        ("M->"        . company-select-last)
        ("C-c C-/"    . company-other-backend)
        ("C-<return>" . company-complete-selection)
        ("C-l"        . company-complete-selection)
        ("<tab>"      . company-indent-or-complete-common)
        ("TAB"        . company-indent-or-complete-common)
        ("C-j"        . company-select-next)
        ("C-k"        . company-select-previous)
        ("C-d"        . company-show-doc-buffer)
        ("C-s"        . company-filter-candidates))
  (:map company-search-map    ; applies to `company-filter-map' too
        ("C-h"        . nil)
        ("C-j"        . nil)
        ("C-k"        . nil)
        ("C-n"        . nil)
        ("C-p"        . nil)
        ("M-/"        . company-complete)
        ("M-TAB"      . company-yasnippet)
        ("C-j"        . company-select-next)
        ("C-k"        . company-select-previous)
        ("C-s"        . company-filter-candidates)
        ([escape]     . company-search-abort))
  :init
  (setq company-idle-delay 0.0
        company-echo-delay 0
        completion-ignore-case t
        company-require-match nil
        company-show-quick-access t
        company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t
        company-dabbrev-ignore-case nil
        company-debbrev-other-buffers 'all
        company-dabbrev-downcase nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-minimum company-tooltip-limit
        company-begin-commands '(self-insert-command)
        company-require-match #'company-explicit-action-p
        company-frontends '(company-pseudo-tooltip-frontend)
        company-transformers '(company-sort-by-occurrence))
  :config
  (setq company-backends '((company-files        ; files & directory
                            company-keywords     ; keywords
                            company-capf         ; what is this?
                            company-cmake
                            company-yasnippet
                            company-c-headers
                            ;; company-clang     ; it's too slow
                            ;; company-ispell
                            ;; company-irony-c-headers
                            ;; company-irony
                            company-dabbrev-code
                            company-semantic
                            company-gtags
                            company-etags
                            company-rtags
                            company-elisp)
                           (company-abbrev company-dabbrev))))

;; Use TAB key to cycle through suggestions.(`tng' means 'TAB and go')
;; (company-tng-configure-default)

(use-package company-rtags)
(use-package company-c-headers)

;; Delete duplicates from company popups
(setq-local company-transformers '(delete-dups)
            company-backends '(company-files (:separate company-dabbrev company-ispell)))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-idle-delay 0.0)
  (company-quickhelp-mode 1))

;; This package adds usage-based sorting to Company
;; completions. (Perhaps it too can be replaced by `historian' one day!)
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package yasnippet
  :delight yas-minor-mode " υ"
  :custom (yas-snippet-dirs '("~/.emacs.d/etc/yasnippet/snippets"))
  :commands yas-reload-all
  :hook ((prog-mode minibuffer-inactive-mode org-mode) . yas-minor-mode)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :custom (yasnippet-snippets-dirs '("~/.emacs.d/etc/yasnippet/snippets"))
  :config (yasnippet-snippets-initialize))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :delight)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (name . "\*mu4e\*"))
               ("programming" (or
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)
;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)
(setq ibuffer-default-sorting-mode 'recency)
;; ───────────────────────────────── FLY-SPELL ─────────────────────────────────
(use-package flyspell
  :config
  (setq ispell-program-name "hunspell" ; Requires Hunspell
        ispell-default-dictionary "en_GB")
  :hook (org-mode . flyspell-mode)
  :bind (("M-<f7>" . flyspell-buffer)
         ("<f7>" . flyspell-word)
         ("C-<f7>" . flyspell-auto-correct-word)
         ("C-;" . flyspell-auto-correct-previous-word)))
;; ────────────────────────────────── WEB-MODE ─────────────────────────────────
(use-package emmet-mode
  :after(web-mode css-mode scss-mode)
  :commands (emmet-mode emmet-expand-line yas-insert-snippet company-complete)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
                                        ;(setq emmet-indentation 2)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  :bind
  ("C-j" . emmet-expand-line)
  ((:map emmet-mode-keymap
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point)))
  );end emmet mode ;; enable Emmet's css abbreviation.
;;________________________________________________________________
;;    Global Key Bindings
;;________________________________________________________________
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(define-key emacs-lisp-mode-map (kbd "C-c C-b")
			  (lambda ()
				"Save and evaluate-buffer."
				(interactive)
				(save-buffer)
				(eval-buffer)))))

(global-unset-key (kbd "<escape>"))
(global-set-key (kbd "<escape>") (kbd "C-g"))

(define-key esc-map "&" 'query-replace-regexp)		; redefined ESC-&
(global-set-key (kbd "M-#") 'query-replace-regexp)
(global-set-key (kbd "M-\"") 'insert-pair)			; Wrap text in quotes
                                        ;(global-set-key (kbd "TAB") 'self-insert-command)	; To make sure that emacs is actually using TABS instead of SPACES

;; I use C-h for backspace in Emacs and move `help-command' elsewhere:
(global-set-key "\^h" 'backward-delete-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
;; (global-set-key (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "C-S-H") 'kill-whole-line)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-x\C-l" 'toggle-truncate-lines) ; this lets us have long lines go off the side of the screen instead of hosing up the ascii art
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")	; Duplicate a whole line
(global-set-key (kbd "C-S-R") 'rename-file)
(global-set-key "\C-cD" 'Delete-current-file)
;; disable ctrl Z
(global-unset-key "\^z")
;; (global-set-key "\C-z" 'call-last-kbd-macro)		; call-last-kbd-macro frequently used key on a double key sequence (I think original is ^Xe)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-o"  'other-window)
(global-set-key "\M-n"  'next-buffer)
(global-set-key "\M-p"  'previous-buffer)
(global-set-key (kbd "M-<tab>") 'company-complete-common-or-cycle)
(global-set-key "\M-TAB"  'company-complete-common-or-cycle)
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

;; spell check for Bangla text
(global-set-key (kbd "C-c B")
                (lambda()(interactive)
                  (ispell-change-dictionary "bn_BD")
                  (flyspell-buffer)))
;; Toggle show-trailing-whitespace.
(global-set-key (kbd "C-c M-w") (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))
(windmove-default-keybindings)
(global-set-key (kbd "s-<left>")    'windmove-left)
(global-set-key (kbd "s-<right>")   'windmove-right)
(global-set-key (kbd "s-<down>")    'windmove-down)
(global-set-key (kbd "s-<up>")      'windmove-up)

(global-set-key (kbd "C-c <left>")    'windswap-left)
(global-set-key (kbd "C-c <right>")   'windswap-right)
(global-set-key (kbd "C-c <down>")    'windswap-down)
(global-set-key (kbd "C-c <up>")      'windswap-up)

(global-set-key (kbd "M-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t t") 'transpose-words)
(global-set-key (kbd "M-t M-t") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t e") 'transpose-sexps)
(global-set-key (kbd "M-t s") 'transpose-sentences)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)
(global-set-key (kbd "M-<f1>") 'emojify-insert-emoji)
;;________________________________________________________________
;;    Separte Customization from init file
;;________________________________________________________________
(setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))

(when (file-exists-p custom-file)
  (load custom-file))
;; (load custom-file 'noerror 'nomessage)

;; https://emacs.metaphoric.dev/#org21aec79
;; Whenever the base core.org file is updated, all the custom user settings are
;; wiped out.
;; To prevent this, the user may define permanent settings in the
;; config.org file.

;; (setq-default userconfig-file (expand-file-name "config.el" user-emacs-directory))
;; (when (file-exists-p userconfig-file)
;;   (load userconfig-file))

;; Load custom themes
(add-to-list 'custom-theme-load-path (expand-file-name "etc/themes/" user-emacs-directory))

;; Linux emoji font
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Noto Color Emoji"))
;;________________________________________________________________
;;    Editing Related
;;________________________________________________________________
(delete-selection-mode t)		; By default emacs will not delete selection text when typing on it, let's fix it
(setq kill-whole-line t) 			; kills the entire line plus the newline whenever you invoke kill-line (i.e. via C-k).

(ffap-bindings) ; find-file-at-point, smarter C-x C-f when point on path or URL

;; Saves the minibuffer history on every Emacs session.
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/var/savehist.el")
(setq history-length 25)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
;; Ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open dired in same buffer
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)
;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/sxiv") ;or /usr/bin/gimp
;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c x") 'image-dired)
  (define-key dired-mode-map (kbd "M-<return>") 'image-dired-dired-display-external))

;; (load-theme 'tango-dark t)
;; (set-foreground-color "ivory")
;; (set-background-color "darkblue")
;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")	; Change the HIGHLIGHT COLOR for SELECTED TEXT
;; (set-face-attribute 'highlight nil :foreground 'unspecified)

;;________________________________________________________________
;;    Fonts Setting
;;________________________________________________________________
(global-font-lock-mode 1)               ; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)   ; We have CPU to spare; highlight all syntax categories.

(setq default-input-method "bengali-probhat")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size 16))

(defun remove-quail-show-guidance ()
  "Function for removing guidance."
  nil)
(defun remove-quail-completion ()
  "Function for removing completion."
  (quail-select-current))
(defun bn-company-wordfreq ()
  "Bangla auto-suggestion with company-wordfreq."
  (interactive)
  (advice-add 'quail-show-guidance :override #'remove-quail-show-guidance)
  (advice-add 'quail-completion :override #'remove-quail-completion)
  (setq ispell-local-dictionary "bengali")
  (setq-local company-backends '(company-wordfreq))
  (setq-local company-transformers nil))
(add-hook 'text-mode-hook (lambda ()
                            (setq-local company-backends '(company-wordfreq))
                            (setq-local company-transformers nil)))

(set-face-attribute 'default nil
		            :font "Fantasque Sans Mono" ; "JetBrains Mono"
		            :weight 'light
		            :height (cond ((string-equal system-type "gnu/linux") 110)
				                  ((string-equal system-type "darwin") 130)))
(set-face-attribute 'font-lock-comment-face nil :family "Cantarell" :slant 'italic :height 92)
(set-face-attribute 'font-lock-function-name-face nil :foreground "cyan" :slant 'italic :weight 'medium)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; (set-face-attribute 'font-lock-comment-face nil :foreground "#5B6268" :slant 'italic)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#c678dd" :slant 'italic :weight 'bold)
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)

;; (set-frame-font "Comic Mono-10.5" nil t)
;; (set-frame-font "Monaco-9" nil t)
;; (set-frame-font "Fantasque Sans Mono-10.5" nil t)
;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Fira Code-10" nil t)

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

(defun ljos/back-to-indentation|beginning-of-line ()
  "Move cursor back to the beginning of the line.
If it is at the beginning of the line it stays there."
  (interactive)
  (when (not (bolp))
    (let ((p (point)))
      (back-to-indentation)
      (when (= p (point))
        (beginning-of-line 1)))))

(global-set-key (kbd "C-a") #'ljos/back-to-indentation|beginning-of-line)

;; ──────────────────────── General But Better Defaults ────────────────────────
(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 speedbar t                         ; Quick file access with bar
 backup-by-copying t               ; don't clobber symlinks
 backup-directory-alist `(("."~/.emacs.d/var/backup/per-session))
 default-directory "~/"
 load-prefer-newer t 				; don't use the compiled code if its the older package
 make-backup-files t               ; backup of a file the first time it is saved
 delete-by-moving-to-trash t		; move deleted files to trash
 delete-old-versions t             ; delete excess backup files silently
 kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2)
 kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
 version-control t                 ; version numbers for backup files
 auto-save-default t               ; auto-save every buffer that visits a file
 auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
 compilation-always-kill t         ; kill compilation process before starting another
 compilation-ask-about-save nil    ; save all buffers on `compile'
 compilation-scroll-output t
 tab-width 4
 indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces
 indent-line-function 'insert-tab
 require-final-newline t
 x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard
 save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them
 apropos-do-all t                  ; Shows all options when running apropos
 mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click
 message-log-max 1000
 fill-column 80
 initial-scratch-message nil       ; Empty the initial *scratch* buffer
 make-pointer-invisible t          ; hide cursor when writing
 column-number-mode t              ; Show (line,column) in mode-line
 cua-selection-mode t              ; Delete regions
 enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
 backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines)
 )
(save-place-mode 1)
(show-paren-mode 1)         ; Highlight matching parenthesis
(global-auto-revert-mode 1) ; Automatically revert a buffer when it changes on disk
(fringe-mode '(8 . 0))      ; Enable fringe on the left for git-gutter-fringe+
(global-subword-mode 1)     ; Iterate through CamelCase words
(electric-pair-mode t)      ; Enable Matching delimeters
(electric-indent-mode nil)  ; Auto indentation
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\{ . ?\})))

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq
 debug-on-error init-file-debug       ; Reduce debug output, well, unless we've asked for it.
 jka-compr-verbose init-file-debug
 read-process-output-max (* 64 1024)  ; 64kb
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0              ; default is 0.5
 scroll-step 1                      ; scroll with less jump
 scroll-preserve-screen-position t
 scroll-margin 3
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster
 auto-window-vscroll nil            ; Lighten vertical scroll
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 hscroll-step 1                     ; Horizontal Scroll
 hscroll-margin 1
 redisplay-skip-fontification-on-input t
 visible-bell t                     ; Flash the screen on error, don't beep.
 view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
 use-dialog-box nil                 ; Don't pop up UI dialogs when prompting
 echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly
 delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
 save-place-forget-unreadable-files nil
 blink-matching-paren t              ; Blinking parenthesis
 next-line-add-newlines nil     ; don't automatically add new line, when scroll down at the bottom of a buffer
 require-final-newline t        ; require final new line
 mouse-sel-retain-highlight t   ; keep mouse high-lighted
 highlight-nonselected-windows nil
 transient-mark-mode t          ; highlight the stuff you are marking
 show-paren-delay 0           		; how long to wait?
 show-paren-style 'mixed      		; alternatives are 'expression' and 'parenthesis'
 ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
 pgtk-wait-for-event-timeout 0.001
 frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")   ; name of the file I am editing as the name of the window
 )

;; ─────────────────── Added functionality (Generic usecases) ──────────────────
;; Unfill paragraph
;; Might be good. For instance for canceling all of the paragraph quickly or for commenting it away.
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
	(fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

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

(global-set-key (kbd "C-c ;") 'comment-pretty)

;; Automatically purge backup files not accessed in a week:
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))
;;________________________________________________________________
;;		Identity Who I Am ?
;;________________________________________________________________
(setq user-full-name       "Likhon Barai"
      user-login-name      "likhon"
      user-real-login-name "raxit"
      user-mail-address    "likhonhere007@gmail.com")
;;________________________________________________________________
;;		Highlight Current LINE
;;________________________________________________________________
(when window-system (global-hl-line-mode 1))
;; (set-face-background 'highlight "#3e4446")	; you canalso try: "#3e4446" or "#gray6" etc.
;; (set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")

;; (when window-system (vline-global-mode 1))
;; (set-face-background 'vline "#3e4446")	; you canalso try: "#ff0000" or "#gray6" or etc.
;; (set-face-foreground 'vline nil)
;; (setq vline-style 'mixed)

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

;; Use the following snippet after you’ve set the alpha as above to assign a toggle to “C-c t”:
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
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; ────────────────────────────── Generic packages ─────────────────────────────

;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-Melpa Packages

;; Add packages contained in site-elisp/ to load-path too.
;; Add Packages Manually from Git

;; cd site-elisp/
;; git submodule add https://github.com/foo/bar.git

;; Verify .gitmodules file that the newly added package exist.
;; Update Manually Added Packages

;; git submodule init
;; git submodule update

(require 'package)
;; Configure Package Manager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package
(eval-and-compile
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq warning-minimum-level :emergency)
  (setq use-package-enable-imenu-support t))

;; ─────────────────── Additional Packages and Configurations ──────────────────
;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

;; github.com/doomemacs/doomemacs/blob/develop/core/core.el#L296
(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq
   gcmh-idle-delay 'auto  ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  )

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
;; Diminish a feature that removes certain minor-modes from mode-line.
(use-package diminish)

;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
(global-dash-fontify-mode)

(use-package delight
  :delight)

;; Benchmark startup
;; benchmark-init records startup time by package so we can debug. It only records things after it’s initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package avy
  :bind(("C-'" . 'avy-goto-char)
        ("C-:" . 'avy-goto-char-2)
        ("M-g g" . 'avy-goto-line)
        ("M-g e" . 'avy-goto-word-0)
        ("M-g w" . 'avy-goto-word-1)
        ("C-c C-j" . 'avy-resume))
  :config
  (setq avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode exwm-mode))
  :custom
  (avy-timeout-seconds 0.5)
  (avy-style 'pre))
;; :custom-face
;; (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))))


(use-package uniquify-files
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package no-littering
  :doc "It’s good to have centralized working datasets storage, to prevent pollution of Emacs config directory."
  :custom
  (no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)))

(setq auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Show recent files in the File menu.
(use-package recentf
  :doc "Recent buffers in a new Emacs session"
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am") ; or, recentf-auto-cleanup 'never
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50))

(use-package magit
  :doc "Git integration for Emacs"
  :bind ("C-x g" . magit-status)
  :delight)

(use-package git-gutter
  :diminish
  :hook ((prog-mode org-mode) . git-gutter-mode )
  ;;✘
  :config
  (setq git-gutter:modified-sign "†")
  (setq git-gutter:added-sign "†")
  (setq git-gutter:deleted-sign "†")
  (set-face-foreground 'git-gutter:added "Green")
  (set-face-foreground 'git-gutter:modified "Gold")
  (set-face-foreground 'git-gutter:deleted "Red"))

(use-package all-the-icons
  :if (display-graphic-p))
(defun aorst/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))
(use-package all-the-icons
  :config
  (when (and (not (aorst/font-installed-p "all-the-icons"))
             (window-system))
    (all-the-icons-install-fonts t)))
;; If you experience a slow down in performance when rendering multiple icons simultaneously, you can try setting the following variable:
(setq inhibit-compacting-font-caches t)

;; :config (all-the-icons-install-fonts 'install-without-asking))
;; (cl-defun all-the-icons-faicon (icon &rest _)
;;   #("" 0 1 (rear-nonsticky t display (raise -0.24) font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2)))))

(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle 0.5
        which-key-idle-dely 50)
  (which-key-setup-minibuffer))
;; :custom
;; (which-key-separator " ")
;; (which-key-prefix-prefix "+")
;; (which-key-setup-side-window-right))

;; Goto last change
;; Sometimes it's useful to step to the last changes in a buffer.
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
  :init (global-undo-tree-mode)
  :custom
  ;; Show a diff window displaying changes between undo nodes.
  (undo-tree-visualizer-diff t)
  ;; Prevent undo tree files from polluting your git repo
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo-tree-hist")))
  ;; Each node in the undo tree should have a timestamp.
  (undo-tree-visualizer-timestamps t))

(use-package winner
  :doc "a minor mode that records your window configurations and lets you undo and redo changes made to it."
  :config
  (winner-mode 1)
  :bind (("M-[" . winner-undo)
         ("M-]" . winner-redo))
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*")))

(use-package aggressive-indent
  :doc "Intended Indentation"
  :init
  (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :delight)

;; Opening Files Externally
(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "sxiv"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(use-package beacon
  :init
  (beacon-mode t)
  (setq beacon-color "#50D050"))

(use-package emojify
  :if (display-graphic-p)
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-emoji-styles '(unicode)))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config
  (solaire-global-mode +1))

(setq custom-safe-themes t)
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (load-theme 'doom-gruvbox t)
  (if (display-graphic-p)
      (progn
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)
        ))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; ───────────────────────────────── MODE-LINE ─────────────────────────────────
;; When displaying the time with display-time-mode, I don’t care about the load average.
;; (setq display-time-default-load-average nil)
;; (display-time-mode)
;; (setq display-time-format "%l:%M%P (%a) %e %b ♪") ; %D for date format
;; (setq battery-mode-line-format "[%b%p%% %t]")
;; (display-battery-mode)
;; (size-indication-mode)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-flycheck-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 35))
;; (set-face-background 'mode-line nil)
;; ────────────────────────────────── IVY-MODE ─────────────────────────────────
(use-package ivy
  :doc "A generic completion mechanism"
  :init (ivy-mode 1)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("<f6>" . ivy-resume)
  	     ("C-c v" . ivy-push-view)
  	     ("C-c V" . ivy-pop-view)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-truncate-lines t)
  (ivy-wrap t)
  (ivy-use-selectable-prompt t)
  (ivy-count-format "[%d/%d] ")
  (enable-recursive-minibuffers t)
  ;; By default, all ivy prompts start with `^'. Disable that.
  (ivy-initial-inputs-alist nil)
  :delight)
(use-package ivy-avy)
(use-package ivy-hydra)
(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1)
  :delight)

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :after ivy
  :custom
  (ivy-posframe-border-width 6)
  :config
  (when (member "Hasklig" (font-family-list))
    (setq ivy-posframe-parameters
          '((font . "Hasklig"))))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-rg . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode t)
  :delight)

;; Prescient sorts and filters candidate lists for avy/counsel.
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package swiper
  :doc "A better search"
  :bind (("C-s" . swiper-isearch)) ; ("C-s" . swiper))
  :delight)
;; ──────────────────────────────── COUNSEL-MODE ───────────────────────────────
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-switch-buffer)
         ("C-c c" . counsel-compile)
         ("C-c F" . counsel-org-file)
         ("C-c g" . counsel-git)
         ("C-c i" . counsel-imenu)
         ("C-c j" . counsel-git-grep)
         ("C-c f" . counsel-file-jump)
         ("C-x l" . counsel-locate)
         ("C-c L" . counsel-git-log)
         ("C-c m" . counsel-linux-app)
         ("C-c n" . counsel-fzf)
         ("C-c o" . counsel-outline)
         ;; ("C-c T" . counsel-load-theme)
         ("C-c z" . counsel-bookmark)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
		 ("<f1> f" . counsel-describe-function)
		 ("<f1> v" . counsel-describe-variable)
		 ("<f1> l" . counsel-load-library)
		 ("<f1> L" . counsel-find-library)
		 ("<f2> i" . counsel-info-lookup-symbol)
		 ("<f2> j" . counsel-set-variable)
		 ("<f2> u" . counsel-unicode-char))
                                        ; ("C-c /" . counsel-ag)
                                        ; ("C-c s" . counsel-rg)
                                        ; ("C-S-o" . counsel-rhythmbox)
  (:map counsel-find-file-map
        ("RET" . ivy-alt-done))
  :delight)

(use-package projectile
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-(" . er/mark-outside-pairs)))
;; ───────────────────────────────── FLY-CHECK ─────────────────────────────────
(use-package flycheck
  :diminish
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  ;; Set fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  :bind
  ((:map flycheck-error-list-mode-map
	     ("q" . delete-window)
	     ("j" . flycheck-error-list-next-error)
	     ("k" . flycheck-error-list-previous-error)))
  :custom-face
  (flycheck-warning ((t (:underline (:color "#fabd2f" :style line :position line)))))
  (flycheck-error ((t (:underline (:color "#fb4934" :style line :position line)))))
  (flycheck-info ((t (:underline (:color "#83a598" :style line :position line))))))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

(defun hs-mode-and-hide ()
  "Turn on code folding and folds all code blocks."
  (interactive)
  (hs-minor-mode)
  (hs-hide-all))

;; code folding
(add-hook 'prog-mode-hook 'hs-mode-and-hide)
;; (global-set-key (kbd "C-c h") 'hs-hide-all)
;; (global-set-key (kbd "C-c s") 'hs-show-all)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)  ; fold the current section
(global-set-key (kbd "<backtab>") 'hs-hide-level)  ; fold the sub sections of the current section

;; Center text in the frame, looks nice ;)
(use-package olivetti
  :diminish
  :hook (text-mode . olivetti-mode)
  :hook (prog-mode . olivetti-mode)
  :hook (Info-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 130))

;; ─────────────────────────────────── C/C++ ─────────────────────────────────── ;;
;; (use-package flycheck-clang-analyzer
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (require 'flycheck-clang-analyzer)
;;      (flycheck-clang-analyzer-setup)))

;; ──────────────────────────────────── SHELL ───────────────────────────────────
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize "[" 'face '(:foreground "red" :weight bold))
         (propertize (user-login-name) 'face '(:foreground "yellow" :weight bold))
         (propertize "@" 'face '(:foreground "green" :weight bold))
         (propertize (system-name) 'face '(:foreground "blue" :weight bold))" "
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face '(:foreground "tomato" :weight bold))
           (propertize (eshell/basename (eshell/pwd)) 'face '(:foreground "magenta")))
         (propertize "]" 'face '(:foreground "red" :weight bold))
         (if (= (user-uid) 0) "# "
           (concat  "$ " )))))

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

(global-set-key (kbd "C-!") 'eshell)

(defun eshell/x ()
  "Cmnd `x' exits that shell and closes that window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;;; Eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell Eshell is
;; an elisp shell. It has its own configuration parameters, distinct from those of
;; shell or ansi-terminal.
;;;; Eshell Settings
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

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode t))

(use-package company-wordfreq)
;; ──────────────────────────────── Basic Utils ────────────────────────────────
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t)))) ; Garbage collection on focus-out, Emacs should feel snappier
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;Remove trailing whitespace on save

;; Browse source tree with Speedbar file browser
(setq speedbar-show-unknown-files t)
;; (setq company-backends (delete 'company-semantic company-backends))

;; ──────────── This snippet loads all *.el files in a directory ─────────── ;;
(defun load-directory (dir)
  "Load all *.el from your .emacs.d directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/elpa/")

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


(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Don’t forget to restore file-name-handler-alist, otherwise TRAMP won’t work and compressed/encrypted files won’t open.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist doom--file-name-handler-alist)))


;; InitPrivate
;; Load init-private.el if it exists
(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))
;; -InitPrivate

;;; Finish up
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
