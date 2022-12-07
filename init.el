;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize Z-MACS (obviously Emacs :smile:)
;; Author: Likhon Sapiens
;; Copyright © 2022 Likhon Sapiens
;; Created: Thu Oct 29 10:15:28 2022 (-0400)
;; Version: 0.1
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Keywords: Z-MACS .emacs.d init
;; Compatibility: emacs-version >= 27.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for Z-MACS
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
(when window-system (global-prettify-symbols-mode t))
;; ────────────────────────────── Generic packages ─────────────────────────────
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))

(require 'cl)
(require 'package)
;; Configure Package Manager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   gcmh-idle-delay 'auto ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :delight " Ⓖ")

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
  :bind (:map flyspell-mode-map
              ("C-;"        . nil)
              ("M-<f7>" . flyspell-buffer)
              ("<f7>" . flyspell-word)
              ("C-<f7>" . flyspell-auto-correct-word)
              ("C-<f12>" . flyspell-auto-correct-previous-word))
  :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (setq ispell-program-name "hunspell" ; Requires Hunspell
        ispell-default-dictionary "en_GB")
  :delight " ⓢ")
;; ────────────────────────────────── WEB-MODE ─────────────────────────────────
(use-package emmet-mode
  :after (web-mode css-mode scss-mode)
  :commands (emmet-mode emmet-expand-line yas-insert-snippet company-complete)
  :config (setq emmet-move-cursor-between-quotes t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  ;; (setq emmet-indentation 2)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  :bind ("C-j" . emmet-expand-line)
  ((:map emmet-mode-keymap
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point)))
  );end emmet mode ;; enable Emmet's css abbreviation.

;; ──────────────────────────────────── GDB ────────────────────────────────────
;; Show main source buffer when using GDB
(setq gdb-show-main t) ; keep your source code buffer displayed in a split window.
;; (setq gdb-many-windows t) ; GDB interface supports a number of other windows

;;________________________________________________________________
;;    Separte Customization from init file
;;________________________________________________________________
(setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Load custom themes
(add-to-list 'custom-theme-load-path (expand-file-name "etc/themes/" user-emacs-directory))

;; ─────────────────────────────────── Emoji ───────────────────────────────────
;; Set up emoji rendering, requires installation of the Noto Emoji font for Linux.
;; Default Windows emoji font
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) "Segoe UI Emoji"))

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

;; (set-face-attribute 'default nil
;; 		            :font "Fantasque Sans Mono" ; "JetBrains Mono"
;; 		            :weight 'light
;; 		            :height (cond ((string-equal system-type "gnu/linux") 110)
;; 				                  ((string-equal system-type "darwin") 130)))
;; (set-face-attribute 'font-lock-comment-face nil :family "Cantarell" :slant 'italic :height 92)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "cyan" :slant 'italic :weight 'medium)
;; (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; (set-face-attribute 'font-lock-comment-face nil :foreground "#5B6268" :slant 'italic)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#c678dd" :slant 'italic :weight 'bold)
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)

;; (set-frame-font "Comic Mono-10.5" nil t)
;; (set-frame-font "Monaco-9" nil t)
;; (set-frame-font "Fantasque Sans Mono-10.5" nil t)
;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Fira Code-10" nil t)

;; ──────────────────────── General But Better Defaults ────────────────────────
(setq-default
 ad-redefinition-action 'accept     ; Silence warnings for redefinition.
 confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
 cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
 speedbar t                         ; Quick file access with bar.
 backup-by-copying t                ; don't clobber symlinks.
 backup-directory-alist `(("."~/.emacs.d/var/backup/per-session))
 default-directory "~/"
 load-prefer-newer t ; don't use the compiled code if its the older package.
 make-backup-files t               ; backup of a file the first time it is saved.
 delete-by-moving-to-trash t       ; move deleted files to trash.
 delete-old-versions t             ; delete excess backup files silently.
 kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2).
 kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2).
 version-control t                 ; version numbers for backup files.
 auto-save-default t               ; auto-save every buffer that visits a file.
 auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
 auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
 compilation-always-kill t         ; kill compilation process before starting another.
 compilation-ask-about-save nil    ; save all buffers on `compile'.
 compilation-scroll-output t
 tab-width 4
 indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
 indent-line-function 'insert-tab
 require-final-newline t
 x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
 save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
 apropos-do-all t                  ; Shows all options when running apropos.
 mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click.
 message-log-max 1000
 fill-column 80
 initial-scratch-message nil       ; Empty the initial *scratch* buffer.
 make-pointer-invisible t          ; hide cursor when writing.
 column-number-mode t              ; Show (line,column) in mode-line.
 cua-selection-mode t              ; Delete regions.
 enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
 backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
 )
(save-place-mode 1)
(show-paren-mode 1)         ; Highlight matching parenthesis.
(global-auto-revert-mode 1) ; Automatically revert buffer when it changes on disk.
;; (fringe-mode '(8 . 0))      ; Enable fringe on the left for git-gutter-fringe+.
(global-subword-mode 1)     ; Iterate through CamelCase words.
(electric-pair-mode t)      ; Enable Matching delimeters.
(electric-indent-mode nil)  ; Auto indentation.
;; make electric-pair-mode work on more brackets.
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
 debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
 jka-compr-verbose init-file-debug
 read-process-output-max (* 64 1024); 64kb
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0              ; default is 0.5.
 scroll-step 1                      ; scroll with less jump.
 scroll-preserve-screen-position t
 scroll-margin 3
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
 auto-window-vscroll nil            ; Lighten vertical scroll.
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 hscroll-step 1                     ; Horizontal Scroll.
 hscroll-margin 1
 redisplay-skip-fontification-on-input t
 tab-always-indent 'complete        ; smart tab behavior - indent or complete.
 visible-bell t                     ; Flash the screen on error, don't beep.
 view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
 use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
 echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
 delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
 save-place-forget-unreadable-files nil
 blink-matching-paren t             ; Blinking parenthesis.
 next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
 require-final-newline t            ; require final new line.
 mouse-sel-retain-highlight t       ; keep mouse high-lighted.
 highlight-nonselected-windows nil
 transient-mark-mode t              ; highlight the stuff you are marking.
 show-paren-delay 0           		; how long to wait?
 show-paren-style 'mixed      		; alternatives are 'expression' and 'parenthesis'.
 ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
 pgtk-wait-for-event-timeout 0.001
 frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b") ; name of the file I am editing as the name of the window.
 )

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
;; (set-face-background 'highlight "#3e4446") ; also try: "#3e4446"/"#gray6"
;; (set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")

;; ────────────────────────── Transparency Alpha Value ─────────────────────────
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; a feature that removes certain minor-modes from mode-line.
(use-package delight
  :delight)
(delight '((abbrev-mode " Abv" abbrev)
           (smart-tab-mode " \\t" smart-tab)
           (eldoc-mode nil "eldoc")
           (rainbow-mode)
           (overwrite-mode " Ov" t)
           (emacs-lisp-mode "Elisp" :major)))
;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
(global-dash-fontify-mode)

;; Benchmark startup
;; benchmark-init records startup time by package so we can debug. It only records things after it’s initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun aorst/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (when (and (not (aorst/font-installed-p "all-the-icons"))
             (window-system))
    (all-the-icons-install-fonts t)))

;; If you experience a slow down in performance when rendering multiple icons simultaneously, you can try setting the following variable:
(setq inhibit-compacting-font-caches t)

;; :config (all-the-icons-install-fonts 'install-without-asking))
;; (cl-defun all-the-icons-faicon (icon &rest _)
;;   #("" 0 1 (rear-nonsticky t display (raise -0.24) font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2)))))

;; ───────────────────────────────── DASHBOARD ─────────────────────────────────
;; A dashboard on startup can clean my mind
(use-package dashboard
  :after all-the-icons
  :init (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-image-banner-max-height 250)
  (dashboard-banner-logo-title "[Π Ο Σ Ε Ι Δ Ο Ν 🔱 Ε Δ Ι Τ Ο Ρ]") ; [Ποσειδον 🔱 εδιτορ]
  (dashboard-startup-banner (concat user-emacs-directory "etc/banners/emacs_pen.png"))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-footer-icon (all-the-icons-octicon "calendar"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "octoface" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/Likhon-baRoy/.emacs.d")) nil "" " |")
           (,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
            "Update"
            "Update Zmacs"
            (lambda (&rest _) (auto-package-update-maybe)) warning "" " |")
           (,(all-the-icons-faicon "flag" :height 1.1 :v-adjust 0.0) nil
            "Report a BUG"
            (lambda (&rest _) (browse-url "https://github.com/Likhon-baRoy/.emacs.d/issues/new")) error "" ""))
          ;; line 2
          ;; ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
          ;;   "AlienFriend"
          ;;   "Browse Alien Page"
          ;;   (lambda (&rest _) (browse-url "https://github.com/b-coimbra/.emacs.d")) nil "" ""))
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

  (setq
   dashboard-projects-backend 'project-el
   dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
   dashboard-items '((recents        . 5)
                     (projects       . 5)
                     (bookmarks      . 5)
                     (agenda         . 5)
                     (registers      . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground nil :weight bold))))) ; "#f1fa8c"

(use-package avy
  :bind(("C-'" . 'avy-goto-char)
        ("C-:" . 'avy-goto-char-2)
        ("M-g g" . 'avy-goto-line)
        ("M-g e" . 'avy-goto-word-0)
        ("M-g w" . 'avy-goto-word-1)
        ("M-g l" . 'avy-move-line)
        ("M-g M-r" . 'avy-move-region)
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
  (recentf-auto-cleanup 'never) ; "05:00am") ; or, recentf-auto-cleanup 'never
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50))

(use-package magit
  :doc "Git integration for Emacs"
  :bind ("C-x g" . magit-status)
  :delight)

(use-package git-gutter
  :delight
  :hook ((prog-mode org-mode) . git-gutter-mode )
  ;;✘
  :config
  (setq git-gutter:modified-sign "†")
  (setq git-gutter:added-sign "†")
  (setq git-gutter:deleted-sign "†")
  (set-face-foreground 'git-gutter:added "Green")
  (set-face-foreground 'git-gutter:modified "Gold")
  (set-face-foreground 'git-gutter:deleted "Red"))

(use-package which-key
  :delight
  :init (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle 0.5
        which-key-idle-dely 50)
  (which-key-setup-minibuffer))
;; :custom
;; (which-key-separator " ")
;; (which-key-prefix-prefix "+")
;; (which-key-setup-side-window-right))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   ;; :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o)))

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :delight
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

;; (use-package goto-last-change)
;; :bind (("C-;" . goto-last-change)))

(use-package goto-chg)

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
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
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
                  "png" "gif" "bmp" "tif" "jpeg")) ; Removed jpg because Telega was
               "sxiv" ; causing feh to be opened...
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(use-package beacon
  :init (beacon-mode t)
  (setq beacon-color "#50D050")
  :delight)

(use-package emojify
  :config (if (display-graphic-p)
              (setq emojify-display-style 'image)
            (setq emojify-display-style 'unicode)
            (setq emojify-emoji-styles '(unicode)))
  :init (global-emojify-mode +1))

(use-package alert
  :commands alert
  :config (setq alert-default-style 'notifications)
  :delight)

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :delight)

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode +1)
  :delight)

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
(size-indication-mode)
;; (display-battery-mode)
;; (display-time-mode)
;; (setq display-time-24hr-format t)
;; (setq display-time-default-load-average nil)
;; (setq battery-mode-line-format "[%b%p%% %t]")
;; (setq display-time-format "%H:%M - %d %B %Y")
;; (setq display-time-format "%l:%M%P (%a)%e %b ♪") ; %D for date format

(cond ((aorst/font-installed-p "JetBrainsMono")
       (set-face-attribute 'default nil :font "JetBrainsMono 10"))
      ((aorst/font-installed-p "Source Code Pro")
       (set-face-attribute 'default nil :font "Source Code Pro 10")))
;; For variable pitched fonts DejaVu font is used if available.
(when (aorst/font-installed-p "DejaVu Sans")
  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans 10"))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(when (aorst/font-installed-p "JetBrainsMono")
  (dolist (char/ligature-re
           `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
                               "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                           (+ "<"))))
             (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  ,(rx (+ "&")))
             (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                           (+ "|"))))
             (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  ,(rx (or "+>" (+ "+"))))
             (?\[ ,(rx (or "[<" "[|")))
             (?\{ ,(rx "{|"))
             (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
             (?\; ,(rx (+ ";")))
             (?_  ,(rx (or "_|_" "__")))
             (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  ,(rx "$>"))
             (?^  ,(rx "^="))
             (?\] ,(rx "]#"))))
    (apply (lambda (char ligature-re)
             (set-char-table-range composition-function-table char
                                   `([,ligature-re 0 font-shape-gstring])))
           char/ligature-re)))

(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :delight " 𝛁")

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
  (ivy-count-format "【%d/%d】")
  (enable-recursive-minibuffers t)
  ;; By default, all ivy prompts start with `^'. Disable that.
  (ivy-initial-inputs-alist nil)
  :delight)

(use-package ivy-avy
  :after ivy)

(use-package ivy-hydra
  :after ivy)

(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :after ivy
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
  :delight " Ⓥ")

;; Prescient sorts and filters candidate lists for avy/counsel.
(use-package prescient
  :delight)

(use-package ivy-prescient
  :after ivy
  :config (ivy-prescient-mode t)
  :delight)

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
  :delight '(:eval (concat " [" projectile-project-name "]"))
  :pin melpa-stable
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

;; Explanation-Mark !
;; (when window-system
;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
;;     [0 24 24 24 24 24 24 0 0 24 24 0 0 0 0 0 0]))

;; BIG BitMap-Arrow
;; (when (fboundp 'define-fringe-bitmap)
;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
;;     [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (("M-g M-j" . flycheck-next-error)
         ("M-g M-k" . flycheck-previous-error)
         ("M-g M-l" . flycheck-list-errors))
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Small BitMap-Arrow
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  :custom-face
  (flycheck-warning ((t (:underline (:color "#fabd2f" :style line :position line)))))
  (flycheck-error ((t (:underline (:color "#fb4934" :style line :position line)))))
  (flycheck-info ((t (:underline (:color "#83a598" :style line :position line)))))
  :delight " ∰") ; "Ⓢ"

(use-package flycheck-popup-tip
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;; catch symbolic error on `ligature-mode', so disabled it.
;; (use-package flycheck-clang-tidy
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-clang-tidy-setup))

;; syntax highlight of the latest C++ language.
(use-package modern-cpp-font-lock
  :delight)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Center text in the frame, looks nice ;)
(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80)
  :delight " ⊛") ; "Ⓐ" "⊗"

;; Required for proportional font
(use-package company-posframe
  :config (company-posframe-mode t)
  :delight)

(use-package company-wordfreq
  :delight " 𝛄")

(use-package fancy-battery
  :config
  (setq fancy-battery-show-percentage t)
  (setq battery-update-interval 15)
  (if window-system
      (fancy-battery-mode)
    (display-battery-mode)))

;; ──────────────────────────────── Basic Utils ────────────────────────────────
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t)))) ; Garbage collection on focus-out, Emacs should feel snappier
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;Remove trailing whitespace on save

;; Browse source tree with Speedbar file browser
(setq speedbar-show-unknown-files t)
;; (setq company-backends (delete 'company-semantic company-backends))

;; ──────────── This snippet loads all *.el files in a directory ───────────
(defun load-directory (dir)
  "Load all *.el from your .emacs.d directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory "~/.emacs.d/elpa/")   ; load installed packages
(load-directory "~/.emacs.d/my-lisp") ; load my configuration of packages


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
