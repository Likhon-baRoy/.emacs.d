;;; init.el --- Initialization file -*- lexical-binding: t; -*-
;;
;; Filename: init.el
;; Description: Initialize Zmacs (obviously Emacs :smile:)
;; Author: Likhon Sapiens
;; Copyright © 2022 Likhon Sapiens
;; Created: Thu Oct 29 10:15:28 2022 (-0400)
;; Version: 0.1
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Keywords: Zmacs .emacs.d init
;; Compatibility: emacs-version >= 28.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for Zmacs
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

;; ─────────────────────────────────── Server ──────────────────────────────────
;; Stop opening Emacs for each file. Set default open application to `emacsclient' or set it manually:
;; emacsclient --no-wait--alternate-editor=emacs [FILE]

(require 'server)
(unless (server-running-p)
  (server-start))

;; ────────────────────────────────── ENCODING ─────────────────────────────────
;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(when window-system (global-prettify-symbols-mode t))

;; ────────────────────────────── Generic packages ─────────────────────────────
(require 'package)
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-quickstart nil)
;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))

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
(eval-and-compile
  (unless (and (fboundp 'package-installed-p)
               (package-installed-p 'use-package))
    (package-refresh-contents) ; update archives
    (package-install 'use-package)) ; grab the newest use-package
  (if init-file-debug
      (setq use-package-compute-statistics t)
    (setq use-package-compute-statistics nil))
  (require 'use-package))

;;; Configure use-package
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-defer nil)        ; :defer t by default
  (use-package-always-ensure t)         ; :ensure t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

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

;; ──────────────────────────────── Spellcheck ────────────────────────────────
(use-package ispell
  :bind ("<f8>" . ispell-word) ; easy spell check
  :custom
  (ispell-program-name "hunspell") ; require Hunspell
  (ispell-dictionary "en_US,en_GB,bn_BD")
  (ispell-personal-dictionary "~/.emacs.d/.hunspell_personal")
  :config
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;"        . nil)
              ("C-,"        . nil)
              ("C-."        . nil)
              ("S-<f8>"     . flyspell-buffer) ; C-M-S-<f1>
              ("C-<f7>"     . flyspell-auto-correct-word)
              ("C-<f12>"    . flyspell-auto-correct-previous-word))
  :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :delight " ⓢ")

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

;; ───────────────────────────────── Web-mode ─────────────────────────────────
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
(require 'dired)
(setq dired-listing-switches "-agho --group-directories-first"
      dired-omit-files "^\\.[^.].*"
      dired-omit-verbose nil
      dired-dwim-target t ; Copy and move files netween dired buffers
      dired-hide-details-hide-symlink-targets nil
      delete-by-moving-to-trash t)

(autoload 'dired-omit-mode "dired-x")

(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (hl-line-mode 1)))
(define-key dired-mode-map ")" #'dired-omit-mode)

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

;; ──────────────────────── General But Better Defaults ────────────────────────
(setq-default
 ad-redefinition-action 'accept     ; Silence warnings for redefinition.
 confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
 cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
 speedbar t                         ; Quick file access with bar.
 backup-by-copying t                ; don't clobber symlinks.
 backup-directory-alist `(("."~/.emacs.d/var/backup/per-session))
 default-directory "~/"
 custom-safe-themes t
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
 vc-follow-symlinks t              ; always follow git symlinks
 enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
 dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
 backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
 )
(require 'paren)
(show-paren-mode 1)            ; Highlight matching parenthesis.
(setq show-paren-delay 0)      ; how long to wait?
(setq show-paren-style 'mixed) ; alternatives are 'expression' and 'parenthesis'
(custom-set-faces
 '(show-paren-match ((t (:background "#3c3836" :foreground "white" :weight extra-bold))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white"))))
 )

;; Space around the windows
;; (set-fringe-style '(8 . 8))
;; (fringe-mode '(8 . 0))      ; Enable fringe on the left for git-gutter-fringe+.
(global-subword-mode 1)     ; Iterate through CamelCase words.
(global-auto-revert-mode 1) ; Automatically revert buffer when it changes on disk.
(electric-pair-mode 1)     ; Enable Matching delimiters.
(electric-indent-mode nil)  ; Auto indentation.

;; make electric-pair-mode work on more brackets.
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\{ . ?\})))

;; use this only when `electric-pair' is active

;; Disable electric-pair-mode in minibuffer during Macro definition
(defvar my-electic-pair-modes '(c-mode c++-mode lisp-mode emacs-lisp-mode org-mode))

(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode my-electic-pair-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

;; disable `<>' auto pairing in electric-pair-mode for org-mode
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; display white spaces and newlines
;; (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))
;; (global-whitespace-mode)

;; show zero-width characters
(set-face-background 'glyphless-char "red")

;;zone out the display when it goes idle for a given length of tim
(setq zone-idle-time 300)
(setq zone-timer (run-with-idle-timer zone-idle-time t 'zone))
(setq zone-programs [
		             zone-pgm-drip
		             zone-pgm-drip-fretfully
		             ])
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
 help-window-select t               ; select help window when opened
 redisplay-skip-fontification-on-input t
 tab-always-indent 'complete        ; smart tab behavior - indent or complete.
 visible-bell t                     ; Flash the screen on error, don't beep.
 view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
 use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
 echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
 delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
 ;; Auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 save-place-forget-unreadable-files nil
 blink-matching-paren t             ; Blinking parenthesis.
 next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
 require-final-newline t            ; require final new line.
 mouse-sel-retain-highlight t       ; keep mouse high-lighted.
 highlight-nonselected-windows nil
 transient-mark-mode t              ; highlight the stuff you are marking.
 ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
 pgtk-wait-for-event-timeout 0.001
 display-line-numbers-type 'relative
 speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
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
           (auto-fill-function " AF")
           (visual-line-mode)
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

;; From: https://panadestein.github.io/emacsd/#org5278580
(use-package emacs
  :preface
  (defun my-reload-emacs ()
    "Reload the Emacs configuration"
    (interactive)
    (load-file "~/.emacs.d/init.el"))
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
  :config
  (if init-file-debug
      (setq warning-minimum-level :debug)
    (setq warning-minimum-level :emergency))
  (fringe-mode '(0 . 0))
  ;; Terminal transparency
  (face-spec-set 'default
                 '((((type tty)) :background "unspecified-bg")))
  ;; Remember line number
  (if (fboundp #'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t))
  ;; Mimetypes
  (setq mailcap-user-mime-data
        '((type . "application/pdf")
          (viewer . pdf-view-mode)))
  :bind
  (("C-c R" . my-reload-emacs))
  ;;  ("<escape>" . keyboard-escape-quit) ; Make ESC close prompts
  ;;  ("C-c C-r" . revert-buffer-no-confirm)
  )

;; Emacs built-in ediff is more powerful than vimdiff IMHO.
;; However, the default configuration can be improved a bit:

;; (use-package ediff
;;   :preface
;;   (defvar my-ediff-original-windows nil)
;;   (defun my-store-pre-ediff-winconfig ()
;;     "Stores the window arrangement before opening ediff."
;;     (setq my-ediff-original-windows (current-window-configuration)))
;;   (defun my-restore-pre-ediff-winconfig ()
;;     "Resets original window arrangement"
;;     (set-window-configuration my-ediff-original-windows))
;;   :hook
;;   ((ediff-before-setup . my-store-pre-ediff-winconfig)
;;    (ediff-quit . my-restore-pre-ediff-winconfig))
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain
;;         ediff-split-window-function 'split-window-horizontally))

(use-package proced
  :commands proced
  :bind ("M-<f12>" . 'proced)
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

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

(use-package avy
  :bind(("C-'" . 'avy-goto-char)
        ("C-:" . 'avy-goto-char-2)
        ("M-g g" . 'avy-goto-line)
        ("M-g e" . 'avy-goto-word-0)
        ("M-g w" . 'avy-goto-word-1)
        ;; ("M-" . 'avy-copy-line)
        ;; ("M-" . 'avy-copy-region)
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

;; allow to edit files which require `root' permission with emacs.
;; (use-package sudo-edit
;;   :bind ("s-e" . sudo-edit))

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
  (recentf-auto-cleanup 'never) ; "05:00am") or (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50))

(use-package magit
  :doc "Git integration for Emacs"
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind ("C-x g" . magit-status)
  :delight)

(use-package git-gutter
  :delight
  :hook ((prog-mode org-mode) . git-gutter-mode )
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "†") ; ✘
  (setq git-gutter:added-sign "†")
  (setq git-gutter:deleted-sign "†")
  (set-face-foreground 'git-gutter:added "Green")
  (set-face-foreground 'git-gutter:modified "Gold")
  (set-face-foreground 'git-gutter:deleted "Red"))

(use-package which-key
  :defer t
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

(use-package helpful
  :doc "Helpful improves the built-in Emacs help system by providing more contextual information."
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("<f1> f" . helpful-function)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

(use-package eyebrowse
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
            (eyebrowse-mode t)
            (setq eyebrowse-wrap-around t)
            (setq eyebrowse-new-workspace t)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-minibuffer-flag t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   ;; :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (ace-window-display-mode 1))

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

(use-package goto-chg
  :bind (("C-;"     . goto-last-change); "C-c b ,"
         ("C-c b ." . goto-last-change-reverse))
  :defer t)

(use-package try
  :defer t)

(use-package winner
  :doc "a minor mode that records your window configurations and lets you undo and redo changes made to it."
  :config (winner-mode 1)
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

(use-package drag-stuff
  :hook ((prog-mode org-mode) . drag-stuff-mode )
  :bind
  ("C-M-j" . drag-stuff-down)
  ("C-M-k" . drag-stuff-up))

(use-package aggressive-indent
  :defer t
  :doc "Intended Indentation"
  ;; :hook ((prog-mode org-mode) . aggressive-indent-mode)
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :delight)
;; (add-to-list 'aggressive-indent-excluded-modes 'snippet-mode)
(add-hook 'snippet-mode-hook (lambda () (electric-indent-mode -1) (aggressive-indent-mode -1)))

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :delight
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-auto-enabled t)
  ;; (highlight-indent-guides-character ?\┆) ;; Indent character samples: | ┆ ┊
  :hook (prog-mode  . highlight-indent-guides-mode)
  ) ; end indent-guides

;; Highlight Volatile Things ?
;; (use-package volatile-highlights
;; :diminish
;; :commands volatile-highlights-mode
;; ;:hook
;; ;(after-init . volatile-highlights-mode)
;; :custom-face
;; (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))
                                        ;end volatile highlites

;; (set-face-background 'highlight "#3e4446") ; also try: "#3e4446"/"#gray6"
;; (set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")

(when window-system
  (use-package hl-line
    :hook ((prog-mode text-mode) . hl-line-mode)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package beacon
  :commands beacon-mode
  :init (beacon-mode t)
  :config
  (setq
   beacon-blink-when-window-changes t   ; only flash on window/buffer changes...
   beacon-blink-when-window-scrolls nil
   beacon-blink-when-point-moves nil
   beacon-dont-blink-commands nil
   beacon-blink-when-focused t
   beacon-blink-duration .5
   beacon-blink-delay .5
   beacon-push-mark 1
   beacon-color "#50D050"
   beacon-size 20)
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

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :delight)

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode +1)
  :delight)

(use-package doom-themes
  :if window-system
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
;; ──────────────────────────────── MODE-LINE ─────────────────────────────────
(size-indication-mode)
;; (display-battery-mode)
;; (setq display-time-24hr-format t)
(setq display-time-format "%l:%M%p" ;  %b %y"
      display-time-default-load-average nil)
(display-time-mode)
;; (setq display-time-format "%l:%M%P (%a) %e %b ♪") ; %D for date format

;; ────────────────────────────────── Fonts ───────────────────────────────────
(global-font-lock-mode 1)               ; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)   ; We have CPU to spare; highlight all syntax categories.

;; Set the font face
(cond ((aorst/font-installed-p "JetBrainsMono")
       (set-face-attribute 'default nil :font (font-spec :family "JetBrainsMono" :size 10.0 :weight 'regular))
       (set-face-attribute 'fixed-pitch nil :font (font-spec :family "JetBrainsMono" :size 10.0 :weight 'regular)))
      ((aorst/font-installed-p "Source Code Pro")
       (set-face-attribute 'default nil :font "Source Code Pro 10")))
;; For variable pitched fonts Iosevka Aile is used if available.
(when (aorst/font-installed-p "Iosevka Aile")
  (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :height 18 :weight 'regular)))

(set-face-attribute 'font-lock-comment-face nil :family "Iosevka Aile Oblique" :height 106) ; :foreground "#5B6268"
(set-face-attribute 'font-lock-function-name-face nil :family "Iosevka Aile" :height 102 :slant 'italic :weight 'regular) ; 'medium
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; (set-frame-font "Comic Mono-10.5" nil t) ; "Monaco-9" "Fantasque Sans Mono-10.5" "Source Code Pro-10" "Fira Code-10"

(use-package ligature
  ;; :load-path "path-to-ligature-repo"
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

;; (use-package smartparens
;;   :init      (progn
;;                (require 'smartparens)
;;                (load-library "smartparens-config"))

;;   :config   (progn
;;               (smartparens-global-mode t)
;;               (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
;;               (sp-with-modes '(html-mode sgml-mode nxml-mode web-mode)
;;                              (sp-local-pair "<" ">")))
;;   :bind
;;   (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
;;    ("C-M-<backspace>" #'backward-kill-sexp)
;;    ("C-M-f" . sp-forward-sexp)
;;    ("C-M-b" . sp-backward-sexp)
;;    ("C-M-n" . sp-up-sexp)
;;    ("C-M-d" . sp-down-sexp)
;;    ("C-M-u" . sp-backward-up-sexp)
;;    ("C-M-p" . sp-backward-down-sexp)
;;    ("C-M-w" . sp-copy-sexp)
;;    ("M-s" . sp-splice-sexp)
;;    ("M-r" . sp-splice-sexp-killing-around)
;;    ("C-)" . sp-forward-slurp-sexp)
;;    ("C-}" . sp-forward-barf-sexp)
;;    ("C-(" . sp-backward-slurp-sexp)
;;    ("C-{" . sp-backward-barf-sexp)
;;    ("M-S" . sp-split-sexp)
;;    ("M-J" . sp-join-sexp)
;;    ("C-M-t" . sp-transpose-sexp))
;;   :delight " ⎎")

;; (use-package smartparens
;;   :init
;;   (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

;;   :config (smartparens-global-mode t)
;;   (sp-pair "'" nil :actions :rem)
;;   (sp-pair "`" nil :actions :rem)
;;   (setq sp-highlight-pair-overlay nil))

;; (use-package smartparens
;;   :init (require 'smartparens-config)
;;   :config
;;   (smartparens-global-mode t) ; these options can be t or nil.
;;   (show-smartparens-global-mode t)
;;   (setq sp-show-pair-from-inside t)
;;   :custom-face
;;   (sp-show-pair-match-face ((t (:foreground "White")))) ;; Could also have :background "Grey" for example.
;;   )

(use-package minions
  :delight " 𝛁"
  :hook (doom-modeline-mode . minions-mode)
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]"))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-checker-simple-format t)
  (doom-line-numbers-style 'relative)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-flycheck-icon t)
  (doom-modeline-height 35))
;; (set-face-background 'mode-line nil)

;; ────────────────────────────────── IVY ─────────────────────────────────
(use-package ivy
  :doc "A generic completion mechanism"
  :init (ivy-mode 1)
  :bind (("C-s" . swiper)
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
  (ivy-wrap t)
  (ivy-truncate-lines t)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (ivy-count-format "【%d/%d】")
  (ivy-initial-inputs-alist nil) ; by default, all ivy prompts start with `^'
  (ivy-on-del-error-function nil)
  (enable-recursive-minibuffers t)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  :config
  ;; visual line mode on swiper scans every visual line, which can be really slow in large files.
  (setq swiper-use-visual-line-p #'ignore)
  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
  :delight)
;; ──────────────────────────────── COUNSEL ───────────────────────────────
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-ibuffer)
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
		 ;; ("<f1> l" . counsel-load-library)
		 ;; ("<f1> L" . counsel-find-library)
		 ("<f2> i" . counsel-info-lookup-symbol)
		 ("<f2> j" . counsel-set-variable)
		 ("<f2> u" . counsel-unicode-char))
  ;; ("C-c /" . counsel-ag)
  ;; ("C-c s" . counsel-rg)
  ;; ("C-S-o" . counsel-rhythmbox)
  (:map counsel-find-file-map
        ("RET" . ivy-alt-done))
  ;; (:map minibuffer-local-map
  ;;       ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :delight)

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :after ivy
  :custom
  (ivy-posframe-border-width 6)
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (when (member "Iosevka Aile" (font-family-list))
    (setq ivy-posframe-parameters
          '((font . "Iosevka Aile"))))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-rg . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode t)
  :delight " Ⓥ")

;; Prescient sorts and filters candidate lists for avy/counsel.
(use-package prescient
  :after counsel
  :config (prescient-persist-mode 1)
  :delight)

;; History for ivy completion, it sometimes makes ivy really slow, so maybe remove the cache file every once in a while
(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode t)
  :delight)

(use-package swiper
  :defer t
  :doc "A better search"
  ;; :bind (("C-s" . swiper-isearch))
  :delight)

(use-package ivy-avy
  :after ivy)

(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :after counsel
  :init (ivy-rich-mode 1)
  :custom (ivy-rich-path-style 'abbreviate)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))); return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode))))))))
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
  :bind (("C-S-SPC" . er/expand-region)
         ("C--" . er/contract-region)
         ("C-(" . er/mark-outside-pairs)
         ("C-)" . er/mark-inside-pairs)))

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
  :delight " ⊗") ; Ⓐ ⊛

(use-package fancy-battery
  :config
  (setq fancy-battery-show-percentage t)
  (setq battery-update-interval 15)
  (if window-system
      (fancy-battery-mode)
    (display-battery-mode)))

;; ──────────────────────────────── Basic Utils ────────────────────────────────
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t)))) ; Garbage collection on focus-out, Emacs should feel snappier
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; remove trailing whitespace on save

;;; Load Path
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

;; ──────────── This snippet loads all *.el files in a directory ───────────
(defun load-directory (dir)
  "Load all *.el from your .emacs.d directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; (load-directory "~/.emacs.d/elpa/") ; load installed packages
(load-directory "~/.emacs.d/my-lisp") ; load my configuration of packages

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
  (dashboard-startup-banner (concat user-emacs-directory "etc/banners/ue-colorful.png"))
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
                     (projects       . 2)
                     (bookmarks      . 5)
                     (agenda         . 3)
                     (registers      . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground nil :weight bold))))) ; "#f1fa8c"

(setq eglot-events-buffer-size 0)

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
