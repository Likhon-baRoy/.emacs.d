;;; auto-compilation.el --- settings for autocompilation -*- lexical-binding: t; -*-
;;; Created on: 25 November 2022

;; Copyright (C) 2021-2022 Likhon Sapins <likhonhere007@gmail.com>

;; Author: Likhon Sapins <likhonhere007@gmail.com>
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((Emacs "28.1"))

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


;; ──────────────────────────────── COMPANY-MODE ───────────────────────────────
(use-package company-box
  :delight
  :after company
  :hook (company-mode . company-box-mode))
;; (set-face-background 'company-box--apply-color "#555555")

(use-package company
  :defer 2
  :delight " Ⱞ"
  :commands company-mode
  ;; :after prog-mode
  ;; :hook (prog-mode . company-mode)
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-h"        . nil)
        ("C-j"        . nil)
        ("C-k"        . nil)
        ("C-n"        . nil)
        ("C-p"        . nil)
        ("C-w"        . nil)
        ("SPC"        . nil) ; prevent SPC from ever triggering a completion.
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
  :custom
  (company-dabbrev-ignore-case t)
  (company-etags-ignore-case t)
  (company-idle-delay 0.0)
  (company-echo-delay 0)
  (completion-ignore-case t)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 1)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-ignore-case nil)
  (company-debbrev-other-buffers 'all)
  (company-dabbrev-downcase nil)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum company-tooltip-limit)
  (company-transformers '(company-sort-by-occurrence))
  ;; (company-begin-commands '(self-insert-command))
  ;; (company-require-match #'company-explicit-action-p)
  ;; (company-frontends '(company-pseudo-tooltip-frontend))
  :config
  (setq company-backends '((company-files        ; files & directory
                            company-keywords     ; keywords
                            company-capf         ; what is it?
                            company-cmake
                            company-c-headers
                            company-clang        ; too much slow
                            company-ispell
                            company-irony-c-headers
                            company-irony
                            company-dabbrev-code
                            company-semantic
                            company-yasnippet
                            company-gtags
                            company-etags
                            company-rtags
                            company-elisp)
                           (company-abbrev company-dabbrev)
                           (delete 'company-semantic company-backends)))
  )
;; Use TAB key to cycle through suggestions.(`tng' means 'TAB and go')
;; (company-tng-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-files        ; files & directory
                    company-keywords     ; keywords
                    company-capf
                    company-cmake
                    company-yasnippet
                    company-c-headers
                    company-clang        ; too much slow
                    company-irony-c-headers
                    company-irony
                    company-dabbrev-code
                    company-semantic
                    company-gtags
                    company-etags
                    company-rtags
                    company-elisp)))))

(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-files        ; files & directory
                    company-capf
                    company-yasnippet
                    company-ispell
                    company-abbrev
                    company-dabbrev
                    company-dabbrev-code)))))

;; Delete duplicates from company popups
(setq-local company-transformers '(delete-dups)
            company-backends '(company-files (:separate company-dabbrev company-ispell)))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Documentation popups for Company
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

;; Required for proportional font
(use-package company-posframe
  :config (company-posframe-mode t)
  :delight)

(use-package yasnippet
  :delight yas-minor-mode " ¥"
  :commands yas-reload-all
  :hook ((prog-mode minibuffer-inactive-mode org-mode) . yas-minor-mode)
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet))
  :custom (yas-snippet-dirs '("~/.emacs.d/etc/yasnippet/snippets"))
  :config
  ;; enalbe nested snippet expansion
  (setq yas-triggers-in-field t)
  (yas-global-mode)
  )

(use-package yasnippet-snippets
  :delight
  :after yas
  :custom (yasnippet-snippets-dirs '("~/.emacs.d/etc/yasnippet/snippets"))
  :config (yasnippet-snippets-initialize))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 0.0)
  (eldoc-echo-area-display-truncation-message nil)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c r"   . eglot-rename)
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-f" . eglot-format-buffer)
              ("C-c C-i" . eglot-find-implementation))
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("digestif")))
  ;; format on save
  (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (add-to-list 'company-backends
                                                  '(company-capf :with company-yasnippet))))

;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;;; Finish up
(provide 'auto-compilation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-compilation.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
