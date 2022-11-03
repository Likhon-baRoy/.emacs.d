;;; after-init.el --- LSP support for Exordium

;;; Commentary:

;;; Code:

(use-package flycheck
  :commands flycheck-mode
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix exordium-lsp-keymap-prefix)

(use-package lsp-mode
  :if exordium-lsp-mode-enable
  :hook ((c-mode-common  . lsp))
  :init
  (setq-default lsp-clients-clangd-executable
                (seq-find #'executable-find exordium-lsp-clangd-executable))
  :commands (lsp lsp-deferred)

  :config
  (if exordium-enable-which-key
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (setq lsp-clients-clangd-args exordium-lsp-clangd-args)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-flycheck-live-reporting t)
  ;; company mode configuration for lsp-mode
  (setq lsp-completion-provider :capf)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)

  ;; process buffer for the LSP server needs to be larger
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; semantic hilite via lsp server
  (setq lsp-enable-semantic-highlighting t)
  )

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position exordium-lsp-ui-doc-position
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position exordium-lsp-ui-flycheck-list-position
        lsp-ui-peek-enable t
        )
  :commands lsp-ui-mode)

(use-package helm-lsp
  :after (lsp-mode helm)
  :if exordium-helm-everywhere
  :commands
  (helm-lsp-workspace-symbol
   helm-lsp-global-workspace-symbol
   helm-lsp-code-actions))

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :requires (dap-lldb dap-cpptools dap-gdb-lldb)
  :init
  (setq lsp-enable-dap-auto-configure t)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

(use-package which-key
  :if exordium-enable-which-key
  :config
  (which-key-mode))

(provide 'after-init.el)
;;; after-init.el ends here
