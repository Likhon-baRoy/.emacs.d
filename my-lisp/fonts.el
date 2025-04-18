;;; fonts.el --- Font configuration -*- lexical-binding: t; -*-

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(defun my/font-available-p (font-name)
  "Check if FONT-NAME is available (installed) on the system."
  (when (stringp font-name)
    (member font-name (font-family-list))))

(defun my/first-available-font (&rest fonts)
  "Return the first available font from FONTS."
  (seq-find #'my/font-available-p fonts))

;; Define fallback fonts
(defvar my/default-mono-font
  (my/first-available-font "JetBrainsMono" "Source Code Pro" "DejaVu Sans Mono" "Monospace")
  "Primary monospaced font.")

(defvar my/variable-font
  (my/first-available-font "Iosevka Aile" "Cantarell" "Noto Sans" "Sans")
  "Primary variable-pitch font.")

;; Apply default font settings
(when my/default-mono-font
  (set-face-attribute 'default nil
                      :font (font-spec :family my/default-mono-font :size 10.0 :weight 'regular))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :family my/default-mono-font :size 10.0 :weight 'regular)))

(when my/variable-font
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :family my/variable-font :size 10.5 :weight 'regular)))

;; Emoji support (Linux and Windows)
(let ((emoji-font (my/first-available-font "Noto Color Emoji" "Segoe UI Emoji")))
  (when emoji-font
    (set-fontset-font t 'symbol (font-spec :family emoji-font) nil 'prepend)
    (set-fontset-font "fontset-default" '(#xFE00 . #xFE0F) emoji-font)))

;; Optional logging for debugging
(message "🧱 Default mono font: %s" my/default-mono-font)
(message "🎨 Variable-pitch font: %s" my/variable-font)

(provide 'fonts)
;;; fonts.el ends here
