;;; fonts.el --- Font configuration -*- lexical-binding: t; -*-
;;
;; Filename: fonts.el
;; Description: This file holds my GNU Emacs Fonts settings
;; Author: Likhon Baroy
;; Copyright © 2022-present Likhon Baroy <likhonhere007@gmail.com>
;; Created: 2025 April 25
;; Version: 0.1
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Keywords: Zmacs .emacs.d Fonts
;; Compatibility: emacs-version >= 28.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the fonts.el file for Zmacs
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.
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


;; ────────────────────────────── General Settings ─────────────────────────────
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
  (my/first-available-font "JetBrains Mono" "Source Code Pro" "DejaVu Sans Mono" "Monospace")
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
