;;; flycheck-popup-tip.el --- Display Flycheck error messages using popup.el

;; Copyright (C) 2017 Saša Jovanić

;; Author: Saša Jovanić <sasa@simplify.ba>
;; URL: https://github.com/flycheck/flycheck-popup-tip/
;; Package-Commit: ef86aad907f27ca076859d8d9416f4f7727619c6
;; Keywords: convenience, tools, flycheck, tooltip
;; Version: 0.12.2
;; Package-Version: 20170812.2351
;; Package-X-Original-Version: 0.12.2
;; Package-Requires: ((flycheck "0.22") (popup "0.5") (emacs "24"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is extension for Flycheck.

;; It displays Flycheck error messages in buffer using `popup.el' library.

;; For more information about Flycheck:
;; http://www.flycheck.org/
;; https://github.com/flycheck/flycheck

;; For more information about this Flycheck extension:
;; https://github.com/flycheck/flycheck-popup-tip

;;;; Setup

;; Add to your `init.el':
;;
;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;;; Code:

(require 'flycheck)
(require 'popup)

(defgroup flycheck-popup-tip nil
  "Display Flycheck errors in tooltips using popup.el."
  :prefix "flycheck-popup-tip-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-popup-tip"))

(defcustom flycheck-popup-tip-error-prefix "\u27a4 "
  "String to be displayed before every error line in popup."
  :group 'flycheck-popup-tip
  :type 'string
  :package-version '(flycheck-popup-tip . "0.10"))

(defvar flycheck-popup-tip-object nil
  "Temp popup object.")

(defvar flycheck-popup-tip-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

(defun flycheck-popup-tip-delete-popup ()
  "Delete messages currently being shown if any."
  (when (popup-live-p flycheck-popup-tip-object)
    (popup-delete flycheck-popup-tip-object))
  (remove-hook 'pre-command-hook 'flycheck-popup-tip-delete-popup t))

(defun flycheck-popup-tip-format-errors (errors)
  "Formats ERRORS messages for display."
  (let* ((messages-and-id (mapcar #'flycheck-error-format-message-and-id
                                  (delete-dups errors)))
         (messages (sort
                    (mapcar
                     (lambda (m) (concat flycheck-popup-tip-error-prefix m))
                     messages-and-id)
                    'string-lessp)))
    (propertize (mapconcat 'identity messages "\n")
                'face
                '(:inherit popup-tip-face
                           :underline nil
                           :overline nil
                           :strike-through nil
                           :box nil
                           :slant normal
                           :width normal
                           :weight normal))))

(defun flycheck-popup-tip-show-popup (errors)
  "Display ERRORS, using popup.el library."
  (flycheck-popup-tip-delete-popup)
  (when errors
    (setq flycheck-popup-tip-object
          (popup-tip
           (flycheck-popup-tip-format-errors errors)
           :nostrip t
           :nowait t))
    (add-hook 'pre-command-hook 'flycheck-popup-tip-delete-popup nil t)))

;;;###autoload
(define-minor-mode flycheck-popup-tip-mode
  "A minor mode to show Flycheck error messages in a popup."
  :lighter nil
  :group 'flycheck-popup-tip
  (let ((hooks '(post-command-hook focus-out-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and flycheck-popup-tip-mode
           (not (eq flycheck-display-errors-function
                    #'flycheck-popup-tip-show-popup)))
      (setq flycheck-popup-tip-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'flycheck-popup-tip-show-popup)
      (dolist (hook hooks)
        (add-hook hook #'flycheck-popup-tip-delete-popup nil t)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not flycheck-popup-tip-mode)
           (eq flycheck-display-errors-function
               #'flycheck-popup-tip-show-popup))
      (setq flycheck-display-errors-function
            flycheck-popup-tip-old-display-function
            flycheck-popup-tip-old-display-function nil)
      (dolist (hook hooks)
        (remove-hook hook 'flycheck-popup-tip-delete-popup t))))))

(provide 'flycheck-popup-tip)

;;; flycheck-popup-tip.el ends here
