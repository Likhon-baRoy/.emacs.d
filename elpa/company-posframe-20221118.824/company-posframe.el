;;; company-posframe.el --- Use a posframe as company candidate menu       -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Clément Pit-Claudel, Feng Shu, Lars Andersen

;; Author: Clément Pit-Claudel, Feng Shu, Lars Andersen <expez@expez.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Package-Version: 20221118.824
;; Package-Commit: ab58972c2cebc5ecf68c4cdd140c3aed2c68f42b
;; Version: 0.6.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.9.0"))

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

;; * company-posframe README                                :README:
;; ** What is company-posframe
;; company-posframe is a company extension, which let company use
;; child frame as its candidate menu.

;; It has the following feature:
;; 1. It is fast enough for daily use.
;; 2. It works well with CJK language.

;; [[./snapshots/company-posframe.png]]

;; ** How to use company-posframe

;; #+BEGIN_EXAMPLE
;; (require 'company-posframe)
;; (company-posframe-mode 1)
;; #+END_EXAMPLE

;; ** Tips

;; *** How to reduce flicker when scroll up and down?
;; In windows or MacOS system, company candidates menu may flicker
;; when scroll up and down, the reason is that the size of posframe
;; changing rapid, user can set the minimum width of menu to limit
;; flicker, for example:

;; #+BEGIN_EXAMPLE
;; (setq company-tooltip-minimum-width 40)
;; #+END_EXAMPLE

;; *** Work better with desktop.el
;; The below code let desktop.el not record the company-posframe-mode

;; #+BEGIN_EXAMPLE
;; (require 'desktop) ;this line is needed.
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; #+END_EXAMPLE

;; ** Note
;; company-posframe.el is derived from Clément Pit-Claudel's
;; company-tooltip.el, which can be found at:

;; [[https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511]]

;; Some quickhelp functions is come from:

;; [[https://github.com/company-mode/company-quickhelp][company-quickhelp]]


;;; Code:
;; * company-posframe's code
(require 'cl-lib)
(require 'company)
(require 'posframe)
(require 'subr-x)

(defgroup company-posframe nil
  "Use a child-frame as company candidate menu"
  :group 'company
  :prefix "company-posframe")

(defcustom company-posframe-font nil
  "The font used by company-posframe's frame.
Using current frame's font if it it nil."
  :group 'company-posframe
  :type 'face)

(defcustom company-posframe-lighter " company-posframe"
  "The lighter string used by `company-posframe-mode'."
  :group 'company-posframe
  :type 'string)

(defcustom company-posframe-show-indicator nil
  "Display an indicator for backends in the mode line of the posframe."
  :group 'company-posframe
  :type 'boolean)

(defcustom company-posframe-show-metadata nil
  "Display metadata (e.g. signature) of the selection below the visible candidates."
  :group 'company-posframe
  :type 'boolean)

(defcustom company-posframe-backend-separator "|"
  "String used to separate entries in the backend indicator."
  :group 'company-posframe
  :type 'string)

(defcustom company-posframe-quickhelp-x-offset 0
  "Horizontal offset for company posframe quickhelp."
  :group 'company-posframe
  :type 'integer)

(defcustom company-posframe-backend-format-function
  #'company-posframe-format-backend-name-active-first
  "Function used to format each backend in the indicator."
  :group 'company-posframe
  :type '(choice (const :tag "show in order"
                        company-posframe-format-backend-name)
                 (const :tag "show active backend first"
                        company-posframe-format-backend-name-active-first)
                 (function :tag "custom function" nil)))

(defcustom company-posframe-quickhelp-delay 1
  "Delay, in seconds, before the quickhelp popup appears.

If set to nil the popup won't automatically appear, but can still
be triggered manually using `company-posframe-quickhelp-show'."
  :type '(choice (number :tag "Delay in seconds")
                 (const :tag "Don't popup help automatically" nil)))

(defcustom company-posframe-quickhelp-show-header t
  "Display a header for the posframe quickhelp frame."
  :group 'company-posframe
  :type 'boolean)

(defface company-posframe-inactive-backend-name
  '((t :inherit mode-line))
  "Face for the active backend name in the header line.")

(defface company-posframe-active-backend-name
  '((t :inherit mode-line-emphasis))
  "Face for the active backend name in the header line.")

(defface company-posframe-metadata
  '((t :inherit font-lock-comment-face))
  "Face for the metadata footer (not the backend indicator).")

(defface company-posframe-quickhelp
  '((t :inherit company-tooltip))
  "Face for company-posframe-quickhelp doc.")

(defface company-posframe-quickhelp-header
  '((t :inherit company-tooltip-selection
       :box nil
       :extend t))
  "Face for company-posframe-quickhelp header.")

(defvar company-posframe-buffer " *company-posframe-buffer*"
  "company-posframe's buffer which used by posframe.")

(defvar company-posframe-quickhelp-buffer
  " *company-posframe-quickhelp-buffer*"
  "The buffer which used by company-posframe-quickhelp.")

(defvar-local company-posframe-quickhelp-timer nil
  "Quickhelp idle timer.")

(defvar company-posframe-show-params nil
  "List of extra parameters passed to `posframe-show' in
  `company-posframe-show'.")

(defvar company-posframe-poshandler
  #'company-posframe-show-at-prefix
  "Poshandler for the completion dialog.")

(defvar company-posframe-quickhelp-show-params
  (list :poshandler #'company-posframe-quickhelp-right-poshandler
        :timeout 60
        :no-properties nil)
  "List of parameters passed to `posframe-show'.")

(defvar company-posframe-notification "")

(defvar company-posframe-last-status nil)

(defvar company-posframe-active-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [wheel-down] 'ignore)
    (define-key keymap [wheel-up] 'ignore)

    ;; Quickhelp keys.
    (define-key keymap (kbd "<f1>") 'company-posframe-quickhelp-toggle)
    (define-key keymap (kbd "<f2>") 'company-posframe-quickhelp-scroll-up)
    (define-key keymap (kbd "<f3>") 'company-posframe-quickhelp-scroll-down)

    keymap)
  "Keymap that is enabled during an active completion in posframe.")

(defun company-posframe-enable-overriding-keymap (orig-func keymap)
  "Advice function of `company-enable-overriding-keymap'."
  (if (not (posframe-workable-p))
      (funcall orig-func keymap)
    (company-uninstall-map)
    (if (eq keymap company-active-map)
        (setq company-my-keymap company-posframe-active-map)
      (setq company-my-keymap keymap))))

(defun company-posframe-format-backend-name-active-first (_backends separator)
  "Format BACKEND for displaying in the modeline, displays active backend first"
  (let (active inactive)
    (dolist (backend company-backends)
      (if (eq backend company-backend)
          (push (propertize
                 (company-posframe-format-backend-name-active-first-helper backend)
                 'face 'company-posframe-active-backend-name)
                active)
        (push (propertize
               (company-posframe-format-backend-name-active-first-helper backend)
               'face 'company-posframe-inactive-backend-name)
              inactive)))
    (mapconcat (lambda (elem)
                 (format "%s" elem))
               (append active inactive) separator)))

(defun company-posframe-format-backend-name-active-first-helper (backend)
  "Helper function for `company-posframe-format-backend-name-active-first`"
  (cl-typecase backend
    (symbol (string-remove-prefix "company-" (symbol-name backend)))
    (list (format "[%s]"
                  (mapconcat
                   #'company-posframe-format-backend-name-active-first-helper
                   backend "|")))
    (otherwise "-")))

(defun company-posframe-format-backend-name (backends separator)
  "Format BACKEND for displaying in the modeline."
  (mapconcat #'company-posframe-format-backend-name-helper backends separator))

(defun company-posframe-format-backend-name-helper (backend)
  "Helper function for `company-posframe-format-backend-name`"
  (propertize (cl-typecase backend
                (symbol (string-remove-prefix "company-" (symbol-name backend)))
                (list (format "[%s]" (mapconcat
                                      #'company-posframe-format-backend-name-helper
                                      backend "|")))
                (otherwise "-"))
              'face (if (equal backend company-backend)
                        'company-posframe-active-backend-name
                      'company-posframe-inactive-backend-name)))

(defun company-posframe-show-at-prefix (info)
  "Poshandler showing `company-posframe' at `company-prefix'."
  (let* ((parent-window (plist-get info :parent-window))
         (point (with-current-buffer (window-buffer parent-window)
                  (- (plist-get info :position)
                     (plist-get info :company-prefix-length))))
         (posn (posn-at-point point parent-window))
         ;; TODO: Strictly speaking, if company-posframe-font is not nil, that
         ;; should be used to find the default width...
         (expected-margin-width (* (plist-get info :company-margin) (default-font-width)))
         (xy (posn-x-y posn)))
    (setcar xy (- (car xy) expected-margin-width))
    (posframe-poshandler-point-bottom-left-corner (plist-put info :position posn))))

(defun company-posframe-show ()
  "Show company-posframe candidate menu."
  (let* ((height (min company-tooltip-limit company-candidates-length))
         (meta (when company-posframe-show-metadata
                 (company-fetch-metadata)))
         (company-lines (company--create-lines company-selection height))
         (margin
          (if (numberp (car company-lines))
              (car company-lines)
            company-tooltip-margin))
         (lines
          ;; Please see: company--create-lines return value changed #52
          ;; https://github.com/tumashu/company-posframe/issues/52
          (if (numberp (car company-lines))
              (cdr company-lines)
            company-lines))
         (backend-names (when company-posframe-show-indicator
                          (funcall company-posframe-backend-format-function
                                   company-backends
                                   company-posframe-backend-separator)))
         (width (max (min (length (car lines))
                          company-tooltip-maximum-width)
                     company-tooltip-minimum-width))
         (contents (concat (mapconcat #'identity lines "\n")
                           (if meta
                               (concat "\n" (propertize (if (> (length meta) width)
                                                            (substring meta 0 width)
                                                          meta)
                                                        'face 'company-posframe-metadata))
                             "")
                           (if company-posframe-show-indicator
                               (concat "\n" (substring backend-names 0
                                                       (min width (length backend-names))))
                             "")))
         (buffer (get-buffer-create company-posframe-buffer)))
    ;; FIXME: Do not support mouse at the moment, so remove mouse-face
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (apply #'posframe-show buffer
           :string contents
           :min-height (+ (+ height (if meta 1 0))
                          (if company-posframe-show-indicator 1 0))
           :min-width (+ company-tooltip-minimum-width
                         (* 2 company-tooltip-margin))
           :max-width (+ company-tooltip-maximum-width
                         (* 2 company-tooltip-margin))
           :font company-posframe-font
           :background-color (face-attribute 'company-tooltip :background)
           :lines-truncate t
           :poshandler company-posframe-poshandler
           :poshandler-extra-info
           (list :company-margin margin
                 :company-prefix-length (length company-prefix))
           company-posframe-show-params)))

(defun company-posframe-hide ()
  "Hide company-posframe candidate menu."
  (posframe-hide company-posframe-buffer))

(defun company-posframe-frontend (orig-func command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  (if (not (posframe-workable-p))
      (funcall orig-func command)
    (setq company-posframe-last-status
          (list (selected-window)
                (current-buffer)))
    (let ((run-quickhelp-command-p
           (and (symbolp this-command)
                (string-match-p "^company-posframe-quickhelp-"
                                (symbol-name this-command)))))
      (cl-case command
        (pre-command
         (when (and company-posframe-quickhelp-delay
                    (not run-quickhelp-command-p))
           (company-posframe-quickhelp-set-timer)
           (company-posframe-quickhelp-hide)))
        (hide
         (when company-posframe-quickhelp-delay
           (company-posframe-quickhelp-cancel-timer))
         (company-posframe-quickhelp-hide)
         (company-posframe-hide))
        (post-command
         (when (not run-quickhelp-command-p)
           (company-posframe-show)))))))

(defun company-posframe-unless-just-one-frontend (orig-func command)
  "`company-posframe-frontend', but not shown for single candidates."
  (if (not (posframe-workable-p))
      (funcall orig-func command)
    (if (company--show-inline-p)
        (company-posframe-hide)
      ;; company-posframe-frontend have two args, the first
      ;; one is useless at the moment, so use 'ignore.
      (company-posframe-frontend 'ignore command))))

(defun company-posframe-window-change ()
  "Hide posframe on window change."
  (when (posframe-workable-p)
    (unless (or (member (buffer-name)
                        (list company-posframe-buffer
                              company-posframe-quickhelp-buffer))
                (equal company-posframe-last-status
                       (list (selected-window)
                             (current-buffer))))
      (company-posframe-hide)
      (company-posframe-quickhelp-hide))))

(defun company-posframe-quickhelp-skip-footers-backwards ()
  "Skip backwards over footers and blank lines."
  (beginning-of-line)
  (while (and (not (= (line-end-position) (point-min)))
              (or
               ;; [back] appears at the end of the help elisp help buffer
               (looking-at-p "\\[back\\]")
               ;; [source] cider's help buffer contains a link to source
               (looking-at-p "\\[source\\]")
               (looking-at-p "^\\s-*$")))
    (forward-line -1)))

(defun company-posframe-quickhelp-completing-read (_prompt candidates &rest _rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-posframe-quickhelp-fetch-docstring (backend)
  "Fetch docstring from BACKEND."
  (let ((quickhelp-str (company-call-backend 'quickhelp-string backend)))
    (if (stringp quickhelp-str)
        (with-temp-buffer
          (insert quickhelp-str)
          (company-posframe-quickhelp-skip-footers-backwards)
          (buffer-string))
      (let ((doc (company-call-backend 'doc-buffer backend)))
        (when doc
          (let* ((doc-buffer (if (consp doc) (car doc) doc))
                 (quickhelp-string
                  (with-current-buffer doc-buffer
                    (buffer-string))))
            (with-temp-buffer
              (insert quickhelp-string)
              (company-posframe-quickhelp-skip-footers-backwards)
              (buffer-string))))))))

(defun company-posframe-quickhelp-doc (selected)
  (when-let ((body (company-posframe-quickhelp-fetch-docstring selected)))
    (cl-letf* (((symbol-function 'completing-read)
                #'company-posframe-quickhelp-completing-read)
               (header
                (if company-posframe-quickhelp-show-header
                    (substitute-command-keys
                     (concat
                      "## "
                      "\\<company-posframe-active-map>\\[company-posframe-quickhelp-toggle]:Show/Hide  "
                      "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-up]:Scroll-Up  "
                      "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-down]:Scroll-Down "
                      "##\n"))
                  "")))
      (concat (propertize header 'face 'company-posframe-quickhelp-header)
              body))))

(defun company-posframe-quickhelp-set-timer ()
  (when (null company-posframe-quickhelp-timer)
    (setq company-posframe-quickhelp-timer
          (run-with-idle-timer company-posframe-quickhelp-delay nil
                               'company-posframe-quickhelp-show))))

(defun company-posframe-quickhelp-cancel-timer ()
  (when (timerp company-posframe-quickhelp-timer)
    (cancel-timer company-posframe-quickhelp-timer)
    (setq company-posframe-quickhelp-timer nil)))

(defun company-posframe-quickhelp-show ()
  (company-posframe-quickhelp-cancel-timer)
  (while-no-input
    (let* ((selected (nth (or company-selection 0) company-candidates))
           (doc (let ((inhibit-message t))
                  (company-posframe-quickhelp-doc selected))))
      (when doc
        (let* ((width
                (let ((n (apply #'max (mapcar #'string-width
                                              (split-string doc "\n+")))))
                  (+ (min fill-column n) 1)))
               (height
                (max (+ company-tooltip-limit
                        (if company-posframe-show-indicator 1 0)
                        (if company-posframe-show-metadata 1 0)
                        1)
                     (with-current-buffer company-posframe-buffer
                       (frame-height posframe--frame)))))
          (lower-frame
           (apply #'posframe-show
                  company-posframe-quickhelp-buffer
                  :string doc
                  :width width
                  :min-width width
                  :min-height height
                  :height height
                  :background-color (face-attribute 'company-posframe-quickhelp :background nil t)
                  :foreground-color (face-attribute 'company-posframe-quickhelp :foreground nil t)
                  company-posframe-quickhelp-show-params)))))))

(defun company-posframe-quickhelp-right-poshandler (_info)
  (with-current-buffer company-posframe-buffer
    (let ((pos posframe--last-posframe-pixel-position))
      (cons (+ (car pos)
               company-posframe-quickhelp-x-offset
               (frame-pixel-width posframe--frame))
            (cdr pos)))))

(defun company-posframe-quickhelp-hide ()
  (posframe-hide company-posframe-quickhelp-buffer))

(defun company-posframe-quickhelp-raise-frame ()
  (interactive)
  ;; FIXME: On macOS, the new lower-frame call causes Emacs to hide.
  ;; 1. https://github.com/tumashu/company-posframe/issues/43
  ;; 2. https://lists.gnu.org/archive/html/emacs-devel/2020-05/msg03253.html
  (unless (memq system-type '(darwin))
    (posframe-funcall company-posframe-quickhelp-buffer
                      #'raise-frame)))

(defun company-posframe-quickhelp-toggle ()
  (interactive)
  (if (posframe-funcall
       company-posframe-quickhelp-buffer
       (lambda ()
         (frame-parameter (window-frame) 'visibility)))
      (company-posframe-quickhelp-hide)
    (company-posframe-quickhelp-show)
    (company-posframe-quickhelp-raise-frame)))

(defun company-posframe-quickhelp-scroll-up (&optional arg)
  (interactive "^P")
  (company-posframe-quickhelp-raise-frame)
  (posframe-funcall company-posframe-quickhelp-buffer
                    #'scroll-up-command arg))

(defun company-posframe-quickhelp-scroll-down (&optional arg)
  (interactive "^P")
  (company-posframe-quickhelp-raise-frame)
  (posframe-funcall company-posframe-quickhelp-buffer
                    #'scroll-down-command arg))

;;;###autoload
(define-minor-mode company-posframe-mode
  "company-posframe minor mode."
  :global t
  :require 'company-posframe
  :group 'company-posframe
  :lighter company-posframe-lighter
  (if company-posframe-mode
      (progn
        (advice-add #'company-enable-overriding-keymap
                    :around #'company-posframe-enable-overriding-keymap)
        (advice-add #'company-pseudo-tooltip-frontend
                    :around #'company-posframe-frontend)
        (advice-add #'company-pseudo-tooltip-unless-just-one-frontend
                    :around #'company-posframe-unless-just-one-frontend)
        ;; When user switches window, child-frame should be hidden.
        (add-hook 'window-configuration-change-hook
                  #'company-posframe-window-change)
        (message company-posframe-notification))
    (posframe-delete company-posframe-buffer)
    (posframe-delete company-posframe-quickhelp-buffer)
    (advice-remove #'company-enable-overriding-keymap
                   #'company-posframe-enable-overriding-keymap)
    (advice-remove #'company-pseudo-tooltip-frontend
                   #'company-posframe-frontend)
    (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend
                   #'company-posframe-unless-just-one-frontend)
    (company-posframe-quickhelp-cancel-timer)
    (remove-hook 'window-configuration-change-hook
                 #'company-posframe-window-change)))

(provide 'company-posframe)

;;; company-posframe.el ends here
