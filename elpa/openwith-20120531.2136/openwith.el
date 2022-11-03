;;; openwith.el --- Open files with external programs

;; Copyright (C) 2007  Markus Triska

;; Author: Markus Triska <markus.triska@gmx.at>
;; Keywords: files, processes
;; Package-Version: 20120531.2136
;; Package-Commit: dd1f0e2a527535086c2b0ae12031dbf3ab7c5fd7
;; URL: https://bitbucket.org/jpkotta/openwith
;; Version: 20120531

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This lets you associate external applications with files so that
;; you can open them via C-x C-f, with RET in dired, etc.

;; Copy openwith.el to your load-path and add to your .emacs:

;;    (require 'openwith)
;;    (openwith-mode t)

;; To customize associations etc., use:

;;    M-x customize-group RET openwith RET

;;; Code:

(defgroup openwith nil
  "Associate external applications with file name patterns."
  :group 'files
  :group 'processes)

(defcustom openwith-associations
  '(("\\.pdf\\'" "acroread" (file))
    ("\\.mp3\\'" "xmms" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))
  "Associations of file patterns to external programs.
File pattern is a regular expression describing the files to
associate with a program. The program arguments are a list of
strings and symbols and are passed to the program on invocation,
where the symbol 'file' is replaced by the file to be opened."
  :group 'openwith
  :type '(repeat (list (regexp :tag "Files")
                       (string :tag "Program")
                       (sexp :tag "Parameters"))))

(defcustom openwith-confirm-invocation nil
  "Ask for confirmation before invoking external programs."
  :group 'openwith
  :type 'boolean)

(defun openwith-make-extension-regexp (strings)
  "Make a regexp that matches a string that starts with a '.',
has any of the supplied STRINGS, and is at the end of the
string."
  (concat "\\." (regexp-opt strings) "$"))

(defun openwith-open-unix (command arglist)
  "Run external command COMMAND, in such a way that it is
  disowned from the parent Emacs process.  If Emacs dies, the
  process spawned here lives on.  ARGLIST is a list of strings,
  each an argument to COMMAND."
  (let ((shell-file-name "/bin/sh"))
    (start-process-shell-command
     "openwith-process" nil
     (concat
      "exec nohup " command " " 
      (mapconcat 'shell-quote-argument arglist " ")
      " >/dev/null"))
     ))

(defun openwith-open-windows (file)
  "Run external command COMMAND, in such a way that it is
  disowned from the parent Emacs process.  If Emacs dies, the
  process spawned here lives on.  ARGLIST is a list of strings,
  each an argument to COMMAND."
  (w32-shell-execute "open" file))

(defun openwith-file-handler (operation &rest args)
  "Open file with external program, if an association is configured."
  (when (and openwith-mode (not (buffer-modified-p)) (zerop (buffer-size)))
    (let ((assocs openwith-associations)
          (file (car args))
          oa)
      ;; do not use `dolist' here, since some packages (like cl)
      ;; temporarily unbind it
      (while assocs
        (setq oa (car assocs)
              assocs (cdr assocs))
        (when (save-match-data (string-match (car oa) file))
          (let ((params (mapcar (lambda (x) (if (eq x 'file) file x))
                                (nth 2 oa))))
            (when (or (not openwith-confirm-invocation)
                      (y-or-n-p (format "%s %s? " (cadr oa)
                                        (mapconcat #'identity params " "))))
	      (if (eq system-type 'windows-nt)
		  (openwith-open-windows file)
		(openwith-open-unix (cadr oa) params))
              (kill-buffer nil)
              (when (featurep 'recentf)
                (recentf-add-file file))
              ;; inhibit further actions
              (error "Opened %s in external program"
                     (file-name-nondirectory file))))))))
  ;; when no association was found, relay the operation to other handlers
  (let ((inhibit-file-name-handlers
         (cons 'openwith-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;;;###autoload
(define-minor-mode openwith-mode
  "Automatically open files with external programs."
  :lighter ""
  :global t
  (if openwith-mode
      (progn
        ;; register `openwith-file-handler' for all files
        (put 'openwith-file-handler 'safe-magic t)
        (put 'openwith-file-handler 'operations '(insert-file-contents))
        (add-to-list 'file-name-handler-alist '("" . openwith-file-handler)))
    (setq file-name-handler-alist
          (delete '("" . openwith-file-handler) file-name-handler-alist))))

(provide 'openwith)

;;; openwith.el ends here
