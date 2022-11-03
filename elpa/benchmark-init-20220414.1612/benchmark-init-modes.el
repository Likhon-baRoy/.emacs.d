;;; benchmark-init-modes.el --- Modes for presenting benchmark results.

;; Copyright (C) 2014 David Holm

;; Author: David Holm <dholmster@gmail.com>
;; Created: 05 Apr 2014

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Installation:

;; See `benchmark-init.el'.

;;; Usage:

;; Results can be presented either in a tabulated list mode, or as a tree of
;; accumulated durations:
;;
;;  - `benchmark-init/show-durations-tabulated'
;;  - `benchmark-init/show-durations-tree'

;;; Code:

(eval-when-compile
  (require 'benchmark-init))

;; Faces

(defgroup benchmark-init/faces nil
  "Faces used by benchmark-init."
  :group 'benchmark-init
  :group 'faces)

(defface benchmark-init/header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for benchmark init header."
  :group 'benchmark-init/faces)

(defface benchmark-init/name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for entry name."
  :group 'benchmark-init/faces)

(defface benchmark-init/type-face
  '((t :inherit font-lock-type-face))
  "Face for entry type."
  :group 'benchmark-init/faces)

(defface benchmark-init/duration-face
  '((t :inherit font-lock-constant-face))
  "Face for entry duration."
  :group 'benchmark-init/faces)

;; Constants

(defconst benchmark-init/buffer-name "*Benchmark Init Results %s*"
  "Name of benchmark-init list buffer.")

(defconst benchmark-init/list-format
  [("Module" 65 t)
   ("Type" 7 t)
   ("ms" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                            (string-to-number (aref (cadr b) 2))))
    :right-align t)
   ("total ms" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 3))
                                  (string-to-number (aref (cadr b) 3))))
    :right-align t)]
  "Benchmark list format.")

(defconst benchmark-init/list-sort-key
  '("ms" . t)
  "Benchmark list sort key.")

;; Global variables

(defvar benchmark-init/tree-mode-hook nil
  "Hook run when entering the tree presentation mode.")

(defvar benchmark-init/tree-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Local keymap for `benchmark-init/tree-mode' buffers.")

;; Tabulated presentation mode

(define-derived-mode benchmark-init/tabulated-mode tabulated-list-mode
  "Benchmark Init Tabulated"
  "Mode for displaying benchmark-init results in a table."
  (setq tabulated-list-format benchmark-init/list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key benchmark-init/list-sort-key)
  (tabulated-list-init-header))

(defun benchmark-init/list-entries ()
  "Generate benchmark-init list entries from durations tree."
  (let (entries)
    (mapc
     (lambda (value)
       (let ((name (cdr (assq :name value)))
             (type (symbol-name (cdr (assq :type value))))
             (duration (round (cdr (assq :duration value))))
             (duration-adj (round (cdr (assq :duration-adj value)))))
         (push (list name `[,name ,type ,(number-to-string duration-adj)
                                  ,(number-to-string duration)]) entries)))
     (cdr (benchmark-init/flatten benchmark-init/durations-tree)))
    entries))

;;;###autoload
(defun benchmark-init/show-durations-tabulated ()
  "Show the benchmark results in a sorted table."
  (interactive)
  (unless (featurep 'tabulated-list)
    (require 'tabulated-list))
  (let ((buffer-name (format benchmark-init/buffer-name "Tabulated")))
    (with-current-buffer (get-buffer-create buffer-name)
      (benchmark-init/tabulated-mode)
      (setq tabulated-list-entries 'benchmark-init/list-entries)
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

;; Tree presentation

(defun benchmark-init/print-header ()
  "Print the presentation header."
  (insert
   (propertize "Benchmark results" 'face 'benchmark-init/header-face)
   "\n\n"))

(defun benchmark-init/print-node (padding node)
  "Print PADDING followed by NODE."
  (let ((name (benchmark-init/node-name node))
        (type (symbol-name (benchmark-init/node-type node)))
        (duration (benchmark-init/node-duration-adjusted node)))
    (insert padding "["
            (propertize (format "%s" name)
                        'face 'benchmark-init/name-face)
            " " (propertize (format "%s" type)
                            'face 'benchmark-init/type-face)
            " " (propertize (format "%dms" (round duration))
                            'face 'benchmark-init/duration-face)
            "]\n")))

(defun benchmark-init/print-nodes (nodes padding)
  "Print NODES after PADDING."
  (let ((x (car nodes))
        (xs (cdr nodes)))
    (let ((children (benchmark-init/node-children x))
          (cur-padding (concat padding (if xs "├─" "╰─")))
          (sub-padding (concat padding (if xs "│ " "  "))))
      (if (benchmark-init/node-root-p x)
          (benchmark-init/print-node "╼►" x)
        (benchmark-init/print-node cur-padding x))
      (when children
        (benchmark-init/print-nodes children sub-padding))
      (when xs
        (benchmark-init/print-nodes xs padding)))))

(defun benchmark-init/tree-buffer-setup ()
  "Configure the buffer for the durations tree."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (benchmark-init/print-header)
    (benchmark-init/print-nodes (list benchmark-init/durations-tree) ""))
  (use-local-map benchmark-init/tree-mode-map)
  (goto-char (point-min)))

(defun benchmark-init/tree-mode ()
  "Major mode for presenting durations in a tree."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map benchmark-init/tree-mode-map)
  (setq major-mode 'benchmark-init/tree-mode)
  (benchmark-init/tree-buffer-setup)
  (run-mode-hooks 'benchmark-init/tree-mode-hook))

(put 'benchmark-init/tree-mode 'mode-class 'special)

;;;###autoload
(defun benchmark-init/show-durations-tree ()
  "Show durations in call-tree."
  (interactive)
  (let ((buffer-name (format benchmark-init/buffer-name "Tree")))
    (switch-to-buffer (get-buffer-create buffer-name))
    (if (not (eq major-mode 'benchmark-init/tree-mode))
        (benchmark-init/tree-mode))))

;; Obsolete functions

(define-obsolete-function-alias 'benchmark-init/show-durations
  'benchmark-init/show-durations-tabulated "2014-04-05")

(provide 'benchmark-init-modes)
;;; benchmark-init-modes.el ends here
