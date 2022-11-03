;;; frame-local.el --- Variables local to a frame  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/frame-local
;; Package-Version: 20180330.940
;; Package-Commit: 51c0889602626e2dcc6f1c1a812b058bc96df03c
;; Keywords: frames, tools, local, lisp
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.0.1

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Access variables local to a frame
;;
;; You can use the functions frame-local-[set,get,setq,getq]
;; With frame-local-[setq,getq], the variables don't need to be quoted
;;
;; It is recommended to prefix the variables with your package name
;; to not create conflicts with variables from other packages.
;;
;; Examples:
;; (frame-local-set 'my-variable 1)
;; (frame-local-get 'my-variable)
;; (frame-local-setq my-other-variable 2 some-frame)
;; (frame-local-getq my-other-variable some-frame)
;;
;; Note that the variables created with this package don't have any
;; relation with variables defined by `defvar', `defconst' etc.
;;
;;; Code:

(defvar frame-local--obarrays nil
  "Plist of obarrays for each frame.")

(defvar frame-local--cache nil
  "Cons of frame and obarray currently used.
This avoid to search the obarray in `frame-local--obarrays' on every request.")

(defun frame-local--get-obarray-1 (frame &optional create)
  (if (plist-member frame-local--obarrays frame)
      (plist-get frame-local--obarrays frame)
    (when create
      (let ((obarray (make-vector 32 0)))
        (setq frame-local--obarrays
              (plist-put frame-local--obarrays frame obarray))
        obarray))))

(defun frame-local--get-obarray (frame &optional create)
  "Return the obarray associated to FRAME.
If there is no obarray and CREATE is non-nil, a new obarray is created."
  (setq frame (window-normalize-frame frame))
  (if (eq frame (car frame-local--cache))
      (cdr frame-local--cache)
    (let ((obarray (frame-local--get-obarray-1 frame create)))
      (when obarray
        (setq frame-local--cache (cons frame obarray)))
      obarray)))

(defun frame-local-set (name value &optional frame)
  "Set the symbol NAME's value to VALUE in FRAME.
If FRAME is nil, set the symbol in the current frame.
Return VALUE."
  (let* ((obarray (frame-local--get-obarray frame t))
         (sym (intern (symbol-name name) obarray)))
    (set sym value)))

(defun frame-local-get (name &optional frame)
  "Return symbol NAME's value in FRAME.
Or in the current frame if FRAME is nil."
  (let* ((obarray (frame-local--get-obarray frame))
         (sym (and obarray (intern-soft (symbol-name name) obarray))))
    (when sym
      (symbol-value sym))))

(defmacro frame-local-setq (name value &optional frame)
  "Similar to `frame-local-set' but NAME must not be quoted.
See `frame-local-set' for the parameters VALUE and FRAME."
  `(frame-local-set ',name ,value ,frame))

(defmacro frame-local-getq (name &optional frame)
  "Similar to `frame-local-get' but NAME must not be quoted.
See `frame-local-get' for the parameter FRAME."
  `(frame-local-get ',name ,frame))

(defun frame-local--on-delete (frame)
  "Delete the obarray associated to FRAME, if any."
  (when (and (framep frame)
             (plist-member frame-local--obarrays frame))
    (let ((obarray (plist-get frame-local--obarrays frame)))
      (setq frame-local--obarrays
            (delq frame (delq obarray frame-local--obarrays))))))

(add-hook 'delete-frame-functions 'frame-local--on-delete t)

(provide 'frame-local)
;;; frame-local.el ends here
