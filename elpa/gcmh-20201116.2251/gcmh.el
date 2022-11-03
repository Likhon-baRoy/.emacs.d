;;; gcmh.el --- the Garbage Collector Magic Hack -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>
;; Maintainer: akrl@sdf.org
;; Package: gcmh
;; Homepage: https://gitlab.com/koral/gcmh
;; Version: 0.2.1
;; Package-Version: 20201116.2251
;; Package-Commit: 0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9
;; Package-Requires: ((emacs "24"))
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
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
;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with
;; the activity.
;; During normal use a high GC threshold is set.
;; When idling GC is triggered and a low threshold is set.
;; A more detailed explanation of the rationale behind this can be found at
;; http://akrl.sdf.org/

;;; Code:

(defgroup gcmh nil
  "Garbage Collector Magic Hack."
  :group 'alloc)

(defcustom gcmh-low-cons-threshold 800000
  "Low cons GC threshold.
This is the GC threshold used while idling. Default value is the
same of `gc-cons-threshold' default."
  :type 'number)

(defcustom gcmh-high-cons-threshold #x40000000
  "High cons GC threshold.
This should be set to a value that makes GC unlikely but does not
cause OS paging."
  :type 'number)

(defcustom gcmh-idle-delay 15
  "Idle time to wait in seconds before triggering GC.
If `auto' this is auto computed based on `gcmh-auto-idle-delay-factor'."
  :type '(choice number (const auto)))

(defcustom gcmh-auto-idle-delay-factor 20
  "Factor to compute the idle delay when in idle-delay auto mode.
The idle delay will be `gcmh-auto-idle-delay-factor' times the
time the last non idle garbage collection time."
  :type 'number)

(defcustom gcmh-verbose nil
  "If t, print a message when garbage collecting."
  :type 'boolean)

(defvar gcmh-idle-timer nil
  "Idle timer for triggering GC.")

(defmacro gcmh-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun gcmh-set-high-threshold ()
  "Set the high GC threshold.
This is to be used with the `pre-command-hook'."
  (setf gc-cons-threshold gcmh-high-cons-threshold))

(defvar gcmh-last-gc-time 0.1
  "How long it took to perform the last garbage collection.")

(defun gcmh-register-idle-gc ()
  "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
  (let ((idle-t (if (eq gcmh-idle-delay 'auto)
		    (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
		  gcmh-idle-delay)))
    (when (timerp gcmh-idle-timer)
      (cancel-timer gcmh-idle-timer))
    (setf gcmh-idle-timer
	  (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))

(defun gcmh-idle-garbage-collect ()
  "Run garbage collection after `gcmh-idle-delay'."
  (if gcmh-verbose
      (progn
	(message "Garbage collecting...")
	(condition-case-unless-debug e
	    (message "Garbage collecting...done (%.3fs)"
		     (setf gcmh-last-gc-time (gcmh-time (garbage-collect))))
	  (error (message "Garbage collecting...failed")
		 (signal (car e) (cdr e)))))
    (setf gcmh-last-gc-time (gcmh-time (garbage-collect))))
  (setf gc-cons-threshold gcmh-low-cons-threshold))

;;;###autoload
(define-minor-mode gcmh-mode
  "Minor mode to tweak Garbage Collection strategy."
  :lighter " GCMH"
  :global t
  (if gcmh-mode
      (progn
        (setf gc-cons-threshold gcmh-high-cons-threshold)
	;; Release severe GC strategy before the user restart to working
	(add-hook 'pre-command-hook #'gcmh-set-high-threshold)
	(add-hook 'post-command-hook #'gcmh-register-idle-gc))
    (setf gc-cons-threshold gcmh-low-cons-threshold
          gcmh-idle-timer nil)
    (remove-hook 'pre-command-hook #'gcmh-set-high-threshold)
    (remove-hook 'post-command-hook #'gcmh-register-idle-gc)))

(provide 'gcmh)

;;; gcmh.el ends here
