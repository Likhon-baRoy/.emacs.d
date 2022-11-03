;;; flycheck-clang-tidy.el --- Flycheck syntax checker using clang-tidy -*- lexical-binding:t -*-

;; Author: Sebastian Nagel<sebastian.nagel@ncoding.at>
;; Maintainer: tastytea <tastytea@tastytea.de>
;; URL: https://github.com/ch1bo/flycheck-clang-tidy
;; Package-Commit: 3bd947fb0dcc1e97617eab7be9e1b6e57db5e091
;; Keywords: convenience languages tools
;; Package-Version: 20201115.1232
;; Package-X-Original-Version: 0.3.0
;; Package-Requires: ((flycheck "0.30"))

;; This file is NOT part of GNU Emacs.
;; See LICENSE

;;; Commentary:

;; Adds a Flycheck syntax checker for C/C++ based on clang-tidy.

;;; Usage:

;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))


;;; Code:

(require 'flycheck)
(require 'dom)

;; To keep variable names consistent.
(defvaralias 'flycheck-clang-tidy-executable 'flycheck-c/c++-clang-tidy-executable)

(flycheck-def-config-file-var flycheck-clang-tidy c/c++-clang-tidy ".clang-tidy"
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-clang-tidy-build-path "build" c/c++-clang-tidy
  "Build path to read a compile command database.

For example, it can be a CMake build directory in which a file named
compile_commands.json exists (use -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
CMake option to get this output)."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-clang-tidy-extra-options nil c/c++-clang-tidy
  "Extra options to pass to clang-tidy. Set to `nil' to disable."
  :type 'string
  :safe #'stringp)

(defun flycheck-clang-tidy-find-project-root (checker)
  "Find the project root for CHECKER using Projectile, vc or the .clang-tidy file."
  (let ((project-root nil))
    (if (member 'projectile-mode minor-mode-list)
        (setq project-root (projectile-project-root)))
    (unless project-root
      (setq project-root (vc-root-dir)))
    (unless project-root
      (let ((config_file_location (flycheck-locate-config-file flycheck-clang-tidy checker)))
        (if config_file_location
            (setq project-root (file-name-directory config_file_location)))))
    (unless project-root
      (message "Could not determine project root, trying current directory.")
      (setq project-root (flycheck-clang-tidy-current-source-dir)))
    project-root))

(defun flycheck-clang-tidy-current-source-dir ()
  "Directory of current source file."
  (file-name-directory (buffer-file-name)))

(defun flycheck-clang-tidy-get-config ()
  "Find and read .clang-tidy if `flycheck-clang-tidy' is set."
  (if flycheck-clang-tidy
      (let ((config-file (flycheck-locate-config-file flycheck-clang-tidy 0)))
        (when config-file
          (with-temp-buffer
            (insert-file-contents config-file)
            (buffer-string))))
    ""))

(defun flycheck-clang-tidy--skip-http-headers ()
  "Position point just after HTTP headers."
  (re-search-forward "^$"))

(defun flycheck-clang-tidy--narrow-to-http-body ()
  "Narrow the current buffer to contain the body of an HTTP response."
  (flycheck-clang-tidy--skip-http-headers)
  (narrow-to-region (point) (point-max)))

(defun flycheck-clang-tidy--decode-region-as-utf8 (start end)
  "Decode a region from START to END in UTF-8."
  (condition-case nil
      (decode-coding-region start end 'utf-8)
    (coding-system-error nil)))

(defun flycheck-clang-tidy--remove-crlf ()
  "Remove carriage return and line feeds from the current buffer."
  (save-excursion
    (while (re-search-forward "\r$" nil t)
      (replace-match "" t t))))

(defun flycheck-clang-tidy--extract-relevant-doc-section ()
  "Extract the parts of the LLVM clang-tidy documentation that are relevant.

This function assumes that the current buffer contains the result
of browsing 'clang.llvm.org', as returned by `url-retrieve'.
More concretely, this function returns the main <div> element
with class 'section', and also removes 'headerlinks'."
  (goto-char (point-min))
  (flycheck-clang-tidy--narrow-to-http-body)
  (flycheck-clang-tidy--decode-region-as-utf8 (point-min) (point-max))
  (flycheck-clang-tidy--remove-crlf)
  (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
         (section (dom-by-class dom "section")))
    (dolist (headerlink (dom-by-class section "headerlink"))
      (dom-remove-node section headerlink))
    section))

(defun flycheck-clang-tidy--explain-error (explanation &rest args)
  "Explain an error in the Flycheck error explanation buffer using EXPLANATION.

EXPLANATION is a function with optional ARGS that, when
evaluated, inserts the content in the appropriate Flycheck
buffer."
  (with-current-buffer flycheck-explain-error-buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (apply explanation args)
      (goto-char (point-min)))))

(defun flycheck-clang-tidy--show-documentation (error-id)
  "Show clang-tidy documentation about ERROR-ID.

Information comes from the clang.llvm.org website."
  (url-retrieve (format
                 "https://clang.llvm.org/extra/clang-tidy/checks/%s.html" error-id)
                (lambda (status)
                  (if-let ((error-status (plist-get status :error)))
                      (flycheck-clang-tidy--explain-error
                       #'insert
                       (format
                        "Error accessing clang-tidy documentation: %s"
                        (error-message-string error-status)))
                    (let ((doc-contents
                           (flycheck-clang-tidy--extract-relevant-doc-section)))
                      (flycheck-clang-tidy--explain-error
                       #'shr-insert-document doc-contents)))))
  "Loading documentation...")

(defun flycheck-clang-tidy-error-explainer (error)
  "Explain a clang-tidy ERROR by scraping documentation from llvm.org."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (if-let (err-message (flycheck-error-message error))
      (if-let (((string-match "\\[\\(.*\\)\\]" err-message))
               (clang-tidy-error-id (match-string 1 err-message)))
          (condition-case err
              (flycheck-clang-tidy--show-documentation clang-tidy-error-id)
            (error
             (format
              "Error accessing clang-tidy documentation: %s"
              (error-message-string err))))
        (error "The clang-tidy error message does not contain an [error-id]"))
    (error "Flycheck error does not contain an error message")))

(flycheck-define-checker c/c++-clang-tidy
  "A C/C++ syntax checker using clang-tidy.

See URL `https://github.com/ch1bo/flycheck-clang-tidy'."
  :command ("clang-tidy"
            (option "-p" flycheck-clang-tidy-build-path)
            (eval (concat "-extra-arg=-I" (flycheck-clang-tidy-current-source-dir)))
            (eval (concat "-config=" (flycheck-clang-tidy-get-config)))
            (eval flycheck-clang-tidy-extra-options)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error: "
          (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: "
            (message) line-end)
   (info line-start (file-name) ":" line ":" column ": note: "
         (message) line-end))
  :modes (c-mode c++-mode)
  :working-directory flycheck-clang-tidy-find-project-root
  :error-explainer flycheck-clang-tidy-error-explainer
  :predicate (lambda () (buffer-file-name))
  :next-checkers ((error . c/c++-cppcheck)))

;;;###autoload
(defun flycheck-clang-tidy-setup ()
  "Setup Flycheck clang-tidy."
  (add-to-list 'flycheck-checkers 'c/c++-clang-tidy))

(provide 'flycheck-clang-tidy)
;;; flycheck-clang-tidy.el ends here
