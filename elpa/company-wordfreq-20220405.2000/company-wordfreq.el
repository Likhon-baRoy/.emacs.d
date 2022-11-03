;;; company-wordfreq.el --- Company backend for human language texts -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/company-wordfreq.el
;; Package-Version: 20220405.2000
;; Package-Commit: 83569cf346c2320ef22f6a858e3424f771c4324e
;; Version: 0.1.0
;; Keywords: company, convenience, matching
;; Package-Requires: ((emacs "27.1") (company "0.9"))

;; SPDX-License-Identifier: GPL-3.0

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 3. <https://www.gnu.org/licenses/>

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `company-wordfreq' is a company backend intended for writing texts in a human
;; language.  The completions it proposes are words already used in the current
;; (or another open) buffer and matching words from a word list file.  This
;; word list file is supposed to be a simple list of words ordered by the
;; frequency the words are used in the language.  So the first completions are
;; words already used in the buffer followed by matching words of the language
;; ordered by frequency.
;;
;; `company-wordfreq' does not come with the word list files directly, but it
;; can download the files for you for many languages from
;; <https://github.com/hermitdave/FrequencyWords>.  I made a fork of that repo
;; just in case the original changes all over sudden without my noticing.
;;
;; The directory where the word list files reside is determined by the variable
;; `company-wordfreq-path', default `~/.emacs.d/wordfreq-dicts'.  Their
;; names must follow the pattern `<language>.txt' where language is the
;; `ispell-local-dictionary' value of the current language.
;;
;; You need =grep= in your =$PATH= as =company-wordfreq= uses it to grep into
;; the word list files.  Should be the case by default on any UNIX like
;; systems.  On windows you might have to tweak it somehow.
;;
;; `company-wordfreq' is supposed to be the one and only company backend and
;; `company-mode' should not transform or sort its candidates.  This can be
;; achieved by setting the variables `company-backends' and
;; `company-transformers' buffer locally in `text-mode' buffers by
;;
;;     (add-hook 'text-mode-hook (lambda ()
;;                              (setq-local company-backends '(company-wordfreq))
;;                              (setq-local company-transformers nil)))
;;
;; Usually you don't need to configure the language picked to get the word
;; completions.  `company-wordfreq' uses the variable
;; `ispell-local-dictionary'.  It should work dynamically even if you use
;; `auto-dictionary-mode'.
;;
;;
;; To download a word list use
;;
;;     M-x company-wordfreq-download-list
;;
;; You are presented a list of languages to choose.  For some languages the
;; word lists are huge, which can lead to noticeable latency when the
;; completions are build.  Therefore you are asked if you want to use a word
;; list with only the 50k most frequent words.  The file will then be
;; downloaded, processed and put in place.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ispell)

(defun company-wordfreq--default-path ()
  "Set up the default for `company-wordfreq-path'."
  (concat (file-name-as-directory user-emacs-directory) "wordfreq-dicts"))

(defcustom company-wordfreq-path (company-wordfreq--default-path)
  "Path where the dictionary files reside.

The dictionary files are expected to have the name <language>.txt
where <language> is the contents of `ispell-local-dictionary' in
the current buffer."
  :type 'string
  :group 'company)

(defvar company-wordfreq--grep-executable nil
  "The PATH of the `grep' executable determined by backend init.")

(defun company-wordfreq--find-grep-program ()
  "Find the grep executable."
  (setq company-wordfreq--grep-executable
        (or (executable-find "grep")
            (user-error "No executable for grep found; company-wordfreq will not work"))))

(defun company-wordfreq--dictionary ()
  "Determine the path of the word list file."
  (concat (file-name-as-directory company-wordfreq-path) ispell-local-dictionary ".txt"))

(defun company-wordfreq--candidates (prefix)
  "Fetches the candidates matching PREFIX."
  (split-string
   (shell-command-to-string (concat
                             company-wordfreq--grep-executable
                             " -i "
                             (shell-quote-argument (concat "^" prefix))
                             " " (company-wordfreq--dictionary)))
   "\n"))

;;;###autoload
(defun company-wordfreq (command &optional arg &rest _ignored)
  "A company backend intended for writing texts in a human language.

The completions it proposes are words already used in the
current (or another open) buffer and matching words from a word
list file.  This word list file is supposed to be a simple list
of words ordered by the frequency the words are used in the
language.  So the first completions are words already used in the
buffer followed by matching words of the language ordered by
frequency.

See the documentation of `company-backends' for arguments COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-wordfreq))
    (init (company-wordfreq--find-grep-program))
    (prefix (when-let ((prefix (company-grab-word)))
              (substring-no-properties prefix)))
    (sorted t)
    (duplicates nil)
    (ignore-case 'keep-prefix)
    (candidates (let ((candidates (append (company-dabbrev 'candidates arg)
                                          (company-wordfreq--candidates arg)))
                      (completion-ignore-case t))
                  (all-completions arg (delete-dups candidates))))))

(defvar company-wordfreq--word-list-buffer nil
  "Pointer to the buffer a word list has been downloaded to.")

;;;###autoload
(defun company-wordfreq-download-list ()
  "Download a wordlist from FrequentWords and process it for use.

The language can be chosen from a completion list.  If the full
wordlist for the chosen language is so big, that there is a
shorter version of 50k words available, you will be prompted to
choose the short version.  Probably it is a good idea to choose
the short version as the full versions can be quite huge and
introduce latency to the completion proposals."
  (interactive)
  (let* ((language (completing-read "Choose language: " (company-wordfreq--proposal-list)))
         (lang-code (company-wordfreq--iso-code language))
         (kind-str (if (and (company-wordfreq--probe-50k lang-code)
                            (company-wordfreq--prompt-fetch-short)) "50k" "full")))
    (setq company-wordfreq--word-list-buffer
          (url-retrieve (company-wordfreq--dict-url lang-code kind-str)
                        #'company-wordfreq--list-retrieved-callback
                        `(,language)
                        :inhibit-cookies))))

(defconst company-wordfreq--frequency-word-url-prefix
  "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/")

(defun company-wordfreq--dict-url (lang-code kind)
  "Setup the file path for the language LANG-CODE.
KIND is either \"full\" or \"50k\"."
    (concat company-wordfreq--frequency-word-url-prefix
            lang-code "/"
            lang-code "_"
            kind ".txt"))

(defun company-wordfreq--probe-50k (lang-code)
  "Test if a 50k version for language LANG-CODE is available."
  (let ((url-request-method "HEAD"))
    (with-current-buffer (url-retrieve-synchronously
                          (company-wordfreq--dict-url lang-code "50k")
                          :inhibit-cookies)
      (goto-char (point-min))
      (let ((status-code
             (nth 1 (split-string (car (split-string (buffer-string) "\n")) " "))))
        (kill-current-buffer)
        (not (equal status-code "404"))))))

(defun company-wordfreq--drop-http-response-header ()
  "Delete the http response header from received word list."
  (goto-char (point-min))
  (re-search-forward "^$")
  (forward-char)
  (delete-region (point-min) (point)))

(defun company-wordfreq--drop-frequency-values ()
  "Delete the frequency value after each word in the word list."
  (goto-char (point-min))
  (while (re-search-forward "\s[0-9]+$" nil t)
    (replace-match "" nil nil)))

(defun company-wordfreq--prompt-fetch-short ()
  "Prompt if the user wants the short version of the word list."
  (y-or-n-p "Use reduced length 50k words? "))

(defun company-wordfreq--list-retrieved-callback (response language)
  "Process the downloaded word list and save it to the appropriate place.

RESPONSE the http response from `url-retrieve', LANGUAGE the
language of the word list."
  (when (eq (car response) :error)
    (user-error "Fetching the word list failed, sorry.
Either a problem with your net connection or something has changed with
the word list source.  Consider filing an issue"))
  (with-current-buffer company-wordfreq--word-list-buffer
    (company-wordfreq--drop-http-response-header)
    (company-wordfreq--drop-frequency-values)
    (unless (file-directory-p company-wordfreq-path)
      (make-directory company-wordfreq-path))
    (write-file (concat (file-name-as-directory company-wordfreq-path)
                        language ".txt"))
    (kill-current-buffer)
    (setq company-wordfreq--word-list-buffer nil)))

(defconst company-wordfreq--language-alist
  '(("afrikaans" . "af")
    ("arabic" . "ar")
    ("bulgarian" . "bg")
    ("bengali" . "bn")
    ("breton" . "br")
    ("bosnian" . "bs")
    ("catalan" . "ca")
    ("czech" . "cs")
    ("danish" . "da")
    ("german" . "de")
    ("greek" . "el")
    ("english" . "en")
    ("esperanto" . "eo")
    ("spanish" . "es")
    ("estonian" . "et")
    ("basque" . "eu")
    ("persian" . "fa")
    ("finnish" . "fi")
    ("french" . "fr")
    ("galician" . "gl")
    ("hebrew" . "he")
    ("hindi" . "hi")
    ("croatian" . "hr")
    ("hungarian" . "hu")
    ("armenian" . "hy")
    ("indonesian" . "id")
    ("icelandic" . "is")
    ("italian" . "it")
    ("japanese" . "ja")
    ("georgian" . "ka")
    ("kazakh" . "kk")
    ("korean" . "ko")
    ("lithuanian" . "lt")
    ("latvian" . "lv")
    ("macedonian" . "mk")
    ("malayalam" . "ml")
    ("malay" . "ms")
    ("dutch" . "nl")
    ("norwegian" . "no")
    ("polish" . "pl")
    ("portuguese" . "pt")
    ("brasileiro" .  "pt_br")
    ("romanian" . "ro")
    ("russian" . "ru")
    ("sinhala" . "si")
    ("slovak" . "sk")
    ("slovenian" . "sl")
    ("albanian" . "sq")
    ("serbian" . "sr")
    ("swedish" . "sv")
    ("tamil" . "ta")
    ("telugu" . "te")
    ("thai" . "th")
    ("tagalog" . "tl")
    ("turkish" . "tr")
    ("ukrainian" . "uk")
    ("urdu" . "ur")
    ("vietnamese" . "vi")))

(defun company-wordfreq--proposal-list ()
  "Get the friendly names of the languages."
  (mapcar #'car company-wordfreq--language-alist))

(defun company-wordfreq--iso-code (language)
  "Get the iso code of LANGUAGE."
  (cdr (assoc language company-wordfreq--language-alist)))


(provide 'company-wordfreq)
;;; company-wordfreq.el ends here
