;;; company-wordfreq-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-wordfreq" "company-wordfreq.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-wordfreq.el

(autoload 'company-wordfreq "company-wordfreq" "\
A company backend intended for writing texts in a human language.

The completions it proposes are words already used in the
current (or another open) buffer and matching words from a word
list file.  This word list file is supposed to be a simple list
of words ordered by the frequency the words are used in the
language.  So the first completions are words already used in the
buffer followed by matching words of the language ordered by
frequency.

See the documentation of `company-backends' for arguments COMMAND and ARG.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-wordfreq-download-list "company-wordfreq" "\
Download a wordlist from FrequentWords and process it for use.

The language can be chosen from a completion list.  If the full
wordlist for the chosen language is so big, that there is a
shorter version of 50k words available, you will be prompted to
choose the short version.  Probably it is a good idea to choose
the short version as the full versions can be quite huge and
introduce latency to the completion proposals." t nil)

(register-definition-prefixes "company-wordfreq" '("company-wordfreq-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-wordfreq-autoloads.el ends here
