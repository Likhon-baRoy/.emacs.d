;;; prescient-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prescient" "prescient.el" (0 0 0 0))
;;; Generated autoloads from prescient.el

(autoload 'prescient-filter "prescient" "\
Use QUERY to filter list of CANDIDATES.

CANDIDATES is a completion table, such as a list of strings
or a function as defined in the Info node
`(elisp)Programmed Completion'.

QUERY is a string containing the sub-queries, which are gotten
using `prescient-split-query'. Each sub-query is used to produce
a regular expression according to the filter methods listed in
`prescient-filter-method'. A candidate must match every regular
expression made from the sub-queries to be included in the list
of returned candidates.

PRED is the predicate used with the completion table, as
described in the above Info node.

This function does not modify CANDIDATES; it always make a new
copy of the list.

\(fn QUERY CANDIDATES &optional PRED)" nil nil)

(autoload 'prescient-completion-sort "prescient" "\
Sort the filtered CANDIDATES.

This function will always sort candidates using the function
`prescient-sort'. When CANDIDATES has been filtered using the
`prescient' completion style, it can optionally also sort them
using the function `prescient-sort-full-matches-first'.

This function checks for the properties `prescient-regexps' and
`prescient-ignore-case' on the first candidate in
CANDIDATES (though they are stored on all candidates filtered by
`prescient-filter'). These properties are used for implementing
the user option `prescient-sort-full-matches-first'.

\(fn CANDIDATES)" nil nil)

(autoload 'prescient-all-completions "prescient" "\
`all-completions' using prescient.el.

STRING is the input. TABLE is a completion table. PRED is a
predicate that further restricts the matching candidates. POINT
would be the current point, but it is not used by this function.
See the function `all-completions' for more information.

This function returns a list of completions whose final `cdr' is
the length of the prefix string used for completion (which might
be all or just part of STRING).

\(fn STRING TABLE &optional PRED POINT)" nil nil)

(autoload 'prescient-try-completion "prescient" "\
`try-completion' using Prescient.

STRING is the input.  TABLE is a completion table.  PRED is a
predicate.  POINT is the current point.  See the function
`try-completion' for more information.

If there are no matches, this function returns nil. If the only
match equals STRING, this function returns t. Otherwise, this
function returns a cons cell of the completed string and its
length. If there is more than one match, that completed string is
actually just the input, in which case nothing happens.

\(fn STRING TABLE &optional PRED POINT)" nil nil)

(add-to-list 'completion-styles-alist '(prescient prescient-try-completion prescient-all-completions "Filtering using prescient.el.\nFor sorting, see the function `prescient-completion-sort'."))

(autoload 'prescient-create-and-bind-toggle-command "prescient" "\
Create and bind a command to toggle the use of a filter method.

The created command toggles the FILTER-TYPE method on
or off buffer locally, and doesn't affect the default
behavior (determined by `prescient-filter-method').

The created command is bound to KBD-STRING in
`prescient-toggle-map'. This map is itself bound to `M-s'
in the completion buffer when `selectrum-prescient-mode' or
`vertico-prescient-mode' are enabled.

FILTER-TYPE is an unquoted symbol that can be used in
`prescient-filter-method'. KBD-STRING is a string that can be
passed to `kbd'.

\(fn FILTER-TYPE KBD-STRING)" nil t)

(register-definition-prefixes "prescient" '("prescient-" "selectrum-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prescient-autoloads.el ends here
