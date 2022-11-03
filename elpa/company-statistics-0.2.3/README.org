* company-statistics
** About
Company-statistics is a global minor mode built on top of the in-buffer
completion system [[http://company-mode.github.io/][company-mode]].  The idea is to keep a log of a certain number
of completions you choose, along with some context information, and use that to
rank candidates the next time you have to choose --- hopefully showing you
likelier candidates at the top of the list.
** Use It
Using the package is simple.

If you install it from the elpa.gnu.org repository with Emacs' package manager,
you only need to enable the mode, e.g., in your =init.el= file:
#+begin_src emacs-lisp
(add-hook 'after-init-hook 'company-statistics-mode)
#+end_src

Alternatively, make sure =company-statistics.el= is in your =load-path=, and add
to your =init.el= file
#+begin_src emacs-lisp
(require 'company-statistics)
(company-statistics-mode)
#+end_src
to load the package manually and turn on the mode.

See the (few but powerful) customizable options for details =M-x customize-group
company-statistics=.
** Design
Company-statistics is an add-on for company-mode, but is only loosely coupled to
it (it works by adding a sorting function to =company-transformers= as well as a
handler to =company-completion-finished-hook=).  It is designed with some
flexibility in mind as for the recorded context information and the way
candidates are scored: the default pair of functions are only examples!  The
stats are automatically persistent between sessions.
** Have Fun!
