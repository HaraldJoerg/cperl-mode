;;; cperl-regexp-tests.el --- Test basic regular expressions in cperl-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020-2020 ...to be decided ...

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords:       internal
;; Human-Keywords: internal
;; Homepage: https://github.com/HaraldJoerg/cperl-mode

;;; Commentary:

;; This tests might soon be obsolete, but help verifying that the
;; constants introduced during refactoring in 2020 actually match the
;; same stuff as the original literals.

;; Run these tests interactively:
;; (ert-run-tests-interactively t)

(require 'cperl-mode)
(ert-deftest cperl-test-basic-identifier-regexp ()
  "Verify that basic identifiers are found.
This is sort of a unit test for some of the regular expressions
in cperl-mode."
  (should (equal (string-match cperl--basic-identifier-regexp
                               "3 + $foo+$bar")
                 5))    ;; plain scalar
  (should (equal (match-end 0) 8))  ;; "+" ends the identifier
  
  (should (equal (string-match cperl--basic-identifier-regexp
                               "3 + $foo{ 'bar' }")
                 5))    ;; braces also end the identifier
  (should (equal (match-end 0) 8))

  (should (equal (string-match cperl--basic-identifier-regexp
                               "3 + $Foo::bar+1")
                 5))    ;; an identifier with package spec
  (should (equal (match-end 0) 8))

  (should (equal (string-match cperl--identifier-regexp
                               "3 + $Foo::bar*3")
                 5))    ;; an identifier with package spec
  (should (equal (match-end 0) 13))

  (should (equal (string-match cperl--identifier-regexp
                               "3 + $___::___*3")
                 5))    ;; yes, underscore is fine
  (should (equal (match-end 0) 13))

  (should (equal (string-match cperl--version-regexp
                               " v1.2.3 ")
                 1))    ;; "structured" version

  (should (equal (string-match (concat "^\\("
                                       cperl--version-regexp
                                       "\\)$")
                               " v1.2 ")
                 nil))  ;; no match, needs at least three digits

  (should (equal (string-match cperl--version-regexp
                               " 42 ")
                 1))

  (should (equal (string-match cperl--version-regexp
                               " 3.1415 ")
                 1))

  (should (equal (string-match cperl--version-regexp
                               " 3. ")
                 1))

  (should (equal (string-match cperl--version-regexp
                               " .1234 ")
                 1))
  )


(defun cperl-test-wrap-podlink-process (text expected)
  "Process TEXT with `cperl--pod-process-links', check against EXPECTED.
This is a helper for `cperl-test-podlink-process' to reduce
boilerplate.  In case of unexpected results, the contents of the
buffer and the expected result are printed to the message buffer,
making it easy to identify which of the individual tests failed.
Also makes sure that a link immediately following TEXT is
processed (no runaway regex)."
  (with-temp-buffer
    (insert "prologue\n")
    (insert text)
    (insert "L<end>")
    (cperl--pod-process-links)
    (goto-char (point-min))
    (let ((found (search-forward expected nil t)))
      (or found
          (message "After processing:\n%s\nExpected\n%s" (buffer-string) expected))
      (should found)
      (should (search-forward "L<end|perldoc://end>" nil t))))) ; no gap

(ert-deftest cperl-test-podlink-process ()
  "Verify that POD L<...> constructs are properly processed."
  ;; In Encode::Supported: Old-style section
  (cperl-test-wrap-podlink-process
   "L<Microsoft-related naming mess>"
   "L<Microsoft-related naming mess|/\"Microsoft-related naming mess\">")
  ;; In File::Temp: Quoted old-style section"
  (cperl-test-wrap-podlink-process
   "L<\"safe_level\">"
   "L<safe_level|/\"safe_level\">")
  ;; In perlfunc: page with quoted section according to spec
  (cperl-test-wrap-podlink-process
   "L<perlsyn/\"Compound Statements\">"
   "L<\"Compound Statements\" in perlsyn|perldoc://perlsyn/Compound-Statements>")
  ;; In File::Temp: Quoted old-style section with unquoted name"
  (cperl-test-wrap-podlink-process
   "L<unlink1|\"unlink1\">"
   "L<unlink1|/\"unlink1\">")
  ;; In perldiag: Unbalanced < in text"
  (cperl-test-wrap-podlink-process
   "L<(?<=pattern) and \\K in perlre|perlre/\\K>"
   "L<(?<=pattern) and \\K in perlre|perldoc://perlre/\\K>")
  ;; In perldiag: Messy quotes in section
  (cperl-test-wrap-podlink-process
   "L</\"\"\\c%c\" is more clearly written simply as \"%s\"\">"
   "L</\"\"\\c%c\" is more clearly written simply as \"%s\"\">") ; untouched!
  ;; In perltie: Old-style section with markup" .
  (cperl-test-wrap-podlink-process
   "L<The C<untie> Gotcha>"
   "L<The C<untie> Gotcha|/\"The C<untie> Gotcha\">")
  ;; In perlfunc: markup in text and section
  (cperl-test-wrap-podlink-process
   "L<C<qE<sol>E<sol>>|/qE<sol>STRINGE<sol>>"
   "L<C<qE<sol>E<sol>>|/qE<sol>STRINGE<sol>>") ; untouched!
;; In perlfunc: Markup immediately followed by markup
  (cperl-test-wrap-podlink-process
   "L<S<C<\"use feature 'unicode_strings\">>|feature/The 'unicode_strings' feature>"
   "L<S<C<\"use feature 'unicode_strings\">>|perldoc://feature/The-'unicode_strings'-feature>")
  ;; In perlsyn: Doubly braced markup within a link - and NL instead of a space
  (cperl-test-wrap-podlink-process
  "L<C<< <FILEHANDLE>
>>|perlop/\"I/O Operators\">"
  "L<C<< <FILEHANDLE>
>>|perldoc://perlop/I/O-Operators>")
  ;; In perlapi - an external manpage.  pod2html handles markup!
  (cperl-test-wrap-podlink-process
   "L<C<setlocale(3)>>"
   "L<C<setlocale(3)>|perldoc://C<setlocale(3)>>")
  ;; extended delimiter, allowing for messy and unbalanced content
  (cperl-test-wrap-podlink-process
   "L<< stuff containing <|/and > anywhere >>"
   "L<< stuff containing <|/and > anywhere >>") ; untouched!
  ;; The following tests are taken from perlpodspec
  (cperl-test-wrap-podlink-process
   "L<Foo::Bar>"
   "L<Foo::Bar|perldoc://Foo::Bar>")
  (cperl-test-wrap-podlink-process
   "L<Perlport's section on NL's|perlport/Newlines>"
   "L<Perlport's section on NL's|perldoc://perlport/Newlines>")
  (cperl-test-wrap-podlink-process
   "L<perlport/Newlines>"
   "L<Newlines in perlport|perldoc://perlport/Newlines>")
  (cperl-test-wrap-podlink-process
   "L<crontab(5)/\"DESCRIPTION\">"
   "L<\"DESCRIPTION\" in crontab(5)|perldoc://crontab(5)/DESCRIPTION")
  (cperl-test-wrap-podlink-process
   "L</Object Attributes>"
   "L</Object Attributes>") ; untouched!
  (cperl-test-wrap-podlink-process
   "L<https://www.perl.org/>"
   "L<https://www.perl.org/>") ; untouched!
  (cperl-test-wrap-podlink-process
   "L<Perl.org|https://www.perl.org/>"
   "L<Perl.org|https://www.perl.org/>") ; untouched!
  )
