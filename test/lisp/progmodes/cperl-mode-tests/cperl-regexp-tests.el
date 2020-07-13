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
;; (ert-run-tests-interactively)

(require 'cperl-mode)
(ert-deftest cperl-test-basic-identifier ()
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


  
  "Verify that '/' is a division after ++ or --, not a regexp.
Reported in https://github.com/jrockway/cperl-mode/issues/45.
If seen as regular expression, then the slash is displayed using
font-lock-constant-face.  If seen as a division, then it doesn't
have a face property."
  :tags '(:fontification)
  ;; The next two Perl expressions have divisions.  Perl "punctuation"
  ;; operators don't get a face.  The comment at the end of line
  ;; prevents cperl-mode from tripping over "End of ‘/ ... /’
  ;; string/RE not found" if it gets it wrong
  (let ((code "{ $a++ / $b } # /"))
    (should (equal (cperl-test-face code "/" ) nil)))
  (let ((code "{ $a-- / $b } # /"))
    (should (equal (cperl-test-face code "/" ) nil)))
  ;; The next two Perl expressions have regular expressions.  The
  ;; delimiter of a RE is fontified with font-lock-constant-face.
  (let ((code "{ $a+ / $b } # /"))
    (should (equal (cperl-test-face code "/" ) font-lock-constant-face)))
  (let ((code "{ $a- / $b } # /"))
    (should (equal (cperl-test-face code "/" ) font-lock-constant-face))))

(defun cperl-test--text-face (text face)
  "Find the next occurrence of TEXT and test against FACE.
Search forward in the current buffer, so several of these tests
can be run in a row.  Check the first character of the match
whether its face property matches FACE."
  (search-forward text)
  (should (equal (get-text-property (match-beginning 0) 'face) face)))


(ert-deftest cperl-here-docs ()
  "Verify that unindented here-docs are closed properly."
  (let ((file (expand-file-name "cperl-here-docs.pl"
                                 cperl-mode-tests-data-directory))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-fontify-buffer)
      (indent-region (point-min) (point-max))
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "test"   font-lock-string-face)
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "return" font-lock-keyword-face))))



(ert-deftest cperl-here-docs-indented ()
  "Verify that indented here-docs are closed properly.
As of Perl 5.26, here-docs can be indented with their code."
  (let ((file (expand-file-name "cperl-here-docs-indented.pl"
                                 cperl-mode-tests-data-directory))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-fontify-buffer)
      (indent-region (point-min) (point-max))
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "test"   font-lock-string-face)
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "return" font-lock-keyword-face))))


(ert-deftest cperl-here-docs-edge ()
  "Verify that an edge case here-doc is closed properly.
In this case, the here-doc contains the terminator string, but
not starting at column 1: This is still part of the here-doc."
  (let ((file (expand-file-name "cperl-here-docs-edge.pl"
                                 cperl-mode-tests-data-directory))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-fontify-buffer)
      (indent-region (point-min) (point-max))
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "test"   font-lock-string-face)
      ;; For a non-indented keyword, the terminator _must_ start
      ;; in column one, so this occurrence is _not_ a terminator.
      (cperl-test--text-face "    HERE"   font-lock-string-face)
      (cperl-test--text-face "HERE"   font-lock-constant-face)
      (cperl-test--text-face "return" font-lock-keyword-face))))


(ert-deftest cperl-test-moosex-declare ()
  "Verify that a module using MooseX::Declare looks halfway decent.
Full compatibility with jrockway's branch is not attempted."
  (let ((file (expand-file-name "cperl-moosex-declare.pm"
                                cperl-mode-tests-data-directory))
        (cperl-automatic-keyword-sets t)
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-fontify-buffer)
      (cperl-test--text-face "use"             font-lock-keyword-face)
      (cperl-test--text-face "MooseX::Declare" font-lock-function-name-face)
      (cperl-test--text-face "class"           font-lock-keyword-face)
      (cperl-test--text-face "BankAccount"     font-lock-function-name-face)
      (cperl-test--text-face "has"             font-lock-type-face)
      (cperl-test--text-face "method"          font-lock-keyword-face)
      (cperl-test--text-face "deposit"         font-lock-function-name-face)
      (cperl-test--text-face "my"              font-lock-keyword-face)
      (cperl-test--text-face "$current_balanc" font-lock-variable-name-face)
      (cperl-test--text-face "confess"         font-lock-type-face)
      (cperl-test--text-face "CheckingAccount" font-lock-function-name-face)
      (cperl-test--text-face "extends"         font-lock-keyword-face)
      (cperl-test--text-face "BankAccount"     font-lock-function-name-face)
      (cperl-test--text-face "before"          font-lock-keyword-face)
      (cperl-test--text-face "withdraw"        font-lock-function-name-face)
      "done.")))

(ert-deftest cperl-test-moosex-declare-noauto ()
  "The same test as before, but disable keyword recognition.
This behaves as older versions of cperl-mode, and should still
recognize variables and core keywords."
  (let ((file (expand-file-name "cperl-moosex-declare.pm"
                                cperl-mode-tests-data-directory))
        (cperl-automatic-keyword-sets nil) ;; Don't apply keyword sets
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-fontify-buffer)
      (cperl-test--text-face "use"             font-lock-keyword-face)
      (cperl-test--text-face "MooseX::Declare" font-lock-function-name-face)
      (cperl-test--text-face "class"           nil)
      (cperl-test--text-face "BankAccount"     nil)
      (cperl-test--text-face "has"             nil)
      (cperl-test--text-face "method"          nil)
      (cperl-test--text-face "deposit"         nil)
      (cperl-test--text-face "my"              font-lock-keyword-face)
      (cperl-test--text-face "$current_balanc" font-lock-variable-name-face)
      (cperl-test--text-face "confess"         nil)
      (cperl-test--text-face "CheckingAccount" nil)
      (cperl-test--text-face "extends"         nil)
      (cperl-test--text-face "BankAccount"     nil)
      (cperl-test--text-face "before"          nil)
      (cperl-test--text-face "withdraw"        nil)
      "done.")))
