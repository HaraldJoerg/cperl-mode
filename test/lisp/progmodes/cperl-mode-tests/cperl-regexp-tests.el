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
