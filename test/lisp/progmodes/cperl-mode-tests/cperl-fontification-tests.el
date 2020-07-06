;;; cperl-fontification-tests.el --- Test fontification in cperl-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020-2020 ...to be decided ...

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords:       internal
;; Human-Keywords: internal
;; Homepage: https://github.com/HaraldJoerg/cperl-mode

;;; Commentary:

;; This is a collection of Tests for the fontification of CPerl-mode.
;; The primary target is to verify that the refactoring we're doing
;; right now (May 2020 - ...) doesn't change any behavior, or does the
;; right thing in cases where new fontification rules are enabled.

;; Run these tests interactively:
;; (ert-run-tests-interactively '(tag :fontification))

;; Adapted from flymake
(defvar cperl-mode-tests-data-directory
  (expand-file-name "lisp/progmodes/cperl-mode-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../../.."
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory containing cperl-mode test data.")

(defun cperl-test-face (text regexp)
  "Returns the face of the first character matched by REGEXP in TEXT."
  (interactive)
  (with-temp-buffer
    (let ((cperl-hairy nil)
	  (cperl-font-lock nil)) ;; Does this matter?
      (insert text)
      (cperl-mode)
      (font-lock-fontify-buffer)
      (goto-char (point-min))
      (re-search-forward regexp)
      (get-text-property (match-beginning 0) 'face))))

(ert-deftest jrockway-issue-45 ()
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
      (beginning-of-buffer)
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
      (beginning-of-buffer)
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
      (beginning-of-buffer)
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



