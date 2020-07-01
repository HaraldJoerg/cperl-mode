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
      (message "%s" (match-string 0))
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
