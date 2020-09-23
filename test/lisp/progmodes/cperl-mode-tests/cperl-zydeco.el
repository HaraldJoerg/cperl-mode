;;; cperl-indexing-test.el --- Test indexing in cperl-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020-2020 ...to be decided ...

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords:       internal
;; Human-Keywords: internal
;; Homepage: https://github.com/HaraldJoerg/cperl-mode

;;; Commentary:

;; This is a collection of Tests for indexing of Perl modules,
;; classes, subroutines, methods and whatnot.

;; Run these tests interactively:
;; (ert-run-tests-interactively '(tag :indexing))


;; Adapted from flymake
(defvar cperl-mode-tests-data-directory
  (expand-file-name "lisp/progmodes/cperl-mode-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../../.."
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory containing cperl-mode test data.")

(ert-deftest cperl-test-zydeco-indenting ()
  "Rudimentary verify that Zydeco sources are indented properly."
  (let ((file (expand-file-name "zydeco.pl"
                                cperl-mode-tests-data-directory))
	(expect (expand-file-name "zydeco_expected.pl"
				  cperl-mode-tests-data-directory)))
    (with-temp-buffer
      (insert-file file)
      (cperl-mode)
      (cperl-set-style "PBP")
      (indent-region (point-min) (point-max))
      (cperl-set-style-back)
      (let ((result (buffer-substring-no-properties
		     (point-min) (point-max))))
	(find-file-existing expect)
	(cperl-mode)
	(should (equal result (buffer-substring-no-properties
		     (point-min) (point-max))))
	(kill-buffer)))))
      
