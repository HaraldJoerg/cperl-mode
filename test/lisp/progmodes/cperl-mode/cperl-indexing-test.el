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


(ert-deftest package-name-block-indexing ()
  "Verify indexing of the syntax package NAME BLOCK.
The syntax package NAME BLOCK is available as of Perl 5.14.
Check that such packages are indexed correctly."
  :tags '(:indexing)
  (let ((code "package Foo::Bar {
    sub baz { ...; }
}"))
    (with-temp-buffer
      (insert code)
      (cperl-mode)
      (cperl-imenu--create-perl-index)
      (let* ((index-alist (cperl-imenu--create-perl-index))
         (packages-alist (assoc "+Packages+..." index-alist))
         (unsorted-alist (assoc "+Unsorted List+..." index-alist))
         )
    (should (markerp (cdr (assoc "package Foo::Bar" packages-alist))))
    (should (markerp (cdr (assoc "Foo::Bar::baz" unsorted-alist))))
    ))))

