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

(ert-deftest cperl-test-package-name-block-indexing ()
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

;;; For testing tags, we need files - buffers won't do it.
(ert-deftest cperl-etags-basic ()
  "Just open a buffer in cperl-mode and run `cperl-etags`."
  (let ((file (expand-file-name "cperl-indexing.pm"
                                cperl-mode-tests-data-directory)))
    (find-file file)
    (cperl-mode)
    (cperl-etags)
    (find-file "TAGS")
    (goto-char (point-min))
    (should (search-forward "Pack::Age"))
    (should (search-forward "foo"))
    (delete-file "TAGS")
    (kill-buffer)))

(ert-deftest cperl-write-tags-basic ()
  "Just open a buffer in cperl-mode and run `cperl-write-tags`."
  (let ((file (expand-file-name "cperl-indexing.pm"
                                cperl-mode-tests-data-directory)))
    (find-file file)
    (cperl-mode)
    (cperl-write-tags)
    (find-file "TAGS")
    (goto-char (point-min))
    (should (search-forward "Pack::Age"))
    (should (search-forward "foo"))
    (delete-file "TAGS")
    (kill-buffer)))

(ert-deftest cperl-write-tags-from-menu ()
  "Just open a buffer in cperl-mode and run `cperl-etags` recursively."
  (let ((file (expand-file-name "cperl-indexing.pm"
                                cperl-mode-tests-data-directory)))
    (find-file file)
    (cperl-mode)
    (cperl-write-tags nil t t t) ;; from the Perl menu "Tools/Tags"
    (find-file "TAGS")
    (goto-char (point-min))
    (should (search-forward "cperl-indexing.pm"))
    (should (search-forward "Pack::Age"))
    (should (search-forward "foo"))
    (goto-char (point-min))
    (should (search-forward "cperl-moose-module.pm"))
    (should (search-forward "My::Moo::dule")) ;; written as package NAME BLOCK
    (should (search-forward "my_method")) ;; This sub doesn't start in column 1
    (goto-char (point-min))
    (should (search-forward "cperl-moosex-declare.pm"))  ;; extra keywords!
    (should (search-forward "BankAccount")) ;; a class, not a module
    (should (search-forward "deposit")) ;; a method, not a sub
;;  (should (search-forward "CheckingAccount")) ;; a  subclass FAILS
    (delete-file "TAGS")
    (kill-buffer)))

(ert-deftest cperl-function-parameters ()
  "Play around with the keywords of Function::Parameters"
  (let ((file (expand-file-name "function-parameters.pm"
                                cperl-mode-tests-data-directory))
        (cperl-automatic-keyword-sets t))
    (find-file file)
    (cperl-mode)
    (cperl-imenu--create-perl-index)
    (let* ((index-alist (cperl-imenu--create-perl-index))
           (packages-alist (assoc "+Packages+..." index-alist))
           (unsorted-alist (assoc "+Unsorted List+..." index-alist))
           )
      (should (markerp (cdr (assoc "Main::foo" unsorted-alist)))) ;; a "fun"ction
      (should (markerp (cdr (assoc "Main::bar" unsorted-alist)))) ;; a method
      (should (markerp (cdr (assoc "package Main" packages-alist))))
      (should (markerp (cdr (assoc "package Derived" packages-alist)))))))

