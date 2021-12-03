
;;; perl-tidy-ediff.el --- Compare sources with perltidy results   -*- lexical-binding:t -*-

;; Copyright (C) 2021 Harald Jörg <haj@posteo.de>

;; Author: Harald Jörg
;; Keywords: languages, Perl

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public Licenseq
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:


;;; In place of a manual....

(defvar perl-tidy-manual 'please-ignore-this-value
  "perl-tidy-ediff.el - why?

The idea came as a suggestion by bojinlund to the haj's
cperl-mode repository.  The point of this implementation is that
it does not write anything to disk: Everything happens in Emacs
buffers.  This makes it different from other perltidy
integrations found in the net, and from running perltidy from the
command line.  Also, it compares the original source with the
tidied version in an ediff session, so you can review the
suggested changes, one by one, and only then decide to save your
source code.

The name space \"perl-tidy\" has been chosen intentionally so
that it can be used on top of any other perltidy Elisp packages.

perl-tidy-ediff.el comes with three related functions:

  * `perl-tidy-ediff' runs the perltidy program on the current
    buffer, and then runs ediff between the current buffer and
    the tidied result.

  * `perl-tidy-ediff-region' runs the perltidy program on the
    current region and starts am ediff session between the
    narrowed source and the tidied version of it.  This might
    cause strange error messages if the current region isn't
    valid Perl code.

  * `perl-tidy-ediff-sub' runs the perltidy program on the
    subroutine which starts before point and starts an ediff
    session between the source code, narrowed to the subroutine,
    and its tidied version.

In all cases, saving the source buffer to a file is not required,
and the tidied result is not written to a file.  Error messages
of perltidy go to a buffer *perl-tidy-errors*.  Nothing is
written to disk, so there's no need to cleanup the source
directory from perltidy's .tdy and .ERR files.

There are two customization settings:

  * `perl-tidy-command' is the path to your perltidy executable.
    The default is \"perltidy\", assuming that it is somewhere on
    your path (where it should be after installing perltidy).

  * `perl-tidy-ediff-args' are options to pass to perltidy.  The
    default is empty.  For example, to use your own
    project-specific perltidy profile, add an option
    \"--profile=.../.perltidyrc\".")

;;; Customization

(defcustom perl-tidy-command "perltidy"
  "The command to run perltidy."
  :group 'perl
  :type 'string)

(defcustom perl-tidy-ediff-args
  nil
  "List of options to pass to perltidy for ediff preparation.
These will be applied on top of the options defined in
`perl-tidy-fixed-ediff-args'.  Use this option to define your own
profile."
  :group 'perl
  :type '(repeat string))

(defvar perl-tidy-fixed-ediff-args
  '("--standard-output"
    "--standard-error-output"
    "--no-check-syntax")
  "List of options we always pass to perltidy for ediff preparation.")


;;; Interactive Functions

(defun perl-tidy-ediff ()
  "Run perltidy on the current buffer, then start an ediff session,
comparing the original source with the perltidy output.  Error
messages of perltidy are saved in a buffer '*perl-tidy-errors*'."
  (interactive)
  (with-current-buffer (perl-tidy--get-source-buffer)
    (perl-tidy--ediff)))

(defun perl-tidy-ediff-region ()
  "Run perltidy on the region, then start an ediff session,

comparing the original source with the perltidy output.  The
source buffer will stay narrowed to the region for the ediff
session.  Error messages of perltidy are saved in a buffer
'*perl-tidy-errors*'."
  (interactive)
  (with-current-buffer (perl-tidy--get-source-buffer)
    (perl-tidy--ediff (region-beginning) (region-end))))

(defun perl-tidy-ediff-sub ()
  "Run perltidy on the subroutine or method before point,
then start an ediff session, comparing the original source with
the perltidy result.  The source buffer will stay narrowed to the
subroutine or method for the ediff session.  Error messages of
perltidy are saved in a buffer '*perl-tidy-errors*'."
  (interactive)
  (with-current-buffer (perl-tidy--get-source-buffer)
    (save-excursion
      (let (begin end)
	(if (null (beginning-of-defun))
	    (message "Point is not in a subroutine or method")
	  (setq begin (point))
	  (end-of-defun)
	  (setq end (point))
	  (perl-tidy--ediff begin end))))))


;;; Internal functions

(defun perl-tidy--get-source-buffer ()
  "Make sure that the source buffer is the current buffer.
This recovers from the situation where one of the perl-tidy
functions is called while the ediff control buffer is current."
  (if (string-equal major-mode "ediff-mode")
      ediff-buffer-A
    (current-buffer)))

(defun perl-tidy--ediff (&optional begin end)
  "Runs perltidy from BEGIN to END.
This is the non-interactive backend for both `perl-tidy-ediff',
`perl-tidy-ediff-region' and `perl-tidy-ediff-sub'.  This
function assumes that the current buffer is the source buffer."
  (let* ((ediff-control-buffer-name "*Ediff Control Panel*")
	 (source-buffer (current-buffer))
	 (source-buffer-name (buffer-name source-buffer))
	 (current-mode major-mode)
	 (output-buffer-name (concat "*perl-tidy-" source-buffer-name "*"))
	 (error-buffer-name (concat "*perl-tidy-errors*"))
	 (perltidy-fixed-arguments
	  (mapconcat 'identity perl-tidy-fixed-ediff-args " "))
	 (perltidy-arguments
	  (mapconcat 'identity perl-tidy-ediff-args " "))
	 error-buffer)

    ;; Don't kill an existing error buffer, maybe it is visible in a
    ;; window right now.  Instead, erase the content and mark as
    ;; unmodified.
   (setq error-buffer (get-buffer error-buffer-name))
    (when error-buffer
       (with-current-buffer error-buffer
	(erase-buffer)
	(set-buffer-modified-p nil)))

    ;; Run the command
    (shell-command-on-region
     (or begin (point-min)) (or end (point-max))
     (concat perl-tidy-command
	     " " perltidy-fixed-arguments
	     " " perltidy-arguments)
     output-buffer-name nil		; don't clobber the source
     error-buffer-name t)		; but save errors

    ;; When perltidy reports errors, Let the error buffer show the
    ;; name of the source buffer instead of "<stdin>".
    (when (and error-buffer
	       (buffer-modified-p error-buffer))
      (with-current-buffer error-buffer
	(goto-char (point-min))
	(while (search-forward-regexp "^<stdin>:" nil t)
	  (replace-match (concat "<" (buffer-name source-buffer) ">:") t))
	(message "perltidy error (see buffer %s)" error-buffer-name)))

    ;; Adjust the tidied buffer so that it matches the source
    (with-current-buffer output-buffer-name
      (read-only-mode)
      (set-buffer-modified-p nil)
      (funcall current-mode))
    (when begin
      (narrow-to-region begin end))

    ;; Reuse and update the ediff session if there's one, start a
    ;; fresh one otherwise.  That way, we don't lose the "current"
    ;; ediff hunk when updating the diff regions.
    (if (and (get-buffer ediff-control-buffer-name)
	     (with-current-buffer ediff-control-buffer-name
	       (and (eq ediff-buffer-A source-buffer)
		    (string-equal (buffer-name ediff-buffer-B)
				  output-buffer-name))))
	(with-current-buffer ediff-control-buffer-name
	  (ediff-update-diffs))
      (ediff-buffers source-buffer output-buffer-name nil 'perltidy-ediff))))

;;; perl-tidy-ediff.el ends here
