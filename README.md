# cperl-mode
Bring new language features and popular DSLs into cperl-mode

## News 2026-01-26

I notice that I have neglected this file for too long.  I removed some
content which I do no longer consider helpful.

As of now, I maintain cperl-mode in the official Emacs repository: I
am happy that the Emacs folks trust me on this.  This repository is
for experiments and immature stuff, so for example there's a branch perl5.44 which is supposed to keep track of the changes in Perl 5.44.

## Overview

cperl-mode, created by Bob Olson and then enhanced and made popular by Ilya Zakharevich in the previous century, is the most popular major mode for editing Perl code with [Gnu Emacs](https://www.gnu.org/software/emacs/).  A version of cperl-mode is included with Emacs.
However, recent and upcoming enhancements of Perl are not (yet) included.

The starting point for this repository is cperl-mode.el from the Gnu [Git repository](https://savannah.gnu.org/git/?group=emacs) as of 2020-06-04, which includes [Jonathan Rockway's additions](https://github.com/jrockway/cperl-mode).

If you have found a bug or desire a change in cperl-mode, then we
recommend the traditional `M-x report-emacs-bug` procedure, make sure
to mention "cperl-mode" in the email subject.  It simply gets more
attention over there (including mine).

Issue reports or discussion here isn't lost, either.  If you send pull requests, please note that substantial contributions can only be included with Emacs if the authors transfer the copyright to the Free Software Foundation.

## Installation Instructions

CPerl mode is not (yet) available as an installable package.  A manual
installation isn't that difficult, though: It consists of just one
file, cperl-mode.el, and it can be used as a drop-in replacement for
the cperl-mode.el which ships with Emacs.

So, to use this version of cperl-mode.el, either clone this repository
or just copy cperl-mode.el to a location of your choice, and then tell
Emacs where to find it in your init file:

  ```(add-to-list 'load-path "/your/directory/here")```

Even better: you can use the "current" version from the Emacs
source tree. [cperl-mode.el from the master
branch](https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/cperl-mode.el)
works with Emacs 27.1 or newer.  This version (occasionally mirrored
to the upstream branch in this repository) does not contain the
experimental support for language extensions, but even more bug fixes.

## Motivation

The Perl programming language is evolving, and so should cperl-mode.  Currently there's [Ovid's initiative](https://github.com/Ovid/Cor/wiki) to bring "native" object-oriented keywords into the Perl core.  We can't _run_ this code yet, but why shouldn't we be able to _write_ it with proper support by the editor?

Also, many popular modules import subroutines into your source code which behave like keywords, though technically they are just plain subroutines.  Yet, I'd love to read such source code with highlighting of these keywords.  Examples for such modules are OO-frameworks like Moose et al. with "keywords" like `has` and `extends`, test frameworks with `is`, `is_deeply` and many others, Plack with `builder`, `enable`  and `mount`, and various exception handlers with `try`, `catch`, and `finally` (the latter are already included in vanilla cperl-mode thanks to Jonathan Rockway).

## Status

A new branch 'perl5.44' offers support for the upcoming Perl version
5.44.  Right now, the only new feature is the support for named
signatures.

