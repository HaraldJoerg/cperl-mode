# cperl-mode
Bring new language features and popular DSLs into cperl-mode

cperl-mode, created by Bob Olson and then enhanced and made popular by Ilya Zakharevich in the previous century, is the most popular major mode for editing Perl code with [Gnu Emacs](https://www.gnu.org/software/emacs/).  The version of cperl-mode which is included with Emacs gets regular updates to keep track of new Perl syntax: The upcoming version (Emacs 30) supports Perl versions up to 5.40, so in particular the "class" feature.  The "hot" version [cperl-mode.el from the master branch](https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/cperl-mode.el) works with Emacs 27 or newer.

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

## Branches in this repository

During the migration period to the Emacs repository we have three
branches here, but the number will decrease in due time.

 * The 'master' branch contains features which are best called
   experimental: Support for new features and keyword-creating
   modules.  There's quite a list which is already supported, among
   them popular OO frameworks like
   [Moo](https://metacpan.org/pod/Moo)/[Moose](https://metacpan.org/pod/Moo)
   and [Zydeco](https://metacpan.org/pod/Zydeco) and widely used
   modules for testing like
   [Test::More](https://metacpan.org/pod/Test::More).  Unfortunately
   the *implementation* of the extensions is a bit of a dead end and
   needs some rework before it can make its way into the official
   Emacs repository.
 * The 'upstream' branch is a copy of `cperl-mode.el` from the master
   branch of the [official repository of Gnu
   Emacs](https://git.savannah.gnu.org/cgit/emacs.git).  It contains
   about a dozen bug fixes which have not yet been distributed with
   Emacs.  This version now covers Perl syntax up to version 5.38
   (in particular, the feature 'class' stuff).
 * The branch perl5.39 will be used to develop and test support
   for Perl syntax of Perl 5.39/5.40.

## The Plan

My first (unpublished) approach was a mode derived from cperl-mode, but this ran into too many issues with the current code of cperl-mode, so I decided to dig deeper and start refactoring.  I plan to:

 1. Factor out quite a lot of regular expression literals into variables
    * Doing so, use `regexp-opt` at runtime to make them easier to read (this is a FIXME anyway)
    * Also, reduce and stabilize the numbers of capture groups in the regular expressions
 2. Build "feature sets" of keywords for try/catch, Moo(se), Cor, and
    maybe more.  These are intended to be made available as minor modes on top of CPerl mode.
 3. Stay compatible with cperl-mode's features, customization options and documentation methods
 4. Eliminate some workarounds for issues which no longer exist

As an effect, I expect this version to be _slower_ than the original, but I don't think this is an issue in 2020.
Also, I expect to make slow progress, as I'm doing this in my spare time.

## Status

The current status is reported in the
[NEWS](https://github.com/HaraldJoerg/cperl-mode/blob/master/etc/NEWS)
file, which follows Emacs conventions for style and location.

## The Future

Once the features in the master branch are considered sufficiently
stable, it is intended to distribute it using the usual Emacs
channels: One option is to make it the official version which ships
with Emacs.  In that case, publishing it via
[ELPA](https://elpa.gnu.org/) makes sense, to allow adjustments to new
versions of Perl or new features on our own schedule.  Most likely a
dual life of CPerl mode as a core part of Emacs _and_ an ELPA package
will be the way to go.

This repository might continue to exist for experimental stuff or for
collecting feedback from the Perl community: The Emacs mailing lists
might have much "noise" which isn't relevant for Perl programmers at
all.
