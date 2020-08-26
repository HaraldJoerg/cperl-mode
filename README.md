# cperl-mode
Bring new language features and popular DSLs into cperl-mode

cperl-mode, created by Bob Olson and then enhanced and made popular by Ilya Zakharevich in the previous century, is the most popular major mode for editing Perl code with [Gnu Emacs](https://www.gnu.org/software/emacs/).  A version of cperl-mode is included with Emacs.
However, recent and upcoming enhancements of Perl are not (yet) included.

The starting point for this repository is cperl-mode.el from the Gnu [Git repository](https://savannah.gnu.org/git/?group=emacs) as of 2020-06-04, which includes [Jonathan Rockway's additions](https://github.com/jrockway/cperl-mode).

If you have found a bug or desire a change in cperl-mode, then we
recommend the traditional `M-x report-emacs-bug` procedure, make sure
to mention "cperl-mode" in the email subject.  It simply gets more
attention over there (including mine).

Issue reports or discussion here isn't lost, either.  If you send pull requests, please note that substantial contributions can only be included with Emacs if the authors transfer the copyright to the Free Software Foundation.

## Motivation

The Perl programming language is evolving, and so should cperl-mode.  Currently there's [Ovid's initiative](https://github.com/Ovid/Cor/wiki) to bring "native" object-oriented keywords into the Perl core.  We can't _run_ this code yet, but why shouldn't we be able to _write_ it with proper support by the editor?

Also, many popular modules import subroutines into your source code which behave like keywords, though technically they are just plain subroutines.  Yet, I'd love to read such source code with highlighting of these keywords.  Examples for such modules are OO-frameworks like Moose et al. with "keywords" like `has` and `extends`, test frameworks with `is`, `is_deeply` and many others, Plack with `builder`, `enable`  and `mount`, and various exception handlers with `try`, `catch`, and `finally` (the latter are already included in vanilla cperl-mode thanks to Jonathan Rockway).

I have not yet checked whether someone else is also working on the same topic, but I want to have something to show when I do.

## The Plan

My first (unpublished) approach was a mode derived from cperl-mode, but this ran into too many issues with the current code of cperl-mode, so I decided to dig deeper and start refactoring.  I plan to:

 1. Factor out quite a lot of regular expression literals into variables
    * Doing so, use `regexp-opt` at runtime to make them easier to read (this is a FIXME anyway)
    * Also, reduce and stabilize the numbers of capture groups in the regular expressions
 2. Build "feature sets" of keywords for try/catch, Moo(se), Cor, and maybe more
 3. Stay compatible with cperl-mode's features, customization options and documentation methods
 4. Eliminate some workarounds for issues which no longer exist

As an effect, I expect this version to be _slower_ than the original, but I don't think this is an issue in 2020.
Also, I expect to make slow progress, as I'm doing this in my spare time.

## Status

The current status is reported in the
[NEWS](https://github.com/HaraldJoerg/cperl-mode/blob/master/etc/NEWS)
file, which follows Emacs conventions for style and location.

## The Future

Once this fork is considered sufficiently stable, it is intended to
distribute it using the usual Emacs channels: One option is to make it
the official version which ships with Emacs.  In that case, publishing
it via [ELPA](https://elpa.gnu.org/) makes sense, to allow adjustments
to new versions of Perl or new features on our own schedule.
Alternatively, it could be published on [MELPA](https://melpa.org/),
but in that case it should be renamed (the package, and _all_ the
symbols within) to avoid confusion with the official package.

This repository might continue to exist for experimental stuff or
for collecting feeedback from the Perl community: The Emacs mailing
lists might have too much "noise" which isn't relevant for Perl
programmers at all.
