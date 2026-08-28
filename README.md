# cperl-mode
Bring new language features and popular DSLs into cperl-mode

cperl-mode, created by Bob Olson and then enhanced and made popular by Ilya Zakharevich in the previous century, is the most popular major mode for editing Perl code with [Gnu Emacs](https://www.gnu.org/software/emacs/).  A version of cperl-mode is included with Emacs.
However, recent and upcoming enhancements of Perl are not (yet) included.

As of August 2026, cperl-mode is available as a
[GNU ELPA package](https://elpa.gnu.org/packages/cperl-mode.html).

This means that Emacs users don't have to wait for a new version of
Emacs or to pull the file from a git repository to get the most recent
development.  Just run `M-x list-packages` and you will find
cperl-mode on the list for easy installation (works with Emacs 26 and
above).

Development of cperl-mode will continue in the
[GNU Emacs repository](https://cgit.git.savannah.gnu.org/cgit/emacs.git).
The file `cperl-mode.el` will occasionally be mirrored to the
"upstream" branch in this repository, which is now the main branch.
The "master" branch is kept, but the approach I made there to modify
cperl-mode turned out to be a dead end (in my defense, it was my first
attempt at Emacs lisp).  I'd rather port functionality from the master
branch into the Emacs repository than to fix its bugs here.

If you have found a bug or desire a change in cperl-mode, then we
recommend the traditional `M-x report-emacs-bug` procedure. Make sure
to mention "cperl-mode" in the email subject.  It simply gets more
attention over there (including mine).

Issue reports or discussion here isn't lost, either.  If you send pull
requests, please note that substantial contributions can only be
included with Emacs if the authors transfer the copyright to the Free
Software Foundation.

## Motivation

The Perl programming language is evolving, and so should cperl-mode.
When Ovid started his [initiative](https://github.com/Ovid/Cor/wiki)
to bring "native" object-oriented keywords into the Perl core, I found
that cperl-mode has not seen any adaption to new Perl syntax... and
started this repository, and also contributing.  I got commit rights
to the Emacs repository some years ago, so this repository is mostly
for people who prefer the GitHub user interface.

## Status

The current cperl-mode supports Emacs versions 26 and newer, and
understands Perl syntax up to Perl 5.44.

I would like to add support for CPAN modules which bring their own
syntax. [Moose](https://metacpan.org/pod/Moose) might be the most
prominent example, but there are many many others.  This would require
to bump the minimum Emacs version to 28.
