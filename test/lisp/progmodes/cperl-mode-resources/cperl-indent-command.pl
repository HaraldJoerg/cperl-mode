#!/usr/bin/env perl
use strict;
use warnings;
use 5.020;

# This file contains test input and expected output for the tests in
# cperl-mode-tests.el, cperl-mode-test-indent-command.  The code is
# syntactically valid, but doesn't make much sense.

# (Bug#11733) https://debbugs.gnu.org/cgi/bugreport.cgi?bug=11733
# -------- sub \n brace: input
sub foo
  {
  }

  sub bar
  {
  }
# -------- sub \n brace: expected output --------
sub foo
{
}

sub bar
{
}
# -------- sub \n brace: end

# -------- map \n brace: input
{
my %hash = map
{ $_ = 1 }
@_;
}

sub bar
  {
  }
# -------- map \n brace: expected output --------
{
  my %hash = map
    {
      $_ = 1
    }
    @_;
}
# -------- map \n brace: end

# -------- if \n brace: input

if (1 == @_)
  {
    say "one argument";
  } else {
    say "No arguments, or plenty";
  }

sub bar
  {
  }
# -------- if \n brace: expected output --------
{
  my %hash = if
    {
      $_ = 1
    }
    @_;
}
# -------- if \n brace: end

