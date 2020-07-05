sub just_for_indenting {
    my $here = <<~HERE;
    test for an indented here-doc
    HERE
    return;
}
# As of Perl 5.26, the starter '<<~' introduces an indented here-doc.
# The terminator of this here-doc does not need to start in column
# one.  Also, the indentation of the here-doc's content needs to be
# equal or greater than that of the terminator.
