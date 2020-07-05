sub just_for_indenting {
    my $here = <<HERE;
    test for a not indented here-doc containing
    HERE
    which should not terminate the here-doc
HERE
    return;
}
# In this edge case, the terminator is contained in the here-doc, but
# doesn't terminate it because for 'standard' here-docs, the delimiter
# must start in column one.
