sub just_for_indenting {
    my $here = <<HERE;
test for a traditional here-doc
HERE
    return;
}
# This is a classical here-doc: Everything following the line
# containing the starter is a string, up to (and excluding) the
# terminator which needs to start in column 1.
