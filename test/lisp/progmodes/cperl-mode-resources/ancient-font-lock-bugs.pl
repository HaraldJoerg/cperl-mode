# Tests for things which apparently have been fixed long ago, but went with
# the comments until version 6.2

# The comment read:
# ;; The following kinds of Perl code erroneously start strings:
# ;; \$`  \$'  \$"
# ;; $opt::s  $opt_s  $opt{s}  (s => ...)  /\s+.../
# ;; likewise with m, tr, y, q, qX instead of s

# The tests verify that the semicolons or other stuff after constructs
# which are accused to erroneously start strings are fontified as they
# should be.

my $string = "All in the golden afternoon\n   Full leisurely we glide:";

$string =~ /\s+golden\s+/;

my $prematch = $`;    # "All in the"
my $postmatch = $';   # "afternoon\n" ... to the end

local $" = ' ';       # In Lisp, lists are separated by spaces

my %opt = (s => "option s");
my $opt_s = $opt{s};

my $other_modules_s = $opt::s // $opt_s;

