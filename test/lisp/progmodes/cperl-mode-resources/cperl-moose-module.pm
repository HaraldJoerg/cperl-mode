# ABSTRACT: Testing cperl-mode
use 5.24.0;
package My::Moo::dule {

    use Moo;
    use Types::Standard qw(Str);
    use namespace::clean;

    use feature qw(signatures);
    no warnings qw(experimental::signatures);

    use Plack::Builder;

    has an_attribute => (
        is => 'ro', isa => Str,
        documentation => '"has" should be highlighted here.'
    );

    builder {
        enable "Foo";
        mount "/" => sub { ...; };
    };

    sub my_method ($self,$params) {
        ...;
    }
}
1;

__END__

=encoding utf8

=head1 NAME

My::Moo::dule - Testing cperl-mode

=head1 SYNOPSIS

  Just load that module and check whether "has" is fontified correctly.
