#!/usr/bin/perl
use v5.26;
use strict;
use warnings;

package O3D {
    use Zydeco;

    class Coordinate {
	type_name Coord;
	has coordinates ( type => ArrayRef[Num], required => true );

	coerce from ArrayRef[Num] via from_aref {
	    $class->new ( coordinates => [ @$_ ] );
	}
    }

    class Sphere {
	with Movable;
	has radius   ( type => Num, required => true );
	has location ( type => Coord, required => true );
	class +Painted with Color {
	    multi method clean () {
		$self->rgb->@* = (0,0,0);
	    }
	}

    }

    role Color {
	has rgb ( type    => ArrayRef[Num],
		  default => sub { [0,0,0] },
		 );
	method paint (Num $red, Num $green, Num $blue) {
	    $self->rgb->@* = ($red,$green,$blue);
	}
    }

    role Movable {
	requires location;
	method moveto (Num $x, Num $y, Num $z) {
	    $self->location->coordinates->@* = ($x, $y, $z);
	}
    }

    class Colored_Sphere {
	factory sphere ($r,$c) = $class->new(radius => $r, location => $c);
	extends Sphere with Color;
    }

}

package main;
use Data::Dump qw/dump/;

my @coords = (1,2,3);
my $sphere = O3D->sphere ( radius   => 2,
			   location => [0,1,0],
			   rgb      => [1,0,0]);
$sphere->moveto(1,0,0);
$sphere->paint(0,1,0);
dump $sphere;

$sphere = O3D->new_sphere_painted ( radius   => 2,
				    location => [0,1,0],
				    rgb      => [1,0,0]);
$sphere->clean;
dump $sphere;
