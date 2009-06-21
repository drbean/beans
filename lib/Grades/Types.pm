package Grades::Types;

use MooseX::Types -declare => [qw/Name Id Member Members/];

use MooseX::Types::Moose qw/ArrayRef HashRef Str/;

subtype Name, as Str, where { $_ =~ m/^[A-Z][a-z]+$/ };
subtype Id, as Str, where { $_ =~ m/^[a-zA-Z]?[0-9]+$/ };
subtype Member, as HashRef, where { $_->{name} =~ m/^[A-Z][a-z]+$/ and
					$_->{id} =~ m/^[a-zA-Z]?[0-9]+$/ };
subtype Members, as ArrayRef[Member];

no MooseX::Types::Moose;
no MooseX::Types;

1;
