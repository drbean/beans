package beans::View::TT;

use strict;
use base 'Catalyst::View::TT';
use Template::Stash;

$Template::Stash::LIST_OPS->{decoded} = sub {
	my $list = shift;
	my %hash;
	@hash{ 0 .. $#$list } = ( @$list );
	return \%hash if $list;
	};

# __PACKAGE__->config(TEMPLATE_EXTENSION => '.tt');
__PACKAGE__->config(TEMPLATE_EXTENSION => '.tt2',
	               # Set the location for TT files
               INCLUDE_PATH => [
                       beans->path_to( 'root/src' ),
                   ],
		TIMER => 0,
		WRAPPER => 'wrapper.tt2',
           );


=head1 NAME

beans::View::TT - TT View for beans

=head1 DESCRIPTION

TT View for beans. 

=head1 AUTHOR

=head1 SEE ALSO

L<beans>

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
