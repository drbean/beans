package beans::Controller::Homework;

use strict;
use warnings;
use parent 'Catalyst::Controller';

=head1 NAME

beans::Controller::Homework - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index 

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->response->body('Matched beans::Controller::Homework in Homework.');
}

=head2 request

Seek listing of homework scores for one player.

=cut

sub request : Local {
	my ($self, $c) = @_;
}

=head2 list

Calculate homework score for one player using Moose homework script.

=cut

sub list : Local {
	my ($self, $c) = @_;
	$c->stash->{player} = "DrBean";
	$c->stash->{id} = "DrBean";
	my @weeks = (
		{ name => 1, score => 2 },
		{ name => 3, score => 2 },
		{ name => 5, score => 0 },
	);
	$c->stash->{weeks} = \@weeks;
	# $c->stash->{template} = "homework/list.tt2";
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
