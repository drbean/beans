package beans::Controller::Homework;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib/beans';
use Homework;

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
	my $params = $c->request->params;
	my $leagueId = $params->{league};
	my $playerName = $params->{player};
	my $playerId = $params->{id};
	my $league = League->new( leagueId => "/home/drbean/class/$leagueId" );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			my $rounds = $league->rounds;
			my $grades = $player->grades;
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $playerName;
			$c->stash->{id} = $playerId;
			$c->stash->{weeks} = [ map { { name => $rounds->[$_],
				score => $grades->[$_] } } 0..$#$grades ];
			$c->stash->{total} = $player->total;
			$c->stash->{percent} = $player->percent;

		}
	}
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
