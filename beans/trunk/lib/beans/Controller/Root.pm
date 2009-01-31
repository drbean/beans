package beans::Controller::Root;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Bean;

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config->{namespace} = '';

=head1 NAME

beans::Controller::Root - Root Controller for beans

=head1 DESCRIPTION

Only 3 actions are possible for the user with this Controller, homework, classwork and grades requests, so they are here rather than have separate namespaces.

=head1 METHODS

=cut

=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    # Hello World
    $c->response->body( $c->welcome_message );
}

sub default :Path {
    my ( $self, $c ) = @_;
    $c->response->body( 'Page not found' );
    $c->response->status(404);
    
}

=head2 end

Attempt to render a view, if needed.

=cut 

sub end : ActionClass('RenderView') {}

=head2 homework

Seek listing of homework scores for one player.

=cut

sub homework : Local {
	my ($self, $c) = @_;
}

=head2 homework_listing

Calculate homework score for one player using Moose homework script.

=cut

sub homework_listing : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league};
	my $playerName = $params->{player};
	my $playerId = $params->{id};
	my $league = Homework->new( leagueId => "/home/greg/beans/$leagueId" );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			my $rounds = $league->rounds;
			my $grades = $player->hwgrades;
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

=head2 classwork

Request a listing of classwork results

=cut 

sub classwork : Local {
	my ($self, $c) = @_;
}

=head2 classwork_listing

Calculate classwork score for one player using Moose classwork script.

=cut

sub classwork_listing : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league};
	my $playerName = $params->{player};
	my $playerId = $params->{id};
	my $league = Classwork->new( leagueId => "/home/greg/beans/$leagueId" );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			my $rounds = $league->rounds;
			my $grades = $player->hwgrades;
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
