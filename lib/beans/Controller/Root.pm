package beans::Controller::Root;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Bean;
use List::Util qw/sum/;

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
	my $league = League->new( leagueId => "/home/greg/beans/$leagueId" );
	my $work = Homework->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			my $rounds = $work->rounds;
			my $grades = $work->hwforid($playerId);
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $playerName;
			$c->stash->{id} = $playerId;
			$c->stash->{weeks} = [ map { { name => $rounds->[$_],
				score => $grades->[$_] } } 0..$#$grades ];
			$c->stash->{total} = $work->total($playerId);
			$c->stash->{percent} = $league->sprintround(
						$work->percent($playerId) );
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
	my $player = $params->{player};
	my $playerId = $params->{id};
	my $league = League->new( leagueId => "/home/greg/beans/$leagueId" );
	my $work = Classwork->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $playerobj = Player->new(league => $league, id => $playerId);
		if ( $player eq $playerobj->name ) {
			my $name = $player;
			my $weeks = $work->allweeks;
			my @grades;
			for my $week ( @$weeks ) {
				my $group = $work->name2group($week, $name);
				my $grade = $league->sprintround($work->work2grades($week)->{$group});
				push @grades, {
					name => $week,
					grade => $grade};
			}
			my $lastweek = $weeks->[-1];
			my $lastgrp = $work->name2group($lastweek, $player);
			my $merit = $work->meritDemerit($lastweek)->{$lastgrp};
			$grades[-1]->{name} .= "(Merits)";
			$grades[-1]->{grade} .= "($merit)";
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $name;
			$c->stash->{id} = $playerId;
			$c->stash->{weeks} = \@grades;
			$c->stash->{percent} = sum(map { $_->{grade} } @grades);
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
