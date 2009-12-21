package beans::Controller::Homework;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Grades;
use List::Util qw/sum/;

=head1 NAME

beans::Controller::Homework - Homework Controller for beans

=head1 DESCRIPTION

Pulling homework actions into own controller.

=head1 METHODS

=cut


=head2 listing

Calculate homework score for one player using Moose homework script.

=cut

sub listing : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league} || $c->request->args->[0];
	my $playerId = $params->{id} || $c->request->args->[1];
	my $playerName = $params->{player} || $c->request->args->[2];
	my $league = League->new( id => "/home/drbean/class/$leagueId" );
	my $work = Grades->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $playerName;
			$c->stash->{id} = $playerId;
			my $rounds = $work->rounds;
			my $grades = $work->hwforid($playerId);
			my %grades;
			@grades{ @$rounds } = @$grades;
			my @weeks = map { { name => $_, score => $grades{$_} } }
				sort keys %grades;
			$c->stash->{weeks} = \@weeks;
			my $total = $work->homework->{$playerId};
			$c->stash->{total} = $work->sprintround( $total );
			my $percent = $work->homeworkPercent->{$playerId};
			$c->stash->{percent} = $work->sprintround( $percent );
			$c->stash->{template} = 'homework_listing.tt2';
		}
	}
}


=head2 raw

Show homework tallies that allowed allocation of homework grade.

=cut

sub raw : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league} || $c->request->args->[0];
	my $playerId = $params->{id} || $c->request->args->[1];
	my $playerName = $params->{player} || $c->request->args->[2];
	my $round = $params->{player} || $c->request->args->[3];
	my $league = League->new( id => "/home/drbean/class/$leagueId" );
	my $work = Grades->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $playerName;
			$c->stash->{id} = $playerId;
			$c->stash->{round} = $round;
			my $rawscores = $work->rawscoresinRound($round);
			my @exercises = map { { name => $_,
					score => $rawscores->{$_}->{$playerId} }
					} sort keys %$rawscores;
			$c->stash->{exercises} = \@exercises;
			my $total = sum ( map { $_->{score} } @exercises );
			$c->stash->{total} = $total;
			my $grade = $work->hwforidasHash($playerId)->{$round};
			$c->stash->{grade} = $grade;
			$c->stash->{template} = 'rawhomework.tt2';
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
