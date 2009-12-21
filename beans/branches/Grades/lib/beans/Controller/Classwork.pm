package beans::Controller::Classwork;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Grades;
use List::Util qw/sum/;

=head1 NAME

beans::Controller::Classwork - Classwork Controller for beans

=head1 DESCRIPTION

Pulling homework actions into own controller and making available data on how grade was reached.

=head1 METHODS

=cut

=head2 listing

Calculate classwork score for one player using Moose classwork script.

=cut

sub listing : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league} || $c->request->args->[0];
	my $playerId = $params->{id} || $c->request->args->[1];
	my $player = $params->{player} || $c->request->args->[2];
	my $league = League->new( id => "/home/drbean/class/$leagueId" );
	my $work = Grades->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $playerobj = Player->new(league => $league, id => $playerId);
		if ( $player eq $playerobj->name ) {
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $player;
			$c->stash->{id} = $playerId;
			my ($weeks, @grades, $classwork);
			if ( $league->approach eq "compwork" )
			{
				$weeks = $work->conversations;
				@grades = map { {
					name=> $_,
					grade => $work->sprintround(
						$work->points($_)->{$playerId})
						} } @$weeks;
				$classwork = $work->compwork->{$playerId};
			}
			else {
				$weeks = $work->allweeks;
	                        for my $week ( @$weeks ) {
					my $group = $work->name2beancan(
								$week, $player);
					my $grade = $work->sprintround($work
						->work2grades($week)->{$group});
					push @grades,
						{ name=>$week, grade=>$grade};
				}
				$classwork = $work->groupwork->{$playerId};
                        }
			$classwork = $work->sprintround($classwork);
			$c->stash->{percent} = $classwork;
			$c->stash->{weeks} = \@grades;
			$c->stash->{template} = 'classwork_listing.tt2';
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
