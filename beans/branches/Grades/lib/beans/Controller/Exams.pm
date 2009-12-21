package beans::Controller::Exams;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Grades;
use List::Util qw/sum/;

=head1 NAME

beans::Controller::Exams - Exams Controller for beans

=head1 DESCRIPTION

Listing, raw actions

=head1 METHODS

=cut


=head2 exams_listing

Calculate exams score for one player using Moose exams script.

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
			my ($weeks, @grades, %raw, $classwork);
			my $exams = $work->examResults->{$playerId};
			my $examGrade = $work->examGrade->{$playerId};
			$examGrade = $work->sprintround($examGrade);
			my $examPercent = $work->examPercent->{$playerId};
			$examPercent = $work->sprintround($examPercent);
			my @names = qw/I II III IV/;
			my $max = $work->examMax;
			my @exams = map { {	name => $names[$_],
						grade => $work->sprintround(
							$exams->[$_]) . "/$max"
					} } 0..$#$exams;
			$c->stash->{exams} = \@exams;
			$c->stash->{total} = $examGrade;
			$c->stash->{percent} = $examPercent;
			$c->stash->{weeks} = \@grades;
			$c->stash->{template} = 'exams_listing.tt2';
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
