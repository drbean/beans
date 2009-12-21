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


=head2 listing

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
			my $max = $work->examMax;
			my $names = $work->examids;
			my @exams = map { {	name => $names->[$_],
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


=head2 raw

Show exam tallies that allowed allocation of exam grade.

=cut

sub raw : Local {
    my ( $self, $c ) = @_;
    my $params     = $c->request->params;
    my $leagueId   = $params->{league} || $c->request->args->[0];
    my $playerId   = $params->{id} || $c->request->args->[1];
    my $playerName = $params->{player} || $c->request->args->[2];
    my $exam       = $c->request->args->[3];
    my $league     = League->new( id => "/home/drbean/class/$leagueId" );
    my $work       = Grades->new( league => $league );
    if ( $league and $league->is_member($playerId) ) {
        my $player = Player->new( league => $league, id => $playerId );
        if ( $playerName eq $player->name ) {
            $c->stash->{league} = $leagueId;
            $c->stash->{player} = $playerName;
            $c->stash->{id}     = $playerId;
            $c->stash->{examId} = $exam;
            my $group = $work->name2examGroup( $exam, $playerName )->[0];
            my $quiz = $work->quiz( $exam, $group );
            my $responses = $work->responses( $exam, $group );
            my $member = $work->examGroupMembers( $exam, $group );
            my $rawscores = $work->rawExamScores( $exam, $group );
            my $role = $work->id2examGroupRole( $exam, $group );
            my @scores = map { $rawscores->{$_} }
		sort { $role->{$a} cmp $role->{$b} } keys %$rawscores;
            $c->stash->{ids} = $work->idsbyRole( $exam, $group );
            $c->stash->{topic} = $work->topic( $exam, $group );
            $c->stash->{form} = $work->form( $exam, $group );
            $c->stash->{quiz} = $quiz;
            $c->stash->{responses} = $responses;
            $c->stash->{scores} = \@scores;
            my $total = sum( @scores );
            $c->stash->{total} = $total;
            my $grade = $work->examResultHash->{$playerId}->{$exam};
            $c->stash->{grade}    = $grade;
            $c->stash->{group}    = $group;
            $c->stash->{template} = 'rawexam.tt2';
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
