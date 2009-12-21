package beans::Controller::Root;

use strict;
use warnings;
use parent 'Catalyst::Controller';

use lib 'lib';
use Grades;
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
			$c->stash->{total} = $work->totalMax;
			my $percent = $work->homework->{$playerId};
			$c->stash->{percent} = $work->sprintround( $percent );
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
			if ( $leagueId =~ m/^GL000/ or $leagueId eq 'FLA0016' )
			{
				$weeks = $work->conversations;
				@grades = map { {
					name=> $_,
					grade => $work->sprintround(
						$work->points($_)->{$playerId})
						} } @$weeks;
				my $lastweek = $weeks->[-1];
				my $correct = $work->correct($lastweek)
								->{$playerId};
				%raw = ( name => 'Correct', score => $correct );
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
				my $lastweek = $weeks->[-1];
				my $lastgrp = $work->name2beancan(
							$lastweek, $player);
				my $merit = $work->meritDemerit($lastweek)->
							{$lastgrp};
				%raw = ( name => "Merits", score => $merit );
				$classwork = $work->classwork->{$playerId};
                        }
			$grades[-1]->{name} .= "($raw{name})";
			$classwork = $work->sprintround($classwork);
			$c->stash->{percent} = $classwork;
			$grades[-1]->{grade} .= "($raw{score})";
			$c->stash->{weeks} = \@grades;
		}
	}
}

=head2 exams_listing

Calculate exams score for one player using Moose exams script.

=cut

sub exams_listing : Local {
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
		}
	}
}

=head2 grades

Request a listing of grades

=cut 

sub grades : Local {
	my ($self, $c) = @_;
}

=head2 grades_listing

Calculate grades for one player using Moose grades.

=cut

sub grades_listing : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league};
	my $player = $params->{player};
	my $playerId = $params->{id};
	my $league = League->new( id => "/home/drbean/class/$leagueId" );
	my $grades = Grades->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $playerobj = Player->new(league => $league, id => $playerId);
		if ( $player eq $playerobj->name ) {
			my $name = $player;
			my $component;
			if ( $leagueId =~ m/^GL000/ or $leagueId eq 'FLA0016' )
			{
				$component = "compwork";
			}
			else { $component = "classwork"; }
			my $classwork = $grades->$component->{$playerId};
			my $homework = $grades->homework->{$playerId};
			my $examGrade = $grades->examGrade->{$playerId};
			my $grade = $grades->grades($component)->{$playerId};
			$classwork = $grades->sprintround($classwork);
			$homework = $grades->sprintround($homework);
			$examGrade = $grades->sprintround($examGrade);
			$grade = $grades->sprintround($grade);
			$c->stash->{league} = $leagueId;
			$c->stash->{id} = $playerId;
			$c->stash->{player} = $name;
			my $weights = $grades->weights;
			my $total = sum values %$weights;
			$c->stash->{weight} = $weights;
			$c->stash->{total} = $total;
			$c->stash->{classwork} = $classwork;
			$c->stash->{homework} = $homework;
			$c->stash->{exams} = $examGrade;
			$c->stash->{grade} = $grade;
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
