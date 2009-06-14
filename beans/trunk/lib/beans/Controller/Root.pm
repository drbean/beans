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
	my $league = League->new( leagueId => "/home/drbean/class/$leagueId" );
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
	my $league = League->new( leagueId => "/home/drbean/class/$leagueId" );
	my $work = Classwork->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $playerobj = Player->new(league => $league, id => $playerId);
		if ( $player eq $playerobj->name ) {
			my $name = $player;
			my $weeks = $work->allweeks;
			my @grades;
			die "@$weeks" unless @$weeks;
			for my $week ( @$weeks ) {
				my $group = $work->name2beancan($week, $name);
				my $grade = $league->sprintround($work->work2grades($week)->{$group});
				push @grades, {
					name => $week,
					grade => $grade};
			}
			my $lastweek = $weeks->[-1];
			my $lastgrp = $work->name2beancan($lastweek, $player);
			my $merit = $work->meritDemerit($lastweek)->{$lastgrp};
			$grades[-1]->{name} .= "(Merits)";
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $name;
			$c->stash->{id} = $playerId;
			$c->stash->{percent} = sum(map { $_->{grade} } @grades);
			$grades[-1]->{grade} .= "($merit)";
			$c->stash->{weeks} = \@grades;
			$grades[-1]->{grade} .= "($merit)";
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
	my $league = League->new( leagueId => "/home/drbean/class/$leagueId" );
	my $grades = Grades->new( league => $league );
	if ( $league and $league->is_member($playerId) )
	{
		my $playerobj = Player->new(league => $league, id => $playerId);
		if ( $player eq $playerobj->name ) {
			my $name = $player;
			my $classwork = $grades->classwork->{$playerId};
			my $homework = $grades->homework->{$playerId};
			my $examGrade = $grades->examGrade->{$playerId};
			my $weights = $grades->weights;
			my $total = sum @$weights;
			my $grade = ( $classwork*$weights->[0] +
				$homework*$weights->[1] +
				$examGrade*$weights->[2] ) / $total;
			$classwork = $league->sprintround($classwork);
			$homework = $league->sprintround($homework);
			my $exams = $grades->examResults->{$playerId};
			my @names = qw/I II III IV/;
			my @exams = map { {	name => $names[$_],
						grade => $league->sprintround( $exams->[$_] )
					} } 0..$#$exams;
			$grade = $league->sprintround($grade);
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $name;
			$c->stash->{id} = $playerId;
			$c->stash->{weight} = $weights;
			$c->stash->{classwork} = $classwork;
			$c->stash->{homework} = $homework;
			$c->stash->{exams} = \@exams;
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
