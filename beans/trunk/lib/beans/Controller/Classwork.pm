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
    my ( $self, $c ) = @_;
    my $params   = $c->request->params;
    my $leagueId = $params->{league} || $c->request->args->[0];
    my $playerId = $params->{id} || $c->request->args->[1];
    my $player   = $params->{player} || $c->request->args->[2];
    my $league   = League->new(
		leagues => $c->config->{leagues}, id => $leagueId );
    my $work   = Grades->new({ league => $league });
    my $classwork = $work->classwork;
    if ( $league and $league->is_member($playerId) ) {
        my $playerobj = Player->new( league => $league, id => $playerId );
        if ( $player eq $playerobj->name ) {
            $c->stash->{league} = $leagueId;
            $c->stash->{player} = $player;
            $c->stash->{id}     = $playerId;
            my ( $weeks, @grades, $totals );
	    $weeks  = $classwork->all_weeks;
	    @grades = map {
		{
		    name => $_,
		    grade =>
		      $work->sprintround( $classwork->points($_)->{$playerId} )
		}
	    } @$weeks;
	    $totals = $classwork->total->{$playerId};
            $c->stash->{percent} = $totals;
            $c->stash->{weeks}   = \@grades;
            $c->stash->{raw} =
              $c->uri_for_action( 'classwork/raw', $leagueId, $playerId,
                $player );
            $c->stash->{template} = 'classwork_listing.tt2';
        }
    }
}

=head2 raw

Show classwork tallies that allowed allocation of homework grade.

=cut

sub raw : Local {
    my ( $self, $c ) = @_;
    my $params     = $c->request->params;
    my $leagueId   = $params->{league} || $c->request->args->[0];
    my $playerId   = $params->{id} || $c->request->args->[1];
    my $playerName = $params->{player} || $c->request->args->[2];
    my $round      = $c->request->args->[3];
    my $league   = League->new(
		leagues => $c->config->{leagues}, id => $leagueId );
    my $work   = Grades->new({ league => $league });
    my $classwork = $work->classwork;
    if ( $league and $league->is_member($playerId) ) {
        my $player = Player->new( league => $league, id => $playerId );
        if ( $playerName eq $player->name ) {
            $c->stash->{league}   = $leagueId;
            $c->stash->{player}   = $playerName;
            $c->stash->{id}       = $playerId;
            $c->stash->{round}    = $round;
            $c->stash->{approach} = $league->approach;
            my $exercise;
            if ( $league->approach eq "Compcomp" ) {
		my $comp = Compcomp->new( league => $league );
                my $qns     = $comp->correct($round);
                my $correct = $qns->{$playerId};
                my $someothers  = $comp->opponents($round);
                my $otherid = $someothers->{$playerId};
		my $other;
                if ( $otherid =~ m/bye|late|unpaired|transfer/i ) {
		    $other = Nonentity->new( league => $league, id => $otherid,
						name => $otherid );
		}
		else {
                    $other = Player->new(
                        league => $league,
                        id     => $otherid
                    );
		}
		my $othercorrect = $qns->{$otherid};
		$exercise = {
		    correct      => $correct,
		    otherid      => $otherid,
		    othername    => $other->name,
		    othercorrect => $othercorrect
		};
	    }
	    else {
		my $session  = $work->week2session($round);
		my $can      = $work->name2beancan( $round, $playerName );
		my $members  = $work->beancans($session)->{$can};
		my $merits   = $work->merits($round)->{$can};
		my $demerits = $work->demerits($round)->{$can};
		$exercise = {
		    beancan  => $can,
		    members  => $members,
		    merits   => $merits,
		    demerits => $demerits,
		};
	    }
	    $c->stash->{exercise} = $exercise;
	    $c->stash->{demerits} = $c->uri_for_action( 'classwork/demerits',
		$leagueId, $playerId, $playerName );
	    $c->stash->{template} = 'rawclasswork.tt2';
	}
    }
}

=head2 demerits

Show absences, tardies that make up demerits.

=cut

sub demerits : Local {
	my ($self, $c) = @_;
	my $params = $c->request->params;
	my $leagueId = $params->{league} || $c->request->args->[0];
	my $playerId = $params->{id} || $c->request->args->[1];
	my $playerName = $params->{player} || $c->request->args->[2];
	my $round =                      $c->request->args->[3];
	my $league   = League->new(
		leagues => $c->config->{leagues}, id => $leagueId );
	my $approach = Approach->new( league => $league );
	my $class = Classwork->new( approach => $approach );
	my $work   = Grades->new( league => $league, classwork => $class  );
	$league->approach->meta->apply($work);
	if ( $league and $league->is_member($playerId) )
	{
		my $player = Player->new( league => $league, id => $playerId );
		if ( $playerName eq $player->name ) {
			my $approach = $league->approach;
			$c->stash->{league} = $leagueId;
			$c->stash->{player} = $playerName;
			$c->stash->{id} = $playerId;
			$c->stash->{round} = $round;
			$c->stash->{approach} = $approach;
			my $session = $work->week2session($round);
			my $can = $work->name2beancan( 
				$round, $playerName );
			my $members = $work->beancans($session)->{$can};
			my $absences = $work->absences( $round )->{$can};
			my $tardies = $work->tardies($round)->{$can};
			my $exercise = {
				beancan => $can,
				members => $members,
				absences => $absences,
				tardies => $tardies, };
			$c->stash->{exercise} = $exercise;
			$c->stash->{template} = 'meritsdemerits.tt2';
		}
	}
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

# vim: set ts=8 sts=4 sw=4 noet:

1;
