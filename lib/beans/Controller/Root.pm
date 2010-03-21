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

=head2 classwork

Request a listing of classwork results

=cut 

sub classwork : Local {
	my ($self, $c) = @_;
}


=head2 grades

Request a listing of grades

=cut 

sub grades :Path :Args(0) {
	my ($self, $c) = @_;
}

=head2 listing

Calculate grades for one player using Moose grades.

=cut

sub listing : Local {
    my ( $self, $c ) = @_;
    my $params   = $c->request->params;
    my $leagueId = $params->{league};
    my $player   = $params->{player};
    my $playerId = $params->{id};
    my $league   = League->new(
		leagues => $c->config->{leagues}, id => $leagueId );
    my $grades   = Grades->new( league => $league );
    if ( $league and $league->is_member($playerId) ) {
        my $playerobj = Player->new( league => $league, id => $playerId );
        if ( $player eq $playerobj->name ) {
            my $name        = $player;
	    $league->approach->meta->apply( $grades );
            my $classwork   = $grades->classworkPercent->{$playerId};
            my $homework    = $grades->homeworkPercent->{$playerId};
            my $examPercent = $grades->examPercent->{$playerId};
            my $grade       = $grades->grades->{$playerId};
            $classwork          = $grades->sprintround($classwork);
            $homework           = $grades->sprintround($homework);
            $examPercent        = $grades->sprintround($examPercent);
            $grade              = $grades->sprintround($grade);
            my $weights = $grades->weights;
            my $total   = sum values %$weights;
            $c->stash->{weight}    = $weights;
            $c->stash->{total}     = $total;
            $c->stash->{classwork} = $classwork;
            $c->stash->{classwork_listing} =
              $c->uri_for_action( 'classwork/listing', $leagueId, $playerId, $name );
            $c->stash->{homework} = $homework;
            $c->stash->{homework_listing} =
              $c->uri_for_action( 'homework/listing', $leagueId, $playerId, $name );
            $c->stash->{exams} = $examPercent;
            $c->stash->{exams_listing} =
              $c->uri_for_action( 'exams/listing', $leagueId, $playerId, $name );
            $c->stash->{grade} = $grade;
        }
    }
    $c->stash->{template} = 'grades_listing.tt2';
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
