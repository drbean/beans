package Grades::Types;

our $VERSION = 0.06;

use List::MoreUtils qw/all/;

use MooseX::Types -declare =>
	[ qw/PlayerName PlayerNames AbsenteeNames PlayerId Member Members
		HomeworkResults
		Beancans Card
		Exam
		Weights/ ];

use MooseX::Types::Moose qw/Int Num ArrayRef HashRef Str Maybe/;

=head1 NAME

Grades::Types - MooseX::Type checks for homework, classwork and exams data

=head1 SYNOPSIS

	use Grades;
	use Grades::Types qw/Beancans Card/;

	has 'beancanseries' => (is => 'ro', isa => Beancans, lazy_build => 1);
	method _build_beancanseries {
		my $series = $self->series;
		my $league = $self->league->id;
		+{ map { $_ => $self->inspect( "$league/$_/beancans.yaml" ) }
			@$series };
	}

	method card (Num $week) {
		my $card = $self->data->{$week};
		croak "Week $week card probably has undefined or non-numeric Merit, Absence, Tardy scores, or possibly illegal beancan."
		    unless is_Card( $card );
		return $card;
	}

=head1 DESCRIPTION

MooseX::Types extension of Moose::Util::TypeConstraint checking of user input.

=cut

=head1 TYPES

=cut

=head2 PlayerName

A string, where the first letter is upper case, there are some letters or spaces or hyphens, and there is an optional digit at the end to disambiguate same-named players.

=cut

subtype PlayerName, as Str;

=head2 PlayerNames

An array ref of PlayerName.

=cut

subtype PlayerNames, as ArrayRef[ PlayerName ], message
{ 'PlayerNames are letters, and apostrophes and dashes, and an optional digit to disambiguate students with same name,' };

=head2 AbsenteeNames

A possibly undefined PlayerNames list type.

=cut

subtype AbsenteeNames, as Maybe[ PlayerNames ], message
	{ 'AbsenteeNames is a possibly empty list of PlayerNames' };

=head2 PlayerId

A string of digits, with possibly a letter in front.

=cut

subtype PlayerId, as Str, where { $_ =~ m/^[a-zA-Z]?[0-9]+$/ };

=head2 Member

A hashref with name and id keys.

=cut

subtype Member, as HashRef, where {
	PlayerName->check( $_->{name} )
	and PlayerId->check( $_->{id} )
};

=head2 Members

A possibly undefined list of Member.

=cut

subtype Members,
	as ArrayRef [Member],
	message { 'Probably undefined or illegal PlayerNames or PlayerIds,' };

=head2 HomeworkResults

A hashref of the homework keyed on the round (an Int.) For each round, the keys are PlayerId, and the values are scores, or Num.

=cut

subtype HomeworkResults,
	as HashRef,
	where { 
		my $results = $_;
		all {
			my $round = $_;
			Int->check( $round ) and
			all {
				my $player = $_;
				PlayerId->check( $player ) and 
				Num->check( $results->{$round}->{$player} )
			}
			keys %{ $results->{$round} };
		}
		keys %$results;
	},
	message {
"Impossible round number or PlayerId, or missing or non-numerical score," };

=head2 Beancans

A hashref of teams and their constituents, where the keys are the sessions (Str) and the keys for each session are teams, or beancans (ie Str) and the corresponding value is PlayerNames.

=cut

subtype Beancans,
	as HashRef,
	where {
		my $lineup = $_;
		all {
			my $session = $_;
			Str->check( $session ) and
			all {
				my $can = $_;
				Str->check( $can ) and
				PlayerNames->check($lineup->{$session}->{$can});
			}
			keys %{ $lineup->{$session} };
		}
		keys %$lineup;
	},
	message { 'Probably undefined or illegal PlayerName, or possibly illegal session or beancan name,' };

=head2 Card

A hashref of classwork results for the lesson, where the keys are beancan names (Str) and for each beancan there are 'merits', 'absences', and 'tardies' keys, with Int values for each key.

=cut

subtype Card,
	as HashRef,
	where {
		my %card = %$_;
		delete $card{Absent};
		all {
			my $can = $_;
			Str->check( $can ) and 
			Int->check( $card{$can}->{merits} ) and
			Int->check( $card{$can}->{absences} ) and
			Int->check( $card{$can}->{tardies} );
		}
		keys %card;
	},
	message { 'Probably undefined or non-numeric Merit, Absence, Tardy scores, or possibly illegal beancan,' };

=head2 Exam

A hashref of the results for one exam, with PlayerId keys and Num values.

=cut

subtype Exam,
	as HashRef,
	where {
		my $exam = $_;
		all {
			my $id = $_;
			PlayerId->check( $id ) and
			Num->check( $exam->{$id} );
		}
		keys %$exam;
	},
	message { 'Probably undefined or non-numeric Exam score, or possibly illegal PlayerId,' };

=head2 Weights

A hashref of weights for the components making up the grade, where the keys are 'classwork', 'homework', and 'exams', and the corresponding Num value is the weight accorded the component in the grade.

=cut

subtype Weights, 
	as HashRef[Int],
	where {
		Num->check( $_->{classwork} ) and
			Num->check( $_->{homework} ) and
			Num->check( $_->{exams} ) and 
			$_->{classwork} + $_->{homework} + $_->{exams} == 100;
		},
	message{ "Classwork, homework, exam weights not defined, or don't sum to 100 percent," };

no MooseX::Types::Moose;
no MooseX::Types;

1;

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-grades at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Grades>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Grades::Types

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Grades>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Grades>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Grades>

=item * Search CPAN

L<http://search.cpan.org/dist/Grades>

=back

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# vim: set ts=8 sts=4 sw=4 noet:
