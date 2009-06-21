package Grades::Types;

use List::MoreUtils qw/all/;

use MooseX::Types -declare =>
	[ qw/PlayerName PlayerNames AbsenteeNames PlayerId Member Members
		HomeworkResults
		Beancans Card
		Exam
		Weights/ ];

use MooseX::Types::Moose qw/Int Num ArrayRef HashRef Str Maybe/;

subtype PlayerName, as Str, where { $_ =~ m/^[A-Z][A-Za-z -]+\d?$/ };

subtype PlayerNames, as ArrayRef[ PlayerName ], message
{ 'PlayerNames are A CAPITAL letter, followed by little letters, and an optional digit to disambiguate students with same name,' };

subtype AbsenteeNames, as Maybe[ PlayerNames ], message
	{ 'AbsenteeNames is a possibly empty list of PlayerNames' };

subtype PlayerId, as Str, where { $_ =~ m/^[a-zA-Z]?[0-9]+$/ };

subtype Member, as HashRef, where {
	PlayerName->check( $_->{name} )
	and PlayerId->check( $_->{id} )
};

subtype Members,
	as ArrayRef [Member],
	message { 'League members are hashrefs with name, id keys' };

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

subtype Card,
	as HashRef,
	where {
		my $card = $_;
		all {
			my $can = $_;
			Str->check( $can ) and 
			Int->check( $card->{$can}->{merits} ) and
			Int->check( $card->{$can}->{absences} ) and
			Int->check( $card->{$can}->{tardies} );
		}
		keys %$card;
	},
	message { 'Probably undefined or non-numeric Merit, Absence, Tardy scores, or possibly illegal beancan,' };

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

subtype Weights, 
	as HashRef[Int],
	where {
		defined $_->{classwork} and
			defined $_->{homework} and
			defined $_->{exams} and 
			$_->{classwork} + $_->{homework} + $_->{exams} == 100;
		},
	message{ "Classwork, homework, exam weights not defined, or don't sum to 100 percent," };

no MooseX::Types::Moose;
no MooseX::Types;

1;

# vim: set ts=8 sts=4 sw=4;
