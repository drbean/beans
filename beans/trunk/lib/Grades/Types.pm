package Grades::Types;

use MooseX::Types -declare =>
	[ qw/PlayerName PlayerNames PlayerId Member Members
		HomeworkRound HomeworkRounds HomeworkResults/ ];

use MooseX::Types::Moose qw/ArrayRef HashRef Str Maybe/;

subtype PlayerName, as Str, where { $_ =~ m/^[A-Z][a-z]+$/ };

subtype PlayerNames, as Maybe[ ArrayRef[ PlayerName ] ], message
	{ 'PlayerNames are A CAPITAL letter, followed by little letters' };

subtype PlayerId, as Str, where { $_ =~ m/^[a-zA-Z]?[0-9]+$/ };

subtype Member, as HashRef, where {
	PlayerName->check( $_->{name} )
	and PlayerId->check( $_->{id} )
};
subtype Members,
	as ArrayRef [Member],
	message { 'League members are hashrefs with name, id keys' };

subtype HomeworkRound,
	as Str, where { $_ =~ m{/(\d+)\.yaml$} };
subtype HomeworkRounds,
	as ArrayRef[HomeworkRound],
	message { 'Homework rounds are of form, [1, 3..5, 7..]' };

subtype HomeworkResults,
	as HashRef,
	where { 
		my $results = $_;
		for my $round ( keys %$results ) {
			for my $player ( keys %{ $results->{$round} } ) {
				PlayerId->check( $player ) and 
				Num->check( $_->{$player} );
			}
		}
	},
	message {
"HomeworkRound is a hashref of player ids and their scores for each round" };

no MooseX::Types::Moose;
no MooseX::Types;

1;

# vim: set ts=8 sts=4 sw=4;
