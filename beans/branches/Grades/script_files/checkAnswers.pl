#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/Bless Dump/;
use Grades;

my $answers = Grades::Script->new_with_options;
my $id = $answers->league;
my $exam = $answers->exam;

my $league = League->new( id => $id );
my $grades = Grades->new( league => $league );

$DB::single=1;
my $groups = $grades->jigsawGroups( $exam );

my $response;
for my $group ( keys %$groups ) {
	$response->{Chinese}->{$group} = 0;
	my $quiz = $grades->quiz( $exam, $group );
	my $topic = $grades->topic($exam, $group);
	my $form = $grades->form($exam, $group);
	my ($codedvalue, $n);
	for my $item ( @$quiz ) {
		if ( $item->{option} ) {
			my $option = $item->{option};
			$codedvalue->[$n++] = { map {
				$option->[$_] => $_ } 0..$#$option };
		}
		else { $codedvalue->[$n++] = { True => 1, False => 0 }; }
	}
	my $idsbyRole = $grades->idsbyRole( $exam, $group );
	my $responses = $grades->responses( $exam, $group );
	for my $id ( @$idsbyRole ) {
		my $score = 0;
		for my $n ( 0 .. $#$quiz ) {
			my $myanswer = $responses->{$id}->{$n+1};
			my $theanswer = $codedvalue->[$n]->{
				$quiz->[$n]->{answer} };
			unless ( defined $myanswer ) {
				warn "${id}'s answer on question " . ($n+1) .
					" in " . $topic . $form . " quiz?";
				next;
			}
			unless ( defined $theanswer ) {
				die "Right answer on question " . ($n+1) .
					" in " . $topic . $form . " quiz?";
			}
			$score++ if $myanswer == $theanswer;
		}
		$response->{letters}->{$group}->{$id} = $score;
		$response->{letters}->{$group}->{story} =
				$grades->topic( $exam, $group ) .
				$grades->form( $exam, $group );
	}
	Bless( $response->{letters}->{$group} )->keys([ @$idsbyRole, 'story' ]);
}

print Dump $response;
