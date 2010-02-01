#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/Bless Dump/;
use Grades;

my $scantron = Grades::Script->new_with_options;
my $id = $scantron->league;
my $exam = $scantron->exam;

my $league = League->new( id => $id );
my $grades = Grades->new( league => $league );

my $members = $league->members;

my %members = map { $_->{name} => $_ } @$members;
my $groups = $grades->jigsawGroups( $exam );


my $response;
for my $group ( keys %$groups ) {
	my $idsbyRole = $grades->idsbyRole( $exam, $group);
	my $qn = $grades->qn( $exam, $group );
	for my $id ( @$idsbyRole ) {
		$response->{$group}->{$id} = { map { $_ => 0 } 1 .. $qn };
		Bless( $response->{$group}->{$id} )->keys( [1..$qn] );
	}
	Bless( $response->{$group} )->keys( $idsbyRole );
}
print Dump $response;
