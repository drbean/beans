#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/Bless Dump/;
use Grades;
use Cwd; use File::Basename;

my $scantron = Grades::Script->new_with_options;
my $id = $scantron->league || basename( getcwd );
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
	my $groupresponse = $response->{$group};
	my %questions; @questions{1..$qn } = ( undef ) x $qn;
	@$groupresponse{@$idsbyRole} = ( \%questions ) x @$idsbyRole;
	Bless( $response->{$group}->{$_} )->keys( [1..$qn] ) for @$idsbyRole;
	Bless( $response->{$group} )->keys( $idsbyRole );
}
print Dump $response;
