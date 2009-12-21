#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/LoadFile DumpFile Bless/;
use Grades;

my $scantron = Grades::Script->new_with_options;
my $id = $scantron->league;
my $exam = $scantron->exam;

my $l = League->new( id => $id );
my $g = Grades->new( league => $l );

die "$id/$exam/response.yaml already exists: $!\n" if
				-e "$id/$exam/response.yaml";
my $m = $l->members;
my %m = map { $_->{name} => $_ } @$m;
my $gps = $g->examGroups('exam3');

my $qn = $g->examConfig($exam)->{questions}->[0];

my $responses = { map { $_ => 0 } 1..$qn };
my $r;
for my $gp ( keys %$gps ) {
	my @idsbyRole;
	my $namesbyRole = $g->examGroupMembers( $exam, $gp );
	for my $role ( sort keys %$namesbyRole ) {
		my $id = $m{ $namesbyRole->{$role} }->{id};
		$r->{$gp}->{$id} = { map { $_ => 0 } 1 .. $qn };
		push @idsbyRole, $id;
	}
	Bless( $r->{$gp} )->keys( \@idsbyRole );
}
DumpFile "$id/$exam/response.yaml", $r;
