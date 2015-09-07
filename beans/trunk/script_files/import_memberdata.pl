#!/usr/bin/env perl 

use strict;
use warnings;

# Created: 07/09/15 16:15:17
# Last Edit:
# $Id$

=head1 NAME

import_memberdata.pl - old students in previous semesters English names

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

use strict;
use warnings;
use IO::All;
use YAML qw/LoadFile Dump/;
use Cwd; use File::Basename;
use Grades;

=head1 SYNOPSIS

import_memberdata.pl -l FLA0011 -o FLA0016 -s 041 -x 032

=cut

my $script = Grades::Script->new_with_options( league => basename(getcwd) );
pod2usage(1) if $script->help;
pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;

my $leagueId = $script->league;
my $semester = $script->session;
my $oldid = $script->one;
my $old_semester= $script->exercise;
my $leagues = "/home/drbean/$semester";
$leagueId = basename( getcwd ) if $leagueId eq '.';

my $dir = basename( getcwd );

my $l = League->new( leagues => "/home/drbean/$semester", id => $dir );
my $g = Grades->new({ league => $l });
my %m = map { $_->{id} => $_ } @{ $l->members };
my $approach = $l->approach;

my $ol = League->new( leagues => "/home/drbean/$old_semester", id => $oldid );
my $om = $ol->members;
my %om = map { $_->{id} => $_ } @$om;

# my @grades = io("../../class/$oldid/grades.txt")->chomp->slurp;
# @grades = map { [ split /: /, $_ ] } @grades;
# my %grades = map { $_->[0] => $_->[1] } @grades;

my $yaml = $l->yaml;
my $members = $l->members;
my @rated;
for my $member ( @$members ) {
	my $id = $member->{id};
	$member->{name} = $om{ $id }->{name} if $om{ $id }->{name};
	# $member->{password} ||= $om{ $id }->{password};
	# $member->{rating} ||= $grades{ $id };
	push @rated, $member;
}

$yaml->{member} = \@rated;

print Dump $yaml;

=head1 DESCRIPTION

Fill in missing (mostly English name) data from old students if it exists in league.yaml files from previous semesters.

Use 'ack field ../../$OLD_SEMESTER' to find suitable leagues.


=cut


=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2015 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of import_memberdata.pl

# vim: set ts=8 sts=4 sw=4 noet:


