#!/usr/bin/perl 

# Created: 03/21/2013 10:08:14 PM
# Last Edit: 2013 Mar 22, 09:21:54 AM
# $Id$

=head1 NAME

print_grades.pl - Format classwork, homework, exams, final grades

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

use strict;
use warnings;
use IO::All;
use YAML qw/LoadFile DumpFile/;
use Cwd;

=head1 SYNOPSIS

print_grades.pl -l AFN231

=cut


my $session = 1;
my $dirs = '/home/drbean/012';

(my $dir = getcwd) =~ s/^.*\/([^\/]*)$/$1/;
use Grades;
my $l = League->new( leagues => $dirs, id => $dir );
my $g = Grades->new({ league => $l });
my %m = map { $_->{id} => $_ } @{ $l->members };
my $approach = $l->approach;
my $c = $g->classwork;

=head1 DESCRIPTION

A gradesheet

=cut

my $hw = $g->homeworkPercent;
my %hw = map { $_ => $g->sprintround( $hw->{$_} ) } keys %$hw;
my $classwork = $approach->new( league => $l )->totalPercent;
my %classwork = map { $_ => $g->sprintround( $classwork->{$_} ) } keys %$classwork;

my $comp = Compcomp->new({ league => $l });
my $ex1 = $l->inspect("$dirs/$dir/exam/$session/g1.yaml");
my $ex2 = $comp->points($session);
# $l->save("$dirs/$dir/exam/$session/g2.yaml", $ex2);
$ex2 = $l->inspect("$dirs/$dir/exam/$session/g2.yaml");
my %exams = map { $_ => ( $ex1->{$_} + $ex2->{$_} ) / 2 } keys %m;
$l->save("$dirs/$dir/exam/$session/g.yaml", \%exams);
my $ex = $g->examPercent;
my %ex = map { $_ => $g->sprintround( $ex->{$_} ) } keys %$ex;

my $grade = $g->grades;

my $weights = $g->weights;
my $io = io 'grades.txt';
my @grades = $l->id . " " . $l->name . " " . $l->field . " Grades\n" .
"Classwork: " . $weights->{classwork} . "\n" .
"Homework: " . $weights->{homework} . "\n" .
"Exams: " . $weights->{exams} . "\n" .
"Name\tId\tClasswork\tHomework\Exams\Grade\n";
my @ids = sort keys %m;
for my $id ( @ids ) {
	push @grades,
"$m{$id}->{name}\t$id\t$classwork{$id}\t$hw{$id}\t$ex{$id}\t$grade->{$id}\n";
}


$io->print(@grades);
$io->autoflush;


=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2013 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of print_grades.pl

# vim: set ts=8 sts=4 sw=4 noet:


