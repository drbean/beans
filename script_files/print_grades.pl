#!/usr/bin/perl 

# Created: 03/21/2013 10:08:14 PM
# Last Edit: 2016 Sep 10, 07:47:37 PM
# $Id$

=head1 NAME

print_grades.pl - Format classwork, homework, exams, final grades

=cut

use strict;
use warnings;
use IO::All;
use YAML qw/LoadFile DumpFile/;
use List::Util qw/min max/;
use Cwd;

=head1 SYNOPSIS

print_grades.pl -x curve -o 60 -w 80 -t 100 > grades.txt

=cut


my $session = 1;
my $dirs = "/home/drbean/$ENV{SEMESTER}";

(my $dir = getcwd) =~ s/^.*\/([^\/]*)$/$1/;

use Grades;
my $l = League->new( leagues => $dirs, id => $dir );
my $g = Grades->new({ league => $l });
my %m = map { $_->{id} => $_ } @{ $l->members };
my $approach = $l->approach;
my $c = $g->classwork;

my $script = Grades::Script->new_with_options;
my $low = $script->one || 60;
my $high = $script->two || 100;
my $median = $script->weights || 80;
my $curving = $script->exercise;

=head1 DESCRIPTION

A gradesheet, with grades curved from low, through median to high if exercise (-x) is "curve".


=cut

my $hw = $g->homeworkPercent;
my %hw = map { $_ => $g->sprintround( $hw->{$_} ) } keys %$hw;
my $classwork = $approach->new( league => $l )->totalPercent;
my %classwork = map { $_ => $g->sprintround( $classwork->{$_} ) } keys %$classwork;

my $ex = $g->examPercent;
my %ex = map { $_ => $g->sprintround( $ex->{$_} ) } keys %$ex;

my $grade = $g->grades;;
my @grade = values %$grade;
my $top = max @grade;
my $bottom = min @grade;
my $middle = $g->median( \@grade );
my $mean = $g->mean( \@grade );

if ( defined $curving and $curving and $curving eq "curve" ) {
    $grade = $g->curve( $low, $median, $high );
}

my $adjusted_mean = $g->mean( [values %$grade] );
my $weights = $g->weights;
my @grades = $l->id . " " . $l->name . " " . $l->field . " Grades\n" .
"Classwork: " . $weights->{classwork} . "\n" .
"Homework: " . $weights->{homework} . "\n" .
"Exams: " . $weights->{exams} . "\n";

push @grades, 
"Raw\n" .
"  Low: " . $bottom . "\n" .
"  Median: " . $middle . "\n" .
"  Mean " . $mean . "\n" .
"  High: " . $top . "\n";

push @grades, 
"Curving:\n" .
"  Low: " . $low . "\n" .
"  Median: " . $median . "\n" .
"  Mean: " . $adjusted_mean . "\n" .
"  High: " . $high . "\n" .
"Name\tId\t   Classwork    Homework\tExams\tGrade\n" if $curving eq "curve";

my @ids = sort keys %m;
for my $id ( @ids ) {
	push @grades,
"$m{$id}->{Chinese}\t$id\t\t$classwork{$id}\t$hw{$id}\t$ex{$id}\t$grade->{$id}\n";
}
push @grades, "\n";
my $out =  $l->yaml->{out};
my @outids = map $_->{ id }, @$out;
for my $id ( @outids ) {
    push @grades,
"Dropout\t$id\t\t0\t0\t0\t0\n"}


print(@grades);

warn "Classwork weeks: @{ $c->all_events } \n";
warn "Homework weeks: @{ $g->rounds } \n";
warn "Exams: @{ $g->examids } \n";

=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2013 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of print_grades.pl

# vim: set ts=8 sts=4 sw=4 noet:


