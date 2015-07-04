#!/usr/bin/perl 

# Created: 西元2014年06月02日 15時04分40秒
# Last Edit: 2015  7月 04, 12時10分41秒
# $Id$

=head1 NAME

zero_homework.pl - hand-filled-in homework.yaml

=cut

use strict;
use warnings;

use YAML qw/LoadFile Dump/;
use IO::All;
use Cwd; use File::Basename;

=head1 SYNOPSIS

zero_homework.pl [ -l FLA0017 -x clay ] >  homework/$LASTWEEK.yaml

=cut

use Grades;

my $script = Grades::Script->new_with_options( league => basename(getcwd) );
pod2usage(1) if $script->help;
pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;
my $leagueId = $script->league;
my $exercise = $script->exercise;

$leagueId = basename( getcwd ) if $leagueId eq '.';
my $l = League->new( id => $leagueId );
my $members = $l->members;
my @ids = sort map { $_->{id} } @$members;

my %grades = map { $_->{id} => 0 } @$members;
my %points = %grades;
print Dump { exercise => $exercise, grade => \%grades, points => \%points };

=head1 DESCRIPTION

Make easy to generate zero homework when have to fill in by hand so can report promptly,

=cut



=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2014 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of zero_homework.pl

# vim: set ts=8 sts=4 sw=4 noet:


