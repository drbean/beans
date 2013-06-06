#!/usr/bin/perl 

# Created: 03/21/2013 10:08:14 PM
# Last Edit: 2013 Mar 27, 11:11:21 AM
# $Id$

=head1 NAME

sum_g12.pl - Mean of jigsaw scores and compcomp scores

=cut

use strict;
use warnings;
use IO::All;
use YAML qw/Dump LoadFile DumpFile/;
use Cwd;

=head1 SYNOPSIS

sum_g12.pl > exam/1/g.yaml

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

Exams are simultaneous jigsaw and compcomp activities. Average those 2 scores.
Jigsaw scores are already in g1.yaml.

=cut

my $comp = Compcomp->new({ league => $l });
my $ex1 = $l->inspect("$dirs/$dir/exam/$session/g1.yaml");
my $ex2 = $comp->points($session);
$l->save("$dirs/$dir/exam/$session/g2.yaml", $ex2);
$ex2 = $l->inspect("$dirs/$dir/exam/$session/g2.yaml");
my %exams = map { $_ => ( $ex1->{$_} + $ex2->{$_} ) / 2 } keys %m;

print Dump \%exams;


=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2013 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of sum_g12.pl

# vim: set ts=8 sts=4 sw=4 noet:


