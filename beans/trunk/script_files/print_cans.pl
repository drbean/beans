#!/usr/bin/perl 

# Created: 西元2014年06月02日 15時04分40秒
# Last Edit: 2014  6月 16, 14時36分50秒
# $Id$

=head1 NAME

print_cans.pl - \n separated list of can names of league in session

=cut

use strict;
use warnings;

use YAML qw/LoadFile Dump/;
use IO::All;
use Cwd; use File::Basename;

=head1 SYNOPSIS

print_ids.pl -l GL00019 > members.txt

=cut

use Grades;

my $script = Grades::Script->new_with_options( league => basename(getcwd) );
pod2usage(1) if $script->help;
pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;
my $leagueId = $script->league;
my $session = $script->session;
$leagueId = basename( getcwd ) if $leagueId eq '.';
my $l = League->new( id => $leagueId );
my $g = Grades->new({ league => $l });
my $co = Compcomp->new({ league => $l });
my $cl = $g->classwork;

my $cans = $cl->beancan_names($session);
my @cans = sort keys %$cans;
$, = "\n";
print STDOUT @cans;

=head1 DESCRIPTION

Use in vim to get list of ids, eg :r! print_ids.pl

=cut



=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2014 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of print_members.pl

# vim: set ts=8 sts=4 sw=4 noet:


