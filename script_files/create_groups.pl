#!/usr/bin/perl 

# Created: 10/15/2011 07:52:09 PM
# Last Edit: 2012  1月 30, 18時28分47秒
# $Id$

=head1 NAME

create_groups.pl - Partition league into teams fairly on basis of ratings

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

use 5.10.0;
use strict;
use warnings;
use IO::All;
use YAML qw/LoadFile DumpFile Dump/;
use Cwd; use File::Basename;
use POSIX qw/floor ceil/;

use Grades;

=head1 SYNOPSIS

create_groups.pl -l . -s 2 | sponge classwork/2/groups.yaml

=cut



=head1 DESCRIPTION

Takes league and individual members' grades and partititions into the teams in $league->yaml->{groupwork}/$session/groups.yaml, $n (or $n-1) players to a team, so that the sum of the grades of members of each team are similar.

=cut


my $script = Grades::Script->new_with_options( league => basename(getcwd) );
pod2usage(1) if $script->help;
pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;
my $leagues = "/home/greg/001";
my $leagueId = $script->league;
$leagueId = basename( getcwd ) if $leagueId eq '.';
my $l = League->new( leagues => '/home/greg/001', id => $leagueId );
my $g = Grades->new({ league => $l });
my $members = $l->members;
my %m = map { $_->{id} => $_ } @$members;
my $grades = $g->grades;

my $session = $script->session;
my $lastsession = $session - 1 if $session > 1;

my $n = 3;

my $gs = LoadFile "classwork/$lastsession/groups.yaml";
my @colors = sort keys %$gs;
my %g;
my @graded = sort { $grades->{$a} <=> $grades->{$b} }keys %m;
my @t = map  $m{$_}->{name}, @graded;
my $groups = ceil @t/$n;
my $rumpPlayers = @t % $n;
my $rumpGroups = $rumpPlayers == 0?	0: $n - $rumpPlayers;

if ( $n == 4 ) {
    my $half =	$rumpPlayers == 1?	ceil @t/2:
		$rumpPlayers == 2?	@t/2:
		$rumpPlayers == 3?	floor @t/2:
		$rumpPlayers == 0?	@t/2 - 1:
					die "rumpPlayers greater than $n";
    if ( $rumpPlayers ) {
	    for my $k ( 0 .. $rumpGroups -1 ) {
		    $g{ $colors[ -1 -$k ] } = [ $t[$k],
						$t[ ( $half - $k ) ],
						$t[ -1 -$k ] ];
	    }
    }
    for my $i ( $rumpGroups .. $groups-1 ) {
	    $g{ $colors[ $i - $rumpGroups ] } = [ $t[ $i ],
						    $t[ $half - $i ],
						    $t[ $#t - ( $half - $i ) ],
						    $t[ -1 - $i ] ];
    }
}

if ( $n == 3 ) {
    if ( $rumpPlayers ) {
	    for my $k ( 0 .. $rumpGroups -1 ) {
		    $g{ $colors[ -1 -$k ] } = [ $t[$k],
						$t[ -1 -$k ] ];
	    }
    }
    my $half = @t/2;
    my @sign = (+1,-1);
    for my $i ( $rumpGroups .. $groups-1 ) {
	    my $j = $i - $rumpGroups;
	    $g{ $colors[ $j ] } = [ $t[ $i ],
						$t[ $half + $sign[$j % 2] * ($j)/2 ],
						$t[ -1 - $i ] ];
    }
}

print Dump \%g;

=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2011 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of create_groups.pl

# vim: set ts=8 sts=4 sw=4 noet:


