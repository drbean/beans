#!/usr/bin/perl 

# Created: 10/15/2011 07:52:09 PM
# Last Edit: 2013 Mar 20, 11:31:08 AM
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

use Getopt::Long;
use Pod::Usage;
use Grades;

use Try::Tiny;

=head1 SYNOPSIS

create_groups.pl -l . -s 2 -n 3 | sponge classwork/2/groups.yaml

=cut



=head1 DESCRIPTION

Takes league and individual members' grades and partititions into the teams in $league->yaml->{groupwork}/$session/groups.yaml, $n (or $n-1) players to a team, so that the sum of the grades of members of each team are similar.

If the number of groups already present in the groups.yaml files is the same as the number of groups which will be generated, the names of the groups are retained. If not, consecutive names in color order are chosen.

If there are rump groups, retain the rump players in the same groups they are already in in groups.yaml, by putting their groups at the end of the line.

=cut


my $script = Grades::Script->new_with_options( league => basename(getcwd) );
pod2usage(1) if $script->help;
pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;
my $leagues = "/home/drbean/012";
my $leagueId = $script->league;
$leagueId = basename( getcwd ) if $leagueId eq '.';
my $l = League->new( leagues => $leagues, id => $leagueId );
my $g = Grades->new({ league => $l });
my $members = $l->members;
my %m = map { $_->{id} => $_ } @$members;
my $grades;
$grades = try { $g->grades } catch { warn "Not grouping on grades: $_";
    $grades = { map { $_ => $m{$_}->{rating} } keys %m } };

my $session = $script->session;
# my $lastsession = $session > 1 ? $session - 1 : 1;
my $lastsession = $session;

my $n = $script->beancan || 3;

my $gs;
$gs = try { LoadFile "classwork/$lastsession/groups.yaml" } catch
    { $gs = {} };
my @keys = keys %$gs;
my @colors = qw/00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19
	20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39/;
my %g;
my @graded = sort { $grades->{$a} <=> $grades->{$b} }keys %m;
my @t = map  $m{$_}->{name}, @graded;
my $groups = ceil @t/$n;
my @groupname = ( @keys == $groups )? sort @keys: @colors[0 .. $groups-1];
my $rumpPlayers = @t % $n;
my $rumpGroups = $rumpPlayers == 0?	0: $n - $rumpPlayers;
my (@resortGroups, @rumpGroupname);
if ( $rumpGroups ) {
    for my $name ( @groupname ) {
	my $members = $gs->{$name};
	if ( $members ) {
	    push @resortGroups, $name if @$members == $n;
	    push @rumpGroupname, $name if @$members < $n;
	    die scalar @$members . " in $name group" if @$members > $n;
	}
    }
}
if ( @resortGroups ) {
    push @resortGroups, @rumpGroupname;
    @groupname = @resortGroups;
}

if ( $n == 4 ) {
    my $half =	$rumpPlayers == 1?	ceil @t/2:
		$rumpPlayers == 2?	@t/2:
		$rumpPlayers == 3?	floor @t/2:
		$rumpPlayers == 0?	@t/2 - 1:
					die "rumpPlayers greater than $n";
    if ( $rumpPlayers ) {
	    for my $k ( 0 .. $rumpGroups -1 ) {
		    $g{ $groupname[ -1 -$k ] } = [ $t[$k],
						$t[ ( $half - $k ) ],
						$t[ -1 -$k ] ];
	    }
    }
    for my $i ( $rumpGroups .. $groups-1 ) {
	    $g{ $groupname[ $i - $rumpGroups ] } = [ $t[ $i ],
						    $t[ $half - $i ],
						    $t[ $#t - ( $half - $i ) ],
						    $t[ -1 - $i ] ];
    }
}

if ( $n == 3 ) {
    if ( $rumpPlayers ) {
	    for my $k ( 0 .. $rumpGroups -1 ) {
		    $g{ $groupname[ -1 -$k ] } = [ $t[$k],
						$t[ -1 -$k ] ];
	    }
    }
    my $half = @t/2;
    my @sign = (-1,+1);
    for my $i ( $rumpGroups .. $groups-1 ) {
	    my $j = $i - $rumpGroups;
	    $g{ $groupname[ $j ] } = [ $t[ $i ],
						$t[ $half + $sign[$j % 2] * ($j)/2 ],
						$t[ -1 - $i ] ];
    }
}

if ( $n == 2 ) {
    my $half = @t/2;
    if ( $rumpPlayers ) {
	for my $k ( 0 .. $rumpGroups -1 ) {
	    $g{ $groupname[ -1 -$k ] } = [ $t[ $half ] ];
	}
    }
    for my $i ( $rumpGroups .. $groups-1 ) {
	$g{ $groupname[ $i - $rumpGroups ] } = [ $t[ $i ],
						$t[ -1 - $i ] ];
    }
    sleep 10;
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


