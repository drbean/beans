#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: id2names.pl
#
#        USAGE: ./id2names.pl  
#
#  DESCRIPTION: class list
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Dr Bean (drbean), drbean at cpan, then a dot, (.), and org
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 18/10/15 16:09:45
#     REVISION: ---
#===============================================================================

use strict;
use warnings;
use utf8;

#!/usr/bin/perl 

# Created: 18/10/15 16:09:45
# Last Edit:
# $Id$

=head1 NAME

id2names.pl - create class list

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

use strict;
use warnings;

=head1 SYNOPSIS

id2names.pl -l FLA0008 # prints to id2names_groups.txt

=cut

use IO::All -encoding => 'UTF-8';
use YAML qw/LoadFile DumpFile/;
use Cwd; use File::Basename;
use List::Util qw/max/;
use Encode;
use Lingua::Han::PinYin;



=head1 DESCRIPTION

Creates class list from league.yaml, session/LASTSESSION/groups.yaml with id, Chinese name, pinyin, English name, beancan

=cut

(my $dir = getcwd) =~ s/^.*\/([^\/]*)$/$1/;
use Grades;
use Grades::Groupwork;
my $l = League->new( leagues => '/home/drbean/041', id => $dir );
my $sessions = $l->yaml->{session};
my $last = max keys %$sessions;
my $week = $sessions->{$last};
my $g = Grades->new({ league => $l });
my $co = Compcomp->new({ league => $l });
my $cl = $g->classwork;
my %m = map { $_->{id} => $_ } @{ $l->members };
my %out = map { $_->{id} => $_ } @{ $l->absentees } if $l->absentees;
my $approach = $l->approach;

my $h2p = Lingua::Han::PinYin->new( tone => 1 );

my %b = map { $m{$_}->{name} => $cl->name2beancan($week, $m{$_}->{name}) } keys %m;
my (%i, %by_name, %by_group);
for my $member ( keys %m ) {
	my $week = $sessions->{$last};
	my $name = $m{$member}->{name};
	my $group_n_letter = $cl->name2beancan($week, $name) . " " .
		$cl->name2letter($week, $name);
	my $chinese = encode( 'UTF-8', $m{$member}->{Chinese} );
	my $pinyin = $h2p->han2pinyin( $chinese );
	$i{$member} = "$member $m{$member}->{Chinese} $pinyin\t" . 
		"$name\t$group_n_letter\n";
	$by_name{$name} = $i{$member};
	$by_group{"$group_n_letter"} = $i{$member};
}
for my $member ( keys %out ) {
	my $name = $out{$member}->{name};
	my $group_n_letter = "Out";
	my $chinese = encode( 'UTF-8', $out{$member}->{Chinese} );
	my $pinyin = $h2p->han2pinyin( $chinese );
	$i{$member} = "$member $out{$member}->{Chinese} $pinyin\t" . 
	"$name\t$group_n_letter\n";
	$by_name{$name} = $i{$member};
	$by_group{$group_n_letter} = $i{$member};
}
my @i = "Session: $last, Week: $week\n";
push @i, "IDs\n";
push @i, $i{$_} for sort keys %i;
push @i, "\nNAMES\n";
push @i, $by_name{$_} for sort keys %by_name;
push @i, "\nGROUPS\n";
push @i, $by_group{$_} for sort keys %by_group;
io("id2names_groups.txt")->print(@i);


=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2015 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of id2names.pl

# vim: set ts=8 sts=4 sw=4 noet:


