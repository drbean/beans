#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/LoadFile DumpFile/;

my $class = LoadFile "$ENV{HOME}/n/class.yaml";
my %names = map { $_->{id} => $_->{name} } @{$class->{member}};
my $orig = "teams.yaml";
my $yaml = LoadFile $orig;
DumpFile "$orig.bak", $yaml;

my $new;
my @teams = keys %$yaml;
map {
	my @team = ();
	my $team = $_;
	#push @team, map { @names{keys %{$yaml->{$team}->{$_}}} }
	#					keys %{$yaml->{$team}};
	#push @team, map { @names{@{$yaml->{$team}->{$_}}} }
	#					keys %{$yaml->{$team}};
	push @team, values %{$yaml->{$team}};
	$new->{$team} = \@team;
	} @teams;

DumpFile "$orig", $new;

__END__

=head1 NAME

beans - Convert file from id keys to name keys

=head1 SYNOPSIS

beans [options] 

Options:

--help            This help message

--man            A man page

--id 9598457	Id of person claiming these beans to be theirs

--name Momotaro	Name of person claiming these beans to be theirs

--season first	The part of the semester in which the beans were won

--team Gray	The team to which the claimant belongs

--league m/j	The league to which the claimant belongs

--beans 75	Number of beans they are asking to be recorded 

=head1 OPTIONS

=over 8

=item B<-id>

Id of person claiming these beans to be theirs.

=item B<-name>

Name of person claiming these beans to be theirs.

=item B<-league>

The league to which the redeemer belongs

=item B<-season>

The season in which the beans were won.

=item B<-team>

The team to which the claimant belongs.

=item B<-beans>

Number of beans they are asking to be recorded.

=back

=head1 DESCRIPTION

B<beans> tallies beans that students have earned for classwork and are redeeming and stores them in beans.yaml as a total, logging their entry in beans.log. It thens adds the total (divided by 5) to homework, midterm and final scores and outputs the grade so far.

=cut
