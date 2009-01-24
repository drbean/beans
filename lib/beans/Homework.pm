#!/usr/bin/perl

package Script;
use Moose;
with 'MooseX::Getopt';

has 'man' => (is => 'ro', isa => 'Bool');
has 'help' => (is => 'ro', isa => 'Bool');
has 'league' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'l',);
has 'player' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'p',);

package League;
use Moose;
use YAML qw/LoadFile DumpFile/;
use List::MoreUtils qw/any/;

extends 'Script';

has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_yaml {
		my ($instance) = @_;
		my $league = $instance->league;
		LoadFile "$league/league.yaml"
}

has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $data = $self->yaml;
	$data->{league};
}

has 'hwdir' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_hwdir {
	my $self = shift;
	my $league = $self->league;
	my $data = $self->yaml;
	my $hwdir = $data->{hw} || "$league/homework"
}
has 'rounds' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_rounds {
	my $self = shift;
	my $hwdir = $self->hwdir;
	my @hw = glob "$hwdir/*.yaml";
	[ sort {$a<=>$b} map m/^$hwdir\/(\d+)\.yaml$/, @hw ];
}
has 'hw' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_hw {
	my $self = shift;
	my $hwdir = $self->hwdir;
	my $rounds = $self->rounds;
	+{ map { $_ => LoadFile "$hwdir/$_.yaml" } @$rounds };
}
has 'hwMax' => (is => 'ro', isa => 'Int', lazy => 1, default =>
				sub { shift->yaml->{hwMax} } );
has 'totalScores' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_totalScores {
	my $self = shift;
	my $hwdir = $self->hwdir;
	LoadFile "$hwdir/total.yaml";
}
has 'totalPercent' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_totalPercent {
	my $self = shift;
	my $hwdir = $self->hwdir;
	LoadFile "$hwdir/percent.yaml";
}
has 'members' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_members {
	my $self = shift;
	my $data = $self->yaml;
	my @members = sort { $a->{id} cmp $b->{id} } @{$data->{member}};
	[ map { Player->new(league => $self, id=>$_->{id},name=>$_->{name})
			} @members ];
	# $data->{member};
}

sub is_member {
	my $self = shift;
	my $id = shift;
	my $data = $self->yaml;
	any { $_->{id} eq $id } @{$data->{member}};
}

sub save {
	my $self = shift;
	DumpFile shift(), shift();
}
	


package Player;
use Moose;
extends 'League';
use List::MoreUtils qw/firstval/;

has 'league' => (is => 'ro', isa => 'League', required => 1);
has 'id' => (is => 'ro', isa => 'Str', required => 1);
has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $league = $self->league;
	my $id = $self->id;
	my $members = $league->members;
	my $name = firstval { $_->id eq $id and $_->name } @$members;
}
has 'Chinese' => (is => 'ro', isa => 'Str');
has 'grades' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_grades {
	my $self = shift;
	my $id = $self->id;
	my $hw = $self->hw;
	my $rounds = $self->rounds;
	[ map { $hw->{$_}->{$id} } @$rounds ];
}

package main;

use strict;
use warnings;

use List::Util qw/sum/;
use IO::All;
use IO::Handle;
use Cwd;
use Pod::Usage;

run() unless caller;

sub run {
	my $script = Script->new_with_options( league => getcwd );
	pod2usage(1) if $script->help;
	pod2usage(-exitstatus => 0, -verbose => 2) if $script->man;
	my $leagueId = $script->league;
	my $league = League->new( league => $leagueId ) or
		die "No $leagueId league: $!";
	my $hwMax = $league->hwMax;
	my $rounds = $league->rounds;
	my $totalMax = @$rounds * $hwMax;
	my $oldtotal = $league->totalScores;
	my $oldpercent = $league->totalPercent;
	my ($newtotal, $newpercent);
	my @formats = qw/'' WEEKS1 WEEKS2 WEEKS3 WEEKS4 WEEKS5 WEEKS6 WEEKS7
		WEEKS8 WEEKS9 WEEKS10 WEEKS11 WEEKS12 WEEKS13/;
	REP->format_name($formats[@$rounds]);
	open REP, '>-' or die 'STDOUT? $!'; 
	my $name = $league->name;
	my @romans = qw/'' 01 02 03 04 05 06 07
		08 09 10 11 12 13 14 15 16 17 18/;
	local $,=', ';
	print REP
"             $name Homework
		    Weeks: Grade(0-$hwMax)/$hwMax
Name        ID  @romans[@$rounds]   Total/$totalMax Grade/100
-------------------------------------------------------------------------
";
	my $members = $league->is_member( $script->player )? 
		[ Player->new( id => $script->player, league => $league ) ]:
		$league->members;
	foreach my $member ( @$members ) 
	{
		our $id = $member->id;
		our $name = $member->name;
		# our $indgrades = [ (2) x @$rounds ];
		our @indgrades = @{ $member->grades };
		@indgrades = map { $_ == 0.5? substr $_, 1: $_ } @indgrades;
		die "No homework grade for $name $id" if grep {not defined} @indgrades;
		our $grade = sum @indgrades;
		our $percent = 100 * $grade / $totalMax;
		$newtotal->{$id} = $grade;
		$newpercent->{$id} = $percent;
		write REP;
		# $grades->{$partner} = $newGrade;
	} 
	
	
	my $hwdir = $league->hwdir;
	my $log = io "$hwdir/hwtotal.log";
	localtime() . ": $0 run to calculate homework totals at round $rounds->[-1].\n"
										>> $log;
	$league->save("$hwdir/total.yaml.bak", $oldtotal);
	$league->save("$hwdir/total.yaml", $newtotal);
	$league->save("$hwdir/percent.yaml.bak", $oldpercent);
	$league->save("$hwdir/percent.yaml", $newpercent);
}

our ($name, $id, @indgrades, $grade, $percent);
format WEEKS1 =
@<<<<<< @<<<<<<< @<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS2 =
@<<<<<< @<<<<<<< @<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS3 =
@<<<<<< @<<<<<<< @<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS4 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS5 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS6 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS7 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS8 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS9 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS10 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS11 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS12 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS13 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.
format WEEKS14 =
@<<<<<< @<<<<<<< @<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<@<<   @<<<<<   @<<
$name,  $id,	@indgrades, $grade, $percent
.

1;

__END__

=head1 NAME

cumulative - Add player results in individual rounds to get a cumulative total and show present standing

=head1 SYNOPSIS

hwtotal [options] 

Options:

--help            This help message

--man            A man page

--league m/j	The league whose results these are

=head1 OPTIONS

=over 8

=item B<-league>

The league to which the redeemer belongs

=back

=head1 DESCRIPTION

B<hwtotal> tallies individuals' scores in the files in the hw directory recorded in league.yaml. It stores the total in cumulative.yaml, in the same directory.

=cut

