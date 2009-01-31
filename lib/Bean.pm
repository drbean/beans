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

has 'leagueId' => (is => 'ro', isa => 'Str', required => 1);
has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_yaml {
		my ($instance) = @_;
		my $league = $instance->leagueId;
		LoadFile "$league/league.yaml";
}

has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $data = $self->yaml;
	$data->{league};
}
has 'members' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_members {
	my $self = shift;
	my $data = $self->yaml;
	my @members = sort { $a->{id} cmp $b->{id} } @{$data->{member}};
	[ map { Player->new(league => $self, id=>$_->{id},name=>$_->{name})
			} @members ];
}
has 'absentees' => (is => 'ro', isa => 'ArrayRef', lazy => 1, default =>
				sub { shift->yaml->{absent} } );

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


package Homework;
use Moose;
extends 'League';
use YAML qw/LoadFile DumpFile/;

has 'hwdir' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_hwdir {
	my $self = shift;
	my $league = $self->leagueId;
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
has 'totalMax' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_totalMax {
	my $self = shift;
	my $rounds = $self->rounds;
	my $hwMax = $self->hwMax;
	$hwMax * @$rounds;
}
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

package Classwork;
use Moose;
extends 'League';
use YAML qw/LoadFile/;
use List::Util qw/max/;
use List::MoreUtils qw/any/;
use Carp;

has 'series' => (is => 'ro', isa => 'ArrayRef', lazy => 1, default =>
				sub { shift->yaml->{series} } );
has 'groupseries' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_groupseries {
	my $self = shift;
	my $series = $self->series;
	my $league = $self->leagueId;
	+{ map { $_ => LoadFile "$league/$_/groups.yaml" } @$series };
}
has 'allfiles'  => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
sub _build_allfiles {
	my $self = shift;
	my $league = $self->leagueId;
	my $series = $self->series;
	[ map { grep m|/(\d+)\.yaml$|, glob "$league/$_/*" } @$series ];
}
has 'allweeks' => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
sub _build_allweeks {
	my $self = shift;
	my $files = $self->allfiles;
	[ map { m|/(\d+)\.yaml$|; $1 } @$files ];
}
has 'lastweek' => ( is => 'ro', isa => 'Int', lazy_build => 1 );
sub _build_lastweek {
	my $self = shift;
	my $weeks = $self->allweeks;
	max @$weeks;
}
has 'data' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_data {
	my $self = shift;
	my $files = $self->allfiles;
	my $weeks = $self->allweeks;
	+{ map { $weeks->[$_] => LoadFile $files->[$_] } 0..$#$weeks };
}

sub groups {
	my $self = shift;
	my $session = shift;
	$self->groupseries->{$session};
}

sub files {
	my $self = shift;
	my $session = shift;
	my $allfiles = $self->allfiles;
	[ grep m|/$session/\d+\.yaml$|, @$allfiles ];
}

sub weeks {
	my $self = shift;
	my $session = shift;
	my $files = $self->files($session);
	[ map { m|(\d+)\.yaml$|; $1 } @$files ];
}

sub week2session {
	my $self = shift;
	my $week = shift;
	my $sessions = $self->series;
	my %sessions2weeks = map { $_ => $self->weeks($_) } @$sessions;
	while ( my ($session, $weeks) = each %sessions2weeks ) {
		return $session if any { $_ eq $week } @$weeks;
	}
	croak "Week $week in none of @$sessions sessions.\n";
}

sub names2groups {
	my $self = shift;
	my $session = shift;
	my $name = shift;
	my $groups = $self->groups($session);
	my @names; push @names, @$_ for values %$groups;
	my %names2groups;
	while ( my ($group, $names) = each %$groups ) {
		for my $name ( @$names ) {
		die "$name in $group and other group in $session session.\n"
				if $names2groups{$name};
			$names2groups{$name} = $group;
		}
	}
	\%names2groups;
}

sub name2group {
	my $self = shift;
	my $session = shift;
	my $name = shift;
	my $groups = $self->groups($session);
	my @names; push @names, @$_ for values %$groups;
	my @name2groups;
	while ( my ($group, $names) = each %$groups ) {
		push @name2groups, $group for grep /^$name$/, @$names;
	}
	die "$name not in exactly one group in $session session.\n"
				unless @name2groups == 1;
	shift @name2groups;
}

sub merits {
	my $self = shift;
	my $data = $self->data;
	my $week = shift;
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	my $card = $data->{$week};
	+{ map { $_ => $card->{$_}->{merits} } keys %$groups };
}

sub absences {
	my $self = shift;
	my $data = $self->data;
	my $week = shift;
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	my $card = $data->{$week};
	+{ map { $_ => $card->{$_}->{absences} } keys %$groups };
}

sub tardies {
	my $self = shift;
	my $data = $self->data;
	my $week = shift;
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	my $card = $data->{$week};
	+{ map { $_ => $card->{$_}->{tardies} } keys %$groups };
}

=head2 payout

How much should be given out for each week in this session, so that the total score of each player over the series averages 80?

=cut

sub payout {
	my $self = shift;
	my $session = shift;
	my $sessions = $self->series;
	my $groups = $self->groups($session);
	my $weeks = $self->weeks($session);
	my $payout = (80/@$sessions) * (keys %$groups) / @$weeks;
}

=head2 demerits

The demerits that week. calculated as twice the number of absences, plus the number of tardies. In a four-member group, this ranges from 0 to 8.

=cut

sub demerits {
	my $self = shift;
	my $week = shift;
	my $absences = $self->absences($week);
	my $tardies = $self->tardies($week);
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	+{map {$_ => ($absences->{$_} * 2 + $tardies->{$_} * 1)} keys %$groups};
}

=head2 favor

A score of 2 given to groups with no more than 6 demerits, to prevent groups who were all there but didn't do anything (ie had no merits and no demerits) from getting a log score of 0, and so getting a grade of 0 for that week.

=cut

sub favor {
	my $self = shift;
	my $week = shift;
	my $demerits = $self->demerits($week);
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	+{ map {$_ => ($demerits->{$_} < 7? 1: 0)} keys %$groups };
}

=head2 maxDemerit

The max demerit that week. achieved by the group with the most absences and tardies.

=cut

sub maxDemerit {
	my $self = shift;
	my $week = shift;
	my $demerits = $self->demerits($week);
	max( values %$demerits );
}

=head2 meritDemerit

Let groups with no merits, and no demerits get a score greater than 1, so the log score is greater than 0. Let groups with 3 or more absences and 1 tardies not be eligible for this favor, but get at least 0. Let other groups get the number of merits - number of demerits, but also be eligible for the favor, and get a score of above 1.

=cut

sub meritDemerit {
	my $self = shift;
	my $week = shift;
	my $merits = $self->merits($week);
	my $demerits = $self->demerits($week);
	my $maxDemerit = $self->maxDemerit($week);
	my $favor = $self->favor($week);
	my $session = $self->week2session($week);
	my $groups = $self->groups($session);
	+{ map {$_=> $maxDemerit+$merits->{$_}+$favor->{$_}-$demerits->{$_}}
		keys %$groups };
}

package Grades;
use Moose;
extends 'League';

has 'weights' => (is => 'ro', isa => 'HashRef', lazy => 1, required => 1,
				default => sub { shift->yaml->{weights} } );

package Player;
use Moose;
use List::MoreUtils qw/firstval/;
use List::Util qw/sum/;
use POSIX;

has 'league' => (is => 'ro', isa => 'League', required => 1);
has 'id' => (is => 'ro', isa => 'Str', required => 1);
has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $league = $self->league;
	my $id = $self->id;
	my $members = $league->members;
	my $member = firstval { $_->id eq $id } @$members;
	$member->name;
}
has 'Chinese' => (is => 'ro', isa => 'Str');
has 'hwgrades' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_hwgrades {
	my $self = shift;
	my $id = $self->id;
	my $league = $self->league;
	my $hw = $league->hw;
	my $rounds = $league->rounds;
	[ map { $hw->{$_}->{$id} } @$rounds ];
}
has 'total' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_total {
	my $self = shift;
	my $hwgrades = $self->hwgrades;
	sum @$hwgrades;
}
has 'percent' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_percent {
	my $self = shift;
	my $grade = $self->total;
	my $league = $self->league;
	my $totalMax = $league->totalMax;
	floor (100 * $grade / $totalMax);
}

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

