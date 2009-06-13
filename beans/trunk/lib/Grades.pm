package Grades;

#Last Edit: 2009  6月 13, 07時50分39秒

our $VERSION = 0.01;

use MooseX::Declare;

package Script;
use Moose;
use MooseX::Getopt;

has 'man' => (is => 'ro', isa => 'Bool');
has 'help' => (is => 'ro', isa => 'Bool');
has 'league' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'l',);
has 'weights' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'w',);
has 'player' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'p',);

package Grades;

=head1 NAME

Grades - 

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

Whether you use gold stars or token economies, How I stopped worrying and learned to love grading students. If you can't beat them, join them. For some reason, students enjoy competition outside the classroom, but less inside.
The league is the class.

Mad rush at the end of the semester to present a Excel file that will escape
=cut


=head1 ATTRIBUTES

=cut

=head2 League
=cut

class League {
	use YAML qw/LoadFile DumpFile/;
	use List::MoreUtils qw/any/;

=head3 id

Unless called from the script or web app, it's a path to the league directory.

=cut

	has 'id' => (is => 'ro', isa => 'Str', required => 1);
	has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_yaml {
			my ($instance) = @_;
			my $league = $instance->id;
			$self->inspect( "$league/league.yaml" );
	}
	has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_name {
		my $data = $self->yaml;
		$data->{league};
	}
	has 'members' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_members {
		my $data = $self->yaml;
		$data->{member};
	}
	has 'absentees' => (is => 'ro', isa => 'ArrayRef', lazy => 1, default =>
					sub { shift->yaml->{absent} } );

	method is_member (Str $id) {
		my $data = $self->yaml;
		any { $_->{id} eq $id } @{$data->{member}};
	}

	method inspect (Str $file) {
		LoadFile $file;
	}

	method save (Str $file, HashRef $data) {
		DumpFile $file, $data;
	}

}

=head2 Homework
=cut

role Homework {
	use YAML qw/LoadFile DumpFile/;
	use List::Util qw/min sum/;
	use Carp;

=head3 hwdir

The directory where the homework is.

=cut

	has 'hwdir' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_hwdir {
		my $league = $self->league->id;
		my $data = $self->league->yaml;
		my $hwdir = $data->{hw} || "$league/homework"
	}

=head3 rounds

An arrayref of the files with homework grades for players in the league in round order.

=cut

	has 'rounds' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_rounds {
		my $hwdir = $self->hwdir;
		my @hw = glob "$hwdir/*.yaml";
		[ sort {$a<=>$b} map m/^$hwdir\/(\d+)\.yaml$/, @hw ];
	}

=head3 hwbyround 

A hashref of the homework grades for players in the league for each round.

=cut

	has 'hwbyround' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_hwbyround {
		my $hwdir = $self->hwdir;
		my $rounds = $self->rounds;
		+{ map { $_ => $self->inspect( "$hwdir/$_.yaml" ) } @$rounds };
	}
	has 'roundMax' => (is => 'ro', isa => 'Int', lazy => 1, default =>
					sub { shift->league->yaml->{hwMax} } );
=head3 totalMax

The total maximum points that a Player could have gotten to this point in the whole season. There may be more (or fewer) rounds played than expected, so the actual top possible score returned by totalMax may be more (or less) than the figure planned.

=cut

	has 'totalMax' => (is => 'ro', isa => 'Int', lazy_build => 1);
	method _build_totalMax {
		my $rounds = $self->rounds;
		my $hwMax = $self->roundMax;
		$hwMax * @$rounds;
	}

	method hwforid (Str $id) {
		my $hw = $self->hwbyround;
		my $rounds = $self->rounds;
		my @hwbyid;
		for my $round ( @$rounds ) {
			if ( $hw->{$round} and defined $hw->{$round}->{$id} ) {
				push @hwbyid, $hw->{$round}->{$id};
			}
			else { warn
				"No homework result for $id in Round $round\n";}
		}
		\@hwbyid;
	}

=head3 homework

Running total homework scores of the league as percentages of the totalMax, with a maximum of 100.

=cut

	method homework {
		my $league = $self->league->id;
		my $hw = $self->hwbyround;
		my $totalMax = $self->totalMax;
		my (%idtotals, %totalcounted);
		for my $round ( keys %$hw ) {
			my %countedinround;
			for my $id ( keys %{ $hw->{$round} } ) {
				$totalcounted{$id}++;
				$countedinround{$id}++;
				carp "$id not in round $round homework" unless
					defined $hw->{$round}->{$id};
				$idtotals{$id} += $hw->{$round}->{$id};
			}
			carp "Missing/added players in $league round $round" if 
				keys %totalcounted != keys %countedinround;
		}
		+{ map { $_ => min( 100, 100 * $idtotals{$_} / $totalMax )
				|| 0 } keys %idtotals };
	}

}

=head2 Classwork
=cut

role Classwork {
	use List::Util qw/max min sum/;
	use List::MoreUtils qw/any/;
	use Carp;
	use POSIX;

	has 'series' => (is => 'ro', isa => 'ArrayRef', lazy => 1, default =>
					sub { shift->league->yaml->{series} } );
	has 'beancanseries' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_beancanseries {
		my $series = $self->series;
		my $league = $self->league->id;
		+{ map { $_ => $self->inspect( "$league/$_/beancans.yaml" ) }
			@$series };
	}

	has 'allfiles'  => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allfiles {
		my $league = $self->league->id;
		my $series = $self->series;
		my $files = [ map { grep m|/(\d+)\.yaml$|,
					glob "$league/$_/*.yaml" } @$series ];
		die "${league}'s @$series files: @$files?" unless @$files;
		return $files;
	}

	has 'allweeks' => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allweeks {
		my $files = $self->allfiles;
		my $weeks = [ map { m|/(\d+)\.yaml$|; $1 } @$files ];
		die "@$weeks" unless @$weeks;
		return $weeks;
	}

	has 'lastweek' => ( is => 'ro', isa => 'Int', lazy_build => 1 );
	method _build_lastweek {
		my $weeks = $self->allweeks;
		max @$weeks;
	}

	has 'data' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_data {
		my $files = $self->allfiles;
		my $weeks = $self->allweeks;
		+{ map { $weeks->[$_] => $self->inspect( $files->[$_] ) }
			0..$#$weeks };
	}

=head3 card

Classwork beans for each beancan for the week

=cut

	method card (Num $week) {
		my $cards = $self->data->{$week};
	}

=head3 beancans

A hashref of all the beancans in a session with the names of the members of each beancan. The number, composition and names of the beancans in each session of the series may change.
	
Players in one beancan all get the same classwork grade for that session. The beancan members may be the same as the members of the class group, who work together in class, or may be individuals. Usually in a big class, the beancans will be the same as the groups, and in a small class they will be individuals.

Rather than refactor the class to work with individuals rather than groups, and expand some methods (?) to fall back to league members if it finds them in the weekly files instead of groups, I decided to introduce another file, beancans.yaml, and change all variable and method names mentioning group to beancan.

=cut 

	method beancans (Str $session) { $self->beancanseries->{$session}; }

	method files (Str $session) {
		my $allfiles = $self->allfiles;
		[ grep m|/$session/\d+\.yaml$|, @$allfiles ];
	}

	method weeks (Str $session) {
		my $files = $self->files($session);
		[ map { m|(\d+)\.yaml$|; $1 } @$files ];
	}

=head3 week2session

	$classwork->week2session(15) # fourth

Given the name of a week, return the name of the session it is in.

=cut

	method week2session (Num $week) {
		my $sessions = $self->series;
		my %sessions2weeks = map { $_ => $self->weeks($_) } @$sessions;
		while ( my ($session, $weeks) = each %sessions2weeks ) {
			return $session if any { $_ eq $week } @$weeks;
		}
		croak "Week $week in none of @$sessions sessions.\n";
	}

=head3 names2beancans

A hashref of names of members of beancans and the beancan names they are members of.

=cut

	method names2beancans (Str $session) {
		my $beancans = $self->beancans($session);
		my %beancansreversed;
		while ( my ($beancan, $names) = each %$beancans ) {
			for my $name ( @$names ) {
			die
	"$name in $beancan beancan and other beancan in $session session.\n"
					if exists $beancansreversed{$name};
				$beancansreversed{$name} = $beancan;
			}
		}
		\%beancansreversed;
	}

	method name2beancan (Num $week, Str $name) {
		die "Week $week?" unless defined $week;
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my @names; push @names, @$_ for values %$beancans;
		my @name2beancans;
		while ( my ($beancan, $names) = each %$beancans ) {
			push @name2beancans, $beancan for grep /^$name$/, @$names;
		}
		die "$name not in exactly one beancan in $session session.\n"
					unless @name2beancans == 1;
		shift @name2beancans;
	}

=head3 beancansNotInCard

	$classwork->beancansNotInCard( $beancans, $card, 3)

Test all beancans exist in the beancans listed on the card for the week.

=cut

	method beancansNotInCard (HashRef $beancans, HashRef $card, Num $week) {
		my %common; $common{$_}++ for keys %$beancans, keys %$card;
		my @notInCard = grep { $common{$_} != 2 } keys %$beancans;
		croak "@notInCard beancans not in week $week data" if
					@notInCard;
	}

=head3 beancanDataOnCard

	$classwork->beancansNotInCard( $beancans, $card, 3)

Test all of the beancans have all the points due them for the week.

=cut

	method beancanDataOnCard (HashRef $beancans, HashRef $card, Num $week) {
		my @noData = grep
				{ my $beancan = $card->{$_};
				not defined $beancan->{merits}
				or not defined $beancan->{absences}
				or not defined $beancan->{tardies} }
				keys %$beancans;
		croak "@noData beancans missing data in week $week" if @noData;
	}

	method merits (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{merits} } keys %$beancans };
	}

	method absences (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{absences} } keys %$beancans };
	}

	method tardies (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{tardies} } keys %$beancans };
	}

=head3 payout

How much should be given out for each week in this session, so that the total score of each player over the series averages 80?

=cut

	method payout (Str $session) {
		my $sessions = $self->series;
		my $beancans = $self->beancans($session);
		my $weeks = $self->weeks($session);
		my $payout = (80/@$sessions) * (keys %$beancans) / @$weeks;
	}

=head3 demerits

The demerits that week. calculated as twice the number of absences, plus the number of tardies. In a four-member beancan, this ranges from 0 to 8.

=cut

	method demerits (Num $week) {
		my $absences = $self->absences($week);
		my $tardies = $self->tardies($week);
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		+{map {$_ => ($absences->{$_} * 2 + $tardies->{$_} * 1)} keys %$beancans};
	}

=head3 favor

A score of 1 given to beancans with no more than 6 demerits, to prevent beancans who were all there but didn't do anything (ie had no merits and no demerits) from getting a log score of 0, and so getting a grade of 0 for that week.

=cut

	method favor (Num $week) {
		my $demerits = $self->demerits($week);
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		+{ map {$_ => ($demerits->{$_} < 7? 1: 0)} keys %$beancans };
	}

=head3 maxDemerit

The max demerit that week. achieved by the beancan with the most absences and tardies.

=cut

	method maxDemerit (Num $week) {
		my $demerits = $self->demerits($week);
		max( values %$demerits );
	}

=head3 meritDemerit

Let beancans with no merits, and no demerits get a score greater than 1, so the log score is greater than 0. Let beancans with 3 or more absences and 1 tardies not be eligible for this favor, but get at least 0. Let other beancans get the number of merits - number of demerits, but also be eligible for the favor, and get a score of above 1.

=cut

	method meritDemerit (Num $week) {
		my $merits = $self->merits($week);
		my $demerits = $self->demerits($week);
		my $maxDemerit = $self->maxDemerit($week);
		my $favor = $self->favor($week);
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		+{ map {$_=> $maxDemerit+$merits->{$_}+$favor->{$_}-$demerits->{$_}}
			keys %$beancans };
	}

=head3 logwork

The points given by the teacher are log-scaled to prevent active students from taking all the payout, and the other students getting very low grades. There may be better ways of grading to the curve than using log scaling. The log of one point is 0, which results in a grade of 0 for that week for that beancan.

=cut

	method logwork (Num $week) {
		my $work = $self->meritDemerit($week);
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		+{ map { $_ => $work->{$_} == 0 ?  0 : 1 + log $work->{$_} }
			keys %$beancans };
	}

=head3 work2grades

The work (ie merits - demerits) of the beancans for the week, as a percentage of the total work, determines the payout of grades, which should average 80 over the sessions of play.

=cut

	method work2grades (Num $week) {
		my $work = $self->logwork($week);
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my $totalwork = sum values %$work;
		my $payout = $self->payout($session);
		+{ map { $_ => $totalwork == 0? 0: ( $work->{$_}*$payout/ $totalwork )
							} keys %$beancans };
	}

=head3 grades4session

Totals for beancans over the session.

=cut

	method grades4session (Str $session) {
		my $weeks = $self->weeks($session);
		my $beancans = $self->beancans($session);
		my (%sessiontotal);
		for my $week ( @$weeks ) {
			my $grade = $self->work2grades($week);
			for my $can ( keys %$beancans ) {
				carp "$can not in week $week classwork"
					unless defined $grade->{$can};
				$sessiontotal{$can} += $grade->{$can};
			}
		}
		\%sessiontotal;
	}

=head3 classwork

Running totals for individual ids out of 100, over the whole series.

=cut

	method classwork {
		my $members = $self->league->members;
		my $series = $self->series;
		my (%grades);
		for my $session ( @$series ) {
			my %presentMembers;
			my $can = $self->names2beancans($session);
			my $grade = $self->grades4session($session);
			for my $member ( @$members ) {
				my $name = $member->{name};
				my $id = $member->{id};
				my $beancan = $can->{$member->{name}};
				if ( defined $beancan ) {
					my $grade = $grade->{$can->{$name}};
					carp $member->{name} .
						" not in $session session"
						unless defined $grade;
					$grades{$id} += $grade;
				} else {
					carp $member->{name} .
					"'s beancan in $session session?"
				}
			}
		}
		for my $member ( @$members ) {
			my $id = $member->{id};
			if ( exists $grades{$id} ) {
				$grades{$id} = min( 100, $grades{$id} );
			}
			else {
				my $name = $member->{name};
				carp "$name $id classwork?";
				$grades{$id} = 0;
			}
		}
		\%grades;
	}

}

role Exams {
	use List::Util qw/sum/;
	use Carp;

	has 'examdirs' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_examdirs {
		my $leagueId = $self->league->id;
		my $examdirs = $self->league->yaml->{exams};
		[ map { "$leagueId/$_" } @$examdirs ];
	}

	has 'examMax' => (is => 'ro', isa => 'Int', lazy => 1, required => 1,
			default => sub { shift->league->yaml->{examMax} } );

=head3 examResults

A hash ref of the ids of the players and an array of their results over the exam series, ie examdirs. Die if any result is larger than examMax.

=cut

	has 'examResults' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examResults {
		my $examdirs = $self->examdirs;
		my @exams = map { $self->inspect("$_/g.yaml") } @$examdirs;
		my %ids;
		for my $exam ( @exams ) { $ids{$_}++ for keys %$exam; }
		for my $id  ( keys %ids ) {
			carp "Only $ids{$id} exam results for $id\n" unless 
					$ids{$id} == @exams;
		}
		+{ map { my $id=$_; $id => [ map { $_->{$id} } @exams ] }
			keys %ids };
	}

	has 'examPercent' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examPercent {
		my $scores = $self->examResults;
		my $max = $self->examMax;
		+{ map { my $id=$_; $id => [ map { $_*(100/$max) } @{$scores->{$_}} ] }
			keys %$scores };
	}
	has 'examGrade' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examGrade {
		my $grades = $self->examPercent;
		+{ map { my $numbers=$grades->{$_};
			$_ => sum(@$numbers)/@{$numbers} }
					keys %$grades };
	}
}

class Player {
	use List::MoreUtils qw/firstval/;
	use List::Util qw/sum/;
	use POSIX;

	has 'league' => (is => 'ro', isa => 'League', required => 1);
	has 'id' => (is => 'ro', isa => 'Str', required => 1);
	has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_name {
		my $league = $self->league;
		my $id = $self->id;
		my $members = $league->members;
		my $member = firstval { $_->id eq $id } @$members;
		$member->name;
	}

	has 'Chinese' => (is => 'ro', isa => 'Str');
	has 'total' => (is => 'ro', isa => 'Int', lazy_build => 1);
	method _build_total {
		my $hwgrades = $self->total;
		sum @$hwgrades;
	}
	has 'percent' => (is => 'ro', isa => 'Int', lazy_build => 1);
	method _build_percent {
		my $grade = $self->total;
		my $league = $self->league;
		my $totalMax = $league->totalMax;
		floor (100 * $grade / $totalMax);
	}
}

class Grades with Homework with Classwork with Exams {

	use Carp qw/croak/;

=head3 league

The league (object) whose grades these are. Must be passed when league object is created.

=cut

	has 'league' => (is =>'ro', isa => 'League', required => 1,
				handles => [ 'inspect' ] );

	has 'weights' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_weights {
		my $weights = $self->league->yaml->{weights};
		my @weights = ref $weights eq 'ARRAY' ? split m/,|\s+/, $weights:
					( $weights->{classwork},
					$weights->{homework},
					$weights->{exams} );
		\@weights;
	}

	method sprintround (Maybe[Num] $number) {
		sprintf '%.0f', $number;
	}

	method grades {
		my $members = $self->league->members;
		my $homework = $self->homework;
		my $classwork = $self->classwork;
		my $exams = $self->examGrade;
		my @ids = map { $_->{id} } @$members;
		my $weights = $self->weights;
		my %grades = map { $_ => $self->sprintround(
			$classwork->{$_} * $weights->[0] /100 +
			$homework->{$_} * $weights->[1] /100 +
			$exams->{$_}    * $weights->[2] /100 )
				} @ids;
		\%grades;
	}

}

1;

__END__
