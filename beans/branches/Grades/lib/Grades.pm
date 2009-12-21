package Grades;

#Last Edit: 2009 12月 10, 10時22分50秒

our $VERSION = 0.07;

use MooseX::Declare;

package Grades::Script;
use Moose;
with 'MooseX::Getopt';

has 'man' => (is => 'ro', isa => 'Bool');
has 'help' => (is => 'ro', isa => 'Bool');
has 'league' => (metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'l',);
has 'exam' => ( metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'e',);
has 'weights' => (metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'w',);
has 'player' => (metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'p',);

package Grades;

=head1 NAME

Grades - A collocation of homework, classwork and exams

=head1 SYNOPSIS

	use Grades;

	my $script = Grades::Script->new_with_options( league => getcwd );
	my $league = League->new( id => $script->league );
	my $grades = Grades->new( league => $league );

	my $homeworkgrades = $grades->homework;
	my $classworkgrades = $grades->classwork;
	my $examgrades = $grades->examGrade;

=head1 DESCRIPTION

An alternative to a spreadsheet for grading students, using YAML files and scripts. The students are the players in a league ( class.) See the README and example emile league in t/emile in the distribution for the layout of the league directory in which homework, classwork and exam scores are recorded.

Keywords: gold stars, token economies, bean counter

=cut

=head1 ATTRIBUTES & METHODS

=cut

=head2 LEAGUE CLASS

=cut

class League {
	use YAML qw/LoadFile DumpFile/;
	use List::MoreUtils qw/any/;
	use Grades::Types qw/PlayerName PlayerNames Members/;

=head3 id

Unless called from the script or web app, it's a path to the league directory.

=cut

	has 'id' => (is => 'ro', isa => 'Str', required => 1);

=head3 yaml

The content of the league configuration file.

=cut

	has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_yaml {
			my ($instance) = @_;
			my $league = $instance->id;
			$self->inspect( "$league/league.yaml" );
	}

=head3 name

The name of the league (class).

=cut

	has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_name {
		my $data = $self->yaml;
		$data->{league};
	}


=head3 approach

The style of classwork competition, eg compwork, or classwork (ie groupwork). This is a name of a method in the appropriate role that returns the final grades for classwork. TODO This is the name of the class to which 'classwork' and '' methods are delegated.

=cut

	has 'approach' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_approach {
		my $data = $self->yaml;
		$data->{approach};
	}

=head3 members

Hash refs of the players (students) in the league. The module assumes each of the members in the arrayref returned by this attribute is a hash ref containing an id and name of the member.

=cut

	has 'members', is => 'ro', isa => Members, lazy_build => 1;
	method _build_members {
		my $data = $self->yaml;
		$data->{member};
	}

=head3 absentees

Students who have stopped coming to class and so won't be included in classwork scoring.

=cut

	has 'absentees', (is => 'ro', isa => PlayerNames,
	    lazy => 1, default => sub { shift->yaml->{absent} } );


=head3 is_member

Whether the passed id is that of a member in the league (class).

=cut

	method is_member (Str $id) {
		my $data = $self->yaml;
		any { $_->{id} eq $id } @{$data->{member}};
	}


=head3 ided

The id of the member with the given player name.

=cut

    method ided( Str $player) {
        my $members = $self->members;
	my @ids = map { $_->{id} }
	    grep { $_->{name} =~ m/^$player$/i } @$members;
	warn @ids . " players named '$player' with ids: @ids," unless @ids==1;
	if ( @ids == 1 ) { return $ids[0] }
	else { return \@ids; }
      }

=head3 inspect

Loads a YAML file.

=cut

	method inspect (Str $file) {
		LoadFile $file;
	}

=head3 save

Dumps a YAML file

=cut

	method save (Str $file, HashRef $data) {
		DumpFile $file, $data;
	}

}


=head2	PLAYER CLASS

=cut

class Player {
	use List::MoreUtils qw/firstval/;
	use List::Util qw/sum/;
	use POSIX;

=head3 league

The league the player is in. This is required.

=cut

	has 'league' => (is => 'ro', isa => 'League', required => 1);

=head3 id

The id of the player. This is required.

=cut

	has 'id' => (is => 'ro', isa => 'Str', required => 1);

=head3 id

The name of the player.

=cut

	has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_name {
		my $league = $self->league;
		my $id = $self->id;
		my $members = $league->members;
		my $member = firstval { $_->{id} eq $id } @$members;
		$member->{name};
	}

	has 'Chinese' => (is => 'ro', isa => 'Str');
}


=head2	GRADES CLASS

=head2 Grades' Homework Methods
=cut

role Homework {
	use YAML qw/LoadFile DumpFile/;
	use List::Util qw/min sum/;
	use Carp;
    use Grades::Types qw/PlayerId HomeworkResults/;

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

An arrayref of the rounds for which there are homework grades for players in the league, in round order, of the form, [1, 3 .. 7, 9 ..].

=cut

	has 'rounds', (is => 'ro', isa => 'ArrayRef[Int]', lazy_build => 1);
	method _build_rounds {
		my $hwdir = $self->hwdir;
		my @hw = glob "$hwdir/*.yaml";
		[ sort {$a<=>$b} map m/^$hwdir\/(\d+)\.yaml$/, @hw ];
	}

=head3 roundIndex

Given a round name (ie number), returns the ordinal position in which this round was played, with the first round numbered 0. Returns undef if the round was not played.

=cut

	method roundIndex (Int $round) {
		my $rounds = $self->rounds;
		my $n = 0;
		for ( @$rounds ) {
			return $n if $_ eq $round;
			$n++;
		}
	}

=head3 roundfiles

An hashref of the files with data for the rounds for which there are homework grades for players in the league, keyed on rounds.

=cut

	has 'roundfiles', (is => 'ro', isa => 'HashRef[ArrayRef]', lazy_build => 1);
	method _build_roundfiles {
		my $hwdir = $self->hwdir;
		my @hw = glob "$hwdir/*.yaml";
		my @rounds = map m/^$hwdir\/(\d+)\.yaml$/, @hw;
		+{ map { $_ => [ glob "$hwdir/${_}*.yaml" ] } @rounds }
	}

=head3 hwbyround 

A hashref of the homework grades for players in the league for each round.

=cut

	has 'hwbyround', (is => 'ro', isa => HomeworkResults, lazy_build => 1);
	method _build_hwbyround {
		my $hwdir = $self->hwdir;
		my $rounds = $self->rounds;
		+{ map { $_ => $self->inspect( "$hwdir/$_.yaml" ) } @$rounds };
	}

=head3 roundMax

The highest possible score in the homework

=cut

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

=head3 rawscoresinRound

Given a round, returns a hashref of the raw scores for that round, keyed on the names of the exercises. These are in files in the hwdir with names of the form ^\d+[_.]\w+\.yaml$

=cut

	method rawscoresinRound (Int $round) {
		my $hwdir = $self->hwdir;
		my $files = $self->roundfiles->{$round};
		my @ex = map m/^$hwdir\/\d+([_.]\w+)\.yaml$/, @$files;
		+{ map { substr($_,1) =>
			$self->inspect( "$hwdir/$round$_.yaml" ) } @ex };
	}

=head3 hwforid

Given a player's id, returns an array ref of the player's hw scores.

=cut

	method hwforid (PlayerId  $id) {
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

=head3 hwforidasHash

Given a player's id, returns an hashref of the player's hw grades, keyed on the rounds.

=cut

	method hwforidasHash (PlayerId  $id) {
		my $hw = $self->hwforid( $id );
		my $rounds = $self->rounds;
		my %hwbyid;
		for my $i ( 0 .. $#$rounds ) {
			my $round = $rounds->[$i];
			$hwbyid{$round} = $hw->[$i];
			if ( not defined $hw->[$i] ) { warn
				"No homework result for $id in Round $round\n";}
		}
		\%hwbyid;
	}

=head3 homework

Running total homework scores of the league.

=cut

	method homework {
		my $league = $self->league->id;
		my $hw = $self->hwbyround;
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
			carp
		    "Missing/added players in $league round $round homework" if 
				keys %totalcounted != keys %countedinround;
		}
		+{ map { $_ => $idtotals{$_} || 0 } keys %idtotals };
	}

=head3 homeworkPercent

Running total homework scores of the league as percentages of the totalMax, with a maximum of 100.

=cut

	method homeworkPercent {
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
			carp
		    "Missing/added players in $league round $round homework" if 
				keys %totalcounted != keys %countedinround;
		}
		+{ map { $_ => min( 100, 100 * $idtotals{$_} / $totalMax )
				|| 0 } keys %idtotals };
	}

}


=head2 Grades' Jigsaw Methods

The jigsaw is a cooperative learning activity where all the players in a group get different information that together produces the 'big picture', and where they are each held responsible for the understanding of each of the other individual members of this big picture.

=cut

role Jigsaw {

=head3 quizfile

The file system location of the file of the given exam with the quiz questions and answers.

=cut

    method quizfile ( Str $exam ) { $self->examConfig($exam)->{file}; }

=head3 topic

The topic of the quiz in the given exam for the given group.

=cut

    method topic ( Str $exam, Str $group ) {
	my $config = $self->examConfig($exam);
	my $activity = $config->{activity}->{$group};
	my $topic = $activity->{topic};
}

=head3 form

The form of the quiz in the given exam for the given group.

=cut

    method form ( Str $exam, Str $group ) {
	my $config = $self->examConfig($exam);
	my $activity = $config->{activity}->{$group};
	my $form = $activity->{form};
}

=head3 quiz

The quiz questions (as an anon array) in the given exam for the given group.

=cut

    method quiz ( Str $exam, Str $group ) {
	my $file = $self->examConfig($exam)->{file};
	my $activity = $self->inspect( $file );
	my $topic = $self->topic( $exam, $group );
	my $form = $self->form( $exam, $group );
	my $quiz = $activity->{$topic}->{$form}->{quiz};
    }

=head3 qn

The number of questions in the given exam for the given group.

=cut

    method qn ( Str $exam, Str $group ) {
	my $quiz = $self->quiz( $exam, $group );
	return scalar @$quiz;
    }

=head3 idsbyRole

Ids in array, in A-D role order

=cut


    method idsbyRole ( Str $exam, Str $group ) {
	my $members = $self->league->members;
	my %namedMembers = map { $_->{name} => $_ } @$members;
	my $namesbyRole = $self->examGroupMembers( $exam, $group );
	my @idsbyRole;
	for my $role ( sort keys %$namesbyRole ) {
		my $id = $namedMembers{ $namesbyRole->{$role} }->{id};
		push @idsbyRole, $id;
	}
	return \@idsbyRole;
    }

=head3 responses

The responses of the members of the given group in the given exam (as an anon hash keyed on the ids of the members). In a file in the exam directory called 'response.yaml'.

=cut


    method responses ( Str $exam, Str $group ) {
	my $examdir = $self->examdir( $exam );
	my $responses = $self->inspect( "$examdir/response.yaml" );
	return $responses->{$group};
    }

}


=head2 Grades' CompComp Methods

The comprehension question competition is a Swiss tournament regulated 2-partner conversation competition where players try to understand more of their opponent's information than their partners understand of theirs.

=cut

role CompComp {

=head3 conversations

The topics of the conversations in order.

=cut

    has 'conversations' => ( is => 'ro', isa => 'ArrayRef[Str]',
	lazy => 1, default => sub { shift->league->yaml->{conversations} } );

=head3 opponents

The opponents of the players in the given conversation.

=cut

    method opponents ( Str $round ) {
	my $league = $self->league->id;
	my $file = "$league/$round/opponent.yaml";
	my $opponents = $self->inspect( $file );
}


=head3 correct

The number of questions correct in the given conversation.

=cut

    method correct ( Str $round ) {
	my $league = $self->league->id;
	my $file = "$league/$round/correct.yaml";
	my $correct = $self->inspect( $file );
}


=head3 points

The points of the players in the given conversation.

=cut

    method points ( Str $round ) {
	my $opponents = $self->opponents( $round );
	my $correct = $self->correct( $round );
	my $points;
	for my $player ( keys %$opponents ) {
	    if ( $player =~ m/bye/i ) {
		my $byer = $opponents->{$player};
		$points->{$byer} = 5;
		next;
	    }
	    if ( $player =~ m/late/i ) {
		my $unpaired = $opponents->{$player};
		for my $unpaired ( @$unpaired ) {
		    $points->{$unpaired} = 1;
		}
		next;
	    }
	    if ( $opponents->{$player} =~ m/unpaired/i ) {
		$points->{$player} = 0;
		next;
	    }
	    if ( $player =~ m/transfer/i ) {
		my $transfer = $opponents->{$player};
		for my $transfer ( @$transfer ) {
		    my $leagueId = $transfer->{league};
		    my $player = $transfer->{player};
		    my $oldleague = League->new( id => $leagueId );
		    my $oldgrades = Grades->new( league => $oldleague );
		    $points->{$player} = $oldgrades->points($round)->{$player};
		}
		next;
	    }
	    my $opponent = $opponents->{$player};
	    my $opponentopponent = $opponents->{$opponent};
	my $test = $opponent and $opponentopponent and $player eq $opponentopponent
		    or $opponent =~ m/unpaired/ and not $opponentopponent;
	    die "$test test: ${player}'s opponent is $opponent, but
		${opponent}'s opponent is $opponentopponent" unless
		$opponent and $opponentopponent and $player eq $opponentopponent
		    or $opponent eq 'Unpaired' and not $opponentopponent;
	    die "No $player quiz card?" unless exists $correct->{$player};
	    my $ourcorrect = $correct->{$player};
	    die "No $opponent card against $player?" unless
		exists $correct->{$opponent};
	    my $theircorrect = $correct->{$opponent};
	    if ( not defined $ourcorrect ) {
		$points->{$player} = 0;
		next;
	    }
	    if ( not defined $theircorrect ) {
		$points->{$player} = 5;
		next;
	    }
	    $points->{$player} = $ourcorrect > $theircorrect? 5:
				$ourcorrect < $theircorrect? 3: 4
	}
	return $points;
    }


=head3 totalcomp

The total over the conversations over the series.

=cut

    method totalcomp {
	my $rounds = $self->conversations;
	my $members = $self->league->members;
	my @ids = map { $_->{id} } @$members;
	my $totals;
	@$totals{ @ids } = (0) x @ids;
	for my $round ( @$rounds ) {
	    my $points = $self->points( $round );
	    for my $id ( @ids ) {
		    next unless defined $points->{$id};
		$totals->{$id} += $points->{$id};
	    }
	}
	return $totals;
    }


=head3 compwork

The total over the conversations over the series expressed as a percentage of the possible score. The average should be 80 percent if every player participates in every comp.

=cut

    method compwork {
	my $rounds = $self->conversations;
	my $n = @$rounds;
	my $totals = $self->totalcomp;
	my %percentages = map { $_ => $totals->{$_} * 100 / (5*$n) } keys %$totals;
	return \%percentages;
    }
}


=head2 Grades' Classwork Methods
=cut

role Classwork {
	use List::Util qw/max min sum/;
	use List::MoreUtils qw/any/;
	use Carp;
	use POSIX;
	use Grades::Types qw/Beancans Card/;

=head3 series

The sessions over the series (semester) in which there was a different grouping (beancans) of players. Everyone in the same beancan for one session gets the same number of beans (classwork score.)

=cut

	has 'series' => (is => 'ro', isa => 'ArrayRef[Str]',
	    lazy => 1, default => sub { shift->league->yaml->{series} } );

=head3 beancanseries

The different beancans for each of the sessions in the series. In the directory for each session of the series, there is a file called beancans.yaml, containing mappings of a beancan name to a sequence of PlayerNames, the members of the beancan.

=cut
	has 'beancanseries' => (is => 'ro', isa => Beancans, lazy_build => 1);
	method _build_beancanseries {
		my $series = $self->series;
		my $league = $self->league->id;
		+{ map { $_ => $self->inspect( "$league/$_/beancans.yaml" ) }
			@$series };
	}


=head3 allfiles

The files containing classwork points (beans) awarded to beancans. 

=cut


	has 'allfiles'  => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allfiles {
		my $league = $self->league->id;
		my $series = $self->series;
		my $files = [ map { grep m|/(\d+)\.yaml$|,
					glob "$league/$_/*.yaml" } @$series ];
		croak "${league}'s @$series files: @$files?" unless @$files;
		return $files;
	}

=head3 allweeks

The weeks (an array ref of integers) in which beans were awarded.

=cut

	has 'allweeks' => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allweeks {
		my $files = $self->allfiles;
		my $weeks = [ map { m|/(\d+)\.yaml$|; $1 } @$files ];
		croak "No classwork weeks: @$weeks" unless @$weeks;
		return $weeks;
	}

=head3 lastweek

The last week in which beans were awarded.

=cut

	has 'lastweek' => ( is => 'ro', isa => 'Int', lazy_build => 1 );
	method _build_lastweek {
		my $weeks = $self->allweeks;
		max @$weeks;
	}

=head3 data

The beans awarded to the beancans in the individual cards over the weeks of the series (semester.)

=cut

	has 'data' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_data {
		my $files = $self->allfiles;
		my $weeks = $self->allweeks;
		+{ map { $weeks->[$_] => $self->inspect( $files->[$_] ) }
			0..$#$weeks };
	}

=head3 card

Classwork beans for each beancan for the given week

=cut

	method card (Num $week) {
		my $card = $self->data->{$week};
		croak "Week $week card probably has undefined or non-numeric Merit, Absence, Tardy scores, or possibly illegal beancan."
		    unless is_Card( $card );
		return $card;
	}

=head3 beancans

A hashref of all the beancans in a given session with the names of the members of each beancan. The number, composition and names of the beancans may change from one session of the series to the next.
	
Players in one beancan all get the same classwork grade for that session. The beancan members may be the same as the members of the class group, who work together in class, or may be individuals. Usually in a big class, the beancans will be the same as the groups, and in a small class they will be individuals.

Players in the 'Absent' beancan all get a grade of 0 for the session.

Rather than refactor the class to work with individuals rather than groups, and expand some methods (?) to fall back to league members if it finds them in the weekly files instead of groups, I decided to introduce another file, beancans.yaml, and change all variable and method names mentioning group to beancan.

=cut 

	method beancans (Str $session) { $self->beancanseries->{$session}; }

=head3 active

Given a session, returns the active beancans, ie all but the 'Absent' beancan.

=cut

	method active (Str $session) {
		my $beancans = $self->beancans($session);
		my %active = %$beancans;
		delete $active{Absent};
		return \%active;
	}

=head3 files

Given a session, returns the files containing beans for the session of form, $session/\d+\.yaml$

=cut

	method files (Str $session) {
		my $allfiles = $self->allfiles;
		[ grep m|/$session/\d+\.yaml$|, @$allfiles ];
	}

=head3 weeks

Given a session, returns the weeks (an array ref of integers) in which beans were awarded in the session.

=cut

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

A hashref of names of members of beancans (players) and the beancans they were members of in a given session.

=cut

	method names2beancans (Str $session) {
		my $beancans = $self->beancans($session);
		my %beancansreversed;
		while ( my ($beancan, $names) = each %$beancans ) {
			for my $name ( @$names ) {
			croak
	"$name in $beancan beancan and other beancan in $session session.\n"
					if exists $beancansreversed{$name};
				$beancansreversed{$name} = $beancan;
			}
		}
		\%beancansreversed;
	}

=head3 name2beancan

	$classwork->name2beancan( $week, $playername )

Given the name of a player, the name of the beancan they were a member of in the given week.

=cut

	method name2beancan (Num $week, Str $name) {
		croak "Week $week?" unless defined $week;
		my $session = $self->week2session($week);
		my $beancans = $self->beancans($session);
		my @names; push @names, @$_ for values %$beancans;
		my @name2beancans;
		while ( my ($beancan, $names) = each %$beancans ) {
			push @name2beancans, $beancan for grep /^$name$/, @$names;
		}
		croak "$name not in exactly one beancan in $session session.\n"
					unless @name2beancans == 1;
		shift @name2beancans;
	}

=head3 beancansNotInCard

	$classwork->beancansNotInCard( $beancans, $card, 3)

Test all beancans, except Absent, exist in the beancans listed on the card for the week.

=cut

	method beancansNotInCard (HashRef $beancans, HashRef $card, Num $week) {
		my %common; $common{$_}++ for keys %$beancans, keys %$card;
		my @notInCard = grep { $common{$_} != 2 and $_ ne 'Absent' }
						keys %$beancans;
		croak "@notInCard beancans not in week $week data" if
					@notInCard;
	}

=head3 beancanDataOnCard

	$classwork->beancansNotInCard( $beancans, $card, 3)

Test all of the beancans, except Absent, have all the points due them for the week. Duplicates the check done by the Card type.

=cut

	method beancanDataOnCard (HashRef $beancans, HashRef $card, Num $week) {
		my @noData = grep { my $beancan = $card->{$_};
				$_ ne 'Absent' and ( 
					not defined $beancan->{merits}
					or not defined $beancan->{absences}
					or not defined $beancan->{tardies}  ) }
				keys %$beancans;
		croak "@noData beancans missing data in week $week" if @noData;
	}

=head3 merits

The points the beancans gained for the given week.

=cut

	method merits (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{merits} } keys %$beancans };
	}

=head3 absences

The numbers of players absent from the beancans in the given week. These are demerits.

=cut

	method absences (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{absences} } keys %$beancans };
	}

=head3 tardies

The numbers of players not on time in the beancans in the given week. These are demerits.

=cut

	method tardies (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{tardies} } keys %$beancans };
	}

=head3 payout

How much should be given out for each beancan (except the 'Absent' beancan) for each week in this session, so that the total score of each player over the session averages 80?

=cut

	method payout (Str $session) {
		my $sessions = $self->series;
		my $beancans = $self->active($session);
		my $weeks = $self->weeks($session);
		my $payout = (80/@$sessions) * (keys %$beancans ) / @$weeks;
	}

=head3 demerits

The demerits that week. calculated as twice the number of absences, plus the number of tardies. In a four-member beancan, this ranges from 0 to 8.

=cut

	method demerits (Num $week) {
		my $absences = $self->absences($week);
		my $tardies = $self->tardies($week);
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		+{map {$_ => ($absences->{$_} * 2 + $tardies->{$_} * 1)} keys %$beancans};
	}

=head3 favor

A score of 1 given to beancans with no more than 6 demerits, to prevent beancans who were all there but didn't do anything (ie had no merits and no demerits) from getting a log score of 0, and so getting a grade of 0 for that week.

=cut

	method favor (Num $week) {
		my $demerits = $self->demerits($week);
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
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
		my $beancans = $self->active($session);
		+{ map {$_=> $merits->{$_} + $favor->{$_} +
				$maxDemerit - $demerits->{$_}}
			keys %$beancans };
	}

=head3 logwork

The points given by the teacher are log-scaled to prevent active students from taking all the payout, and the other students getting very low grades. There may be better ways of grading to the curve than using log scaling. The log of one point is 0, which results in a grade of 0 for that week for that beancan.

=cut

	method logwork (Num $week) {
		my $work = $self->meritDemerit($week);
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		+{ map { $_ => $work->{$_} == 0 ?  0 : 1 + log $work->{$_} }
			keys %$beancans };
	}

=head3 work2grades

The work (ie merits - demerits) of the individual beancans for the week, as a percentage of the total work of all the beancans, determines the payout of grades, which should average 80 over the sessions of play. I was logscaling grades. I am now not doing that.

=cut

	method work2grades (Num $week) {
		# my $work = $self->logwork($week);
		my $work = $self->meritDemerit($week);
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		my $totalwork = sum values %$work;
		my $payout = $self->payout($session);
		my %grades = map { $_ => $totalwork == 0? 0:
					( $work->{$_}*$payout/ $totalwork )
							} keys %$beancans;
		$grades{Absent} = 0;
		return \%grades;
	}

=head3 grades4session

Totals for the beancans over the given session. TODO Why '+=' in sessiontotal?

=cut

	method grades4session (Str $session) {
		my $weeks = $self->weeks($session);
		my $beancans = $self->beancans($session);
		my (%sessiontotal);
		for my $week ( @$weeks ) {
			my $grade = $self->work2grades($week);
			for my $can ( keys %$beancans ) {
				if ( $can =~ m/absent/i ) {
					$sessiontotal{$can} = 0;
					next;
				}
				carp "$can not in week $week classwork"
					unless defined $grade->{$can};
				$sessiontotal{$can} += $grade->{$can};
			}
		}
		\%sessiontotal;
	}

=head3 groupwork

Running totals for individual ids out of 100, over the whole series.

=cut

	method groupwork {
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

=head2 Grades' Exams Methods
=cut

role Exams {
	use List::Util qw/max sum/;
	use List::MoreUtils qw/any/;
	use Carp;
	use Grades::Types qw/Exam/;

=head3 examids

The ids of the exams, as specified as a sequence in 'league.yaml'.

=cut

	has 'examids' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_examids {
		my $leagueId = $self->league->id;
		my $examids = $self->league->yaml->{exams};
	}

=head3 examdir

The directory in which results for the given exam exist.

=cut

	method examdir ( Str $exam ) {
		my $leagueId = $self->league->id;
		return "$leagueId/$exam";
	}

=head3 examdirs

The directories in which exam results exist, returned as an array ref.

=cut

	has 'examdirs' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_examdirs {
		my $leagueId = $self->league->id;
		my $exams = $self->league->yaml->{exams};
		[ map { $self->examdir($_) } @$exams ];
	}

=head3 examsubdirs

The directories in which exam results exist in subdirs of those directories, returned as an array ref of array refs.

=cut

	has 'examsubdirs' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
	method _build_examsubdirs {
		my $leagueId = $self->league->id;
		my $examdirs = $self->league->yaml->{exams};
		my @dirs;
		for my $dir ( @$examdirs ) {
			if ( ref $dir eq 'ARRAY' ) {
				my @subdirs = map { "$leagueId/$dir/$_" } @$dir;
				push @dirs, \@subdirs;
			}
			else { push @dirs, "$leagueId/$_"; }
		}
		return \@dirs;
	}

=head3 examMax

The maximum score possible in each individual exam. That is, what the exam is out of.

=cut

	has 'examMax' => (is => 'ro', isa => 'Int', lazy => 1, required => 1,
			default => sub { shift->league->yaml->{examMax} } );

=head3 examResults

A hash ref of the ids of the players and arrays of their results over the exam series, ie examdirs, in files named 'g.yaml'. Croak if any result is larger than examMax.

=cut

	has 'examResults' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examResults {
		my $examdirs = $self->examdirs;
		my @exams = map { $self->inspect("$_/g.yaml") } @$examdirs;
		my $max = $self->examMax;
		my %ids;
		for my $n ( 0 .. $#exams ) {
		    croak "Exam " . ++$n . " probably has undefined or non-numeric Exam scores, or possibly illegal PlayerIds."
			    unless is_Exam( $exams[$n] );
		    $ids{$_}++ for keys %{$exams[$n]};
		}
		my %results;
		for my $id  ( keys %ids ) {
			carp "Only $ids{$id} exam results for $id\n" unless 
					$ids{$id} == @exams;
			$results{$id} = [ map { $_->{$id} } @exams ];
			my $personalMax = max( @{ $results{$id} } );
			croak "${id}'s $personalMax greater than exam max, $max"
				if $personalMax > $max;
		}
		return \%results;
	}

=head3 examResultHash

A hash ref of the ids of the players and hashrefs of their results for each exam. Croak if any result is larger than examMax.

=cut

	has 'examResultHash' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examResultHash {
		my $examids = $self->examids;
		my $examResults = $self->examResults;
		my %examResults;
		for my $id ( keys %$examResults ) {
			my $results = $examResults->{$id};
			my %results;
			@results{@$examids} = @$results;
			$examResults{$id} = \%results;
		}
		return \%examResults;
	}

=head3 examResultsasPercent

A hashref of the ids of the players and arrays of their results over the exams expressed as percentages of the maximum possible score for the exams.

=cut

	has 'examResultsasPercent' => (is=>'ro', isa=>'HashRef', lazy_build=>1);
	method _build_examResultsasPercent {
		my $scores = $self->examResults;
		my $max = $self->examMax;
		+{ map { my $id=$_; $id => [ map { $_*(100/$max) } @{$scores->{$_}} ] }
			keys %$scores };
	}

=head3 examGrade

A hash ref of the ids of the players and their total scores on exams.

=cut

	has 'examGrade' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examGrade {
		my $grades = $self->examResults;
		+{ map { my $numbers=$grades->{$_};
			$_ => sum(@$numbers) }
					keys %$grades };
	}

=head3 examPercent

A hash ref of the ids of the players and their total score on exams, expressed as a percentage of the possible exam score. This is the average of their exam scores.

=cut

	has 'examPercent' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_examPercent {
		my $grades = $self->examResultsasPercent;
		+{ map { my $numbers=$grades->{$_};
			$_ => sum(@$numbers)/@{$numbers} }
					keys %$grades };
	}

=head3 examConfig

The round.yaml file with data about the given (sub)exam.

=cut

	method examConfig (Str $examId) {
		my $leagueId = $self->league->id;
		my $examdir = "$leagueId/$examId";
		my $round = $self->inspect( "$examdir/round.yaml" );
	}

=head3 examGroups

A hash ref of all the groups in the exam and the names of members of the groups, keyed on groupnames. There may be duplicated names if one player did the exam twice as an 'assistant' for a group with not enough players, and missing names if a player did not do the exam.

=cut

	method examGroups (Str $examId) {
		my $round = $self->examConfig( $examId );
		$round->{group};
	}

=head3 roles

At the moment, just A .. D.

=cut

	has 'roles' => (is => 'ro', isa => 'ArrayRef[Str]',
	    default => sub { [ qw/A B C D/ ] } );


=head3 assistants

A array ref of all the players in the (sub)exam who did it twice to 'assist' groups with not enough (or absent) players, or individuals with no groups, or to do exams with people who arrived late.

=cut

	method assistants (Str $examId) {
		my $round = $self->examConfig( $examId );
		$round->{assistants};
	}

=head3 examGroupMembers

An hash ref of the names of the members of the given group in the given exam, keyed on the roles, A..D.

=cut

	method examGroupMembers (Str $examId, Str $group) {
		my $groups = $self->examGroups( $examId );
		my $members = $groups->{$group};
	}

=head3 examGroupRole

An hash ref of the roles of the members of the given group in the given exam, keyed on the name of the player.

=cut

	method examGroupRole (Str $examId, Str $group) {
		my $members = $self->examGroupMembers( $examId, $group );
		my %roles = reverse %$members;
		return \%roles;
	}

=head3 id2examGroupRole

An hash ref of the roles of the members of the given group in the given exam, keyed on the id of the player.

=cut

	method id2examGroupRole (Str $examId, Str $group) {
		my $member = $self->examGroupMembers( $examId, $group );
		my %idedroles = map { $self->league->ided($member->{$_}) => $_ }
						keys %$member;
		return \%idedroles;
	}

=head3 name2examGroup

An array ref of the group(s) to which the given name belonged in the given exam. Normally, the array ref has only one element. But if the player was an assistant an array ref of more than one group is returned. If the player did not do the exam, no groups are returned.

=cut

	method name2examGroup (Str $examId, Str $name) {
		my $groups = $self->examGroups( $examId );
		my @memberships;
		for my $id ( keys %$groups ) {
			my $group = $groups->{$id};
			my @members = values %$group;
			push @memberships, $id if any { $_ eq $name } @members;
		}
		return \@memberships;
	}

=head3 rawExamScores

TODO

=cut

	method rawExamScores (Str $examId, Str $group) {
		my $leagueId = $self->league->id;
		my $examdir = "$leagueId/$examId";
		my $data = $self->inspect( "$examdir/scores.yaml" );
		my $groupdata = $data->{letters}->{$group};
		my $ids = $self->idsbyRole( $examId, $group );
		my @roles = grep { my $id = $_; any { $_ eq $id } @$ids } keys %$groupdata;
		my %scores;
		@scores{@roles} = @{$groupdata}{@roles};
		return \%scores;
	}

}


=head2 Grades' Core Methods
=cut

class Grades with Homework with CompComp with Classwork with Exams with Jigsaw {

	use Carp;
	use Grades::Types qw/Weights/;

=head3 league

The league (object) whose grades these are.

=cut

	has 'league' => (is =>'ro', isa => 'League', required => 1,
				handles => [ 'inspect' ] );


=head3 weights

An hash ref of the weights (expressed as a percentage) accorded to the three components, classwork, homework, and exams in the final grade.

=cut

	has 'weights' => (is => 'ro', isa => Weights, lazy_build => 1 );
	method _build_weights { my $weights = $self->league->yaml->{weights}; }


=head3 sprintround

sprintf( '%.0f', $number). sprintf warns if $number is undef.

=cut

	method sprintround (Maybe[Num] $number) {
		sprintf '%.0f', $number;
	}

=head3 grades

A hashref of student ids and final grades.

=cut

	method grades {
		my $league = $self->league;
		my $members = $league->members;
		my $homework = $self->homework;
		my $classcomponent = $league->approach;
		my $classwork = $self->$classcomponent;
		my $exams = $self->examPercent;
		my @ids = map { $_->{id} } @$members;
		my $weights = $self->weights;
		my %grades = map { $_ => $self->sprintround(
			$classwork->{$_} * $weights->{classwork} /100 +
			$homework->{$_} * $weights->{homework} /100 +
			$exams->{$_}    * $weights->{exams} /100 )
				} @ids;
		\%grades;
	}

}

no Moose;

__PACKAGE__->meta->make_immutable;

1;    # End of Grades

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-grades at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Grades>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Grades

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Grades>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Grades>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Grades>

=item * Search CPAN

L<http://search.cpan.org/dist/Grades>

=back

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut


# vim: set ts=8 sts=4 sw=4 noet:
__END__
