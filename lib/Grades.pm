package Grades;

#Last Edit: 2010 12月 04, 19時28分21秒
#$Id$

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

has 'round' => ( metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'r',);

# letters2score.pl
has 'exercise' => ( metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'x',);
has 'one' => ( metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 'o',);
has 'two' => ( metaclass => 'Getopt', is => 'ro', isa => 'Str',
		cmd_flag => 't',);

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

	$league->approach->meta->apply( $grades );
	my $classworkgrades = $grades->classwork;
	my $homeworkgrades = $grades->homework;
	my $examgrades = $grades->examGrade;

=head1 DESCRIPTION

An alternative to a spreadsheet for grading students, using YAML files and scripts. The students are the players in a league ( class.) See the README and example emile league in t/emile in the distribution for the layout of the league directory in which homework, classwork and exam scores are recorded.

Grades are a collocation of Classwork, Homework and Exams roles, but the Classwork role 'delegates' its methods to one of a number of approaches, each of which has a 'total' and 'totalPercent' method. Current approaches, or forms of curriculum, include Compcomp, Groupwork and Jigsaw.

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
	use Try::Tiny;
	use Carp;

=head3 leagues

The path to the league directory.

=cut

	has 'leagues' => (is => 'ro', isa => 'Str', required => 1, lazy => 1,
	    default => '/home/drbean/class' );

=head3 id

Actually, it's a path to the league directory, below the $grades->leagues dir.

=cut

	has 'id' => (is => 'ro', isa => 'Str', required => 1);

=head3 yaml

The content of the league configuration file.

=cut

	has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_yaml {
			my $leaguedirs = $self->leagues;
			my $league = $self->id;
			$self->inspect( "$leaguedirs/$league/league.yaml" );
	}

=head3 name

The name of the league (class).

=cut

	has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_name {
		my $data = $self->yaml;
		$data->{league};
	}


=head3 field

The field of the league (class). What is the subject or description, the area of endeavor?

=cut

	has 'field' => (is => 'ro', isa => 'Str', lazy_build => 1);
	method _build_field {
		my $data = $self->yaml;
		$data->{field};
	}


=head3 approach

The style of classwork competition, eg Compcomp, or Groupwork. This is the name of the class (think OOP) to which 'classwork' and other methods are delegated.

=cut

	has 'approach' => (is => 'ro', isa => 'Str', lazy => 1,
	    default => sub { shift->yaml->{approach} } );

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


=head3 transfer

    $oldleague = $newleague->transfer->{93}

Players who have transferred to this league from some other league at some point and the leagues they transferred from.

=cut

	has 'transfer', (is => 'ro', isa => 'HashRef',
	    lazy => 1, default => sub { shift->yaml->{transfer} } );


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
	my %ids = map { $_->{name} => $_->{id} }
	    grep { $_->{name} =~ m/^$player$/i } @$members;
	my @names = keys %ids;
	my @ids = values %ids;
	local $" = ', ';
	carp @ids . " players named @names, with ids: @ids," unless @ids==1;
	if ( @ids == 1 ) { return $ids[0] }
	else { return $ids{$player}; }
      }

=head3 inspect

Loads a YAML file.

=cut

    method inspect (Str $file) {
	my ($warning, $data);
	try { $data = LoadFile $file }
	    catch { carp "Couldn't open $file," };
	return $data;
	}

=head3 save

Dumps a YAML file

=cut

    method save (Str $file, HashRef $data) {
	try { DumpFile $file, $data }
	    catch { warn "Couldn't save $data to $file," };
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


=head2 NONENTITY CLASS

=cut 

class Nonentity extends Player {

=head3 name

The name is 'Bye'. The id is too, as a matter of fact.

=cut

    has 'name' => (is => 'ro', isa => 'Str', required => 1 );

}


=head2	GRADES CLASS

=head2 Grades' Homework Methods
=cut

role Homework {
	use YAML qw/LoadFile DumpFile/;
	use List::Util qw/min sum/;
	use Scalar::Util qw/looks_like_number/;
	use Carp;
    use Grades::Types qw/PlayerId HomeworkResult HomeworkRound HomeworkRounds
	RoundsResults/;

=head3 hwdir

The directory where the homework is.

=cut

    has 'hwdir' => (is => 'ro', isa => 'Str', lazy_build => 1);
    method _build_hwdir {
	my $league = $self->league->id;
	my $leaguedir = $self->league->leagues . "/" . $league;
	my $basename = shift->league->yaml->{hw} || "exams";
	my $hwdir = $leaguedir . '/' . $basename;
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

	has 'hwbyround', (is => 'ro', isa => RoundsResults, lazy_build => 1);
	method _build_hwbyround {
		my $hwdir = $self->hwdir;
		my $rounds = $self->rounds;
		my %results =
		    map { $_ => $self->inspect("$hwdir/$_.yaml") } @$rounds;
		my %grades = map { $_ => $results{$_}{grade} } @$rounds;
		return \%grades;
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
		my @ex = map m/^$hwdir\/$round([_.]\w+)\.yaml$/, @$files;
		my $results = $self->inspect("$hwdir/$round.yaml");
		return { $results->{exercise} => $results->{points} };
	}

=head3 hwforid

Given a player's id, returns an array ref of the player's hw scores.

=cut

    method hwforid( PlayerId $id) {
	my $leagueId = $self->league->id;
        my $hw       = $self->hwbyround;
        my $rounds = $self->rounds;
        my @hwbyid;
        for my $round (@$rounds) {
            unless ( $hw->{$round} ) {
                warn "No homework results in Round $round in $leagueId league";
                next;
            }
            my $grade = $hw->{$round}->{$id};
	    if ( defined $grade and looks_like_number( $grade ) ) {
                push @hwbyid, $grade;
            }
            elsif ( defined $grade and $grade =~ m/transfer/i ) {
                my $oldleagueId = $self->league->transfer->{$id};
                my $league   = League->new( id => $oldleagueId );
                my $grades   = Grades->new({ league => $league });
                my $transfergrade    = $grades->hwbyround->{$round}->{$id};
                warn
"$id transfered from $oldleagueId league but no homework there in round $round"
                  unless defined $transfergrade;
                push @hwbyid, $transfergrade || 0;
            }
            else {
	warn "No homework result for $id in Round $round in $leagueId league\n";
            }
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
		my $league = $self->league;
		my $leagueId = $league->id;
		my $players = $league->members;
		my %players = map { $_->{id} => $_ } @$players;
		my %idtotals;
		for my $player ( keys %players ) {
		    my $homework = $self->hwforid( $player );
		    my $total = sum @$homework;
		    $idtotals{$player} = $total;
		}
		+{ map { $_ => $idtotals{$_} || 0 } keys %idtotals };
	}

=head3 homeworkPercent

Running total homework scores of the league as percentages of the totalMax to that point, with a maximum of 100.

=cut

	method homeworkPercent {
		my $league = $self->league->id;
		my $totalMax = $self->totalMax;
		my $idtotals = $self->homework;
		my %percent = map {
		    $_ => min( 100, 100 * $idtotals->{$_} / $totalMax )
				|| 0 } keys %$idtotals;
		return \%percent;
	}

}


=head2 Grades' Jigsaw Methods

The jigsaw is a cooperative learning activity where all the players in a group get different information that together produces the 'big picture', and where they are each held responsible for the understanding of each of the other individual members of this big picture.

=cut

role Jigsaw {
    use List::MoreUtils qw/any all/;
    use Try::Tiny;
    use Moose::Autobox;

=head3 jigsawdirs

The directory where the jigsaws are.

=cut

    has 'jigsawdirs' => (is => 'ro', isa => 'Str', lazy_build => 1);
    method _build_jigsawdirs {
	my $league = $self->league->id;
	my $leaguedir = $self->league->leagues . "/" . $league;
	my $basename = shift->league->yaml->{jigsaw} || "exam";
	my $jigsawdir = $leaguedir .'/' . $basename;
	}

=head3 config

The round.yaml file with data about the jigsaw activity in the given round (directory.)

=cut

    method config( Str $round) {
	my $jigsaws = $self->jigsawdirs;
        my $config;
	try { $config = $self->inspect("$jigsaws/$round/round.yaml") }
	    catch { warn "No config file for $jigsaws/$round jigsaw" };
	return $config;
    }

=head3 topic

The topic of the quiz in the given jigsaw for the given group.

=cut

    method topic ( Str $jigsaw, Str $group ) {
	my $config = $self->config('Jigsaw', $jigsaw);
	my $activity = $config->{activity};
	for my $topic ( keys %$activity ) {
	    my $forms = $activity->{$topic};
	    for my $form ( keys %$forms ) {
		my $tables = $forms->{$form};
		return $topic if any { $_ eq $group } @$tables;
	    }
	}
	return;
}

=head3 form

The form of the quiz in the given jigsaw for the given group.

=cut

    method form ( Str $jigsaw, Str $group ) {
	my $config = $self->config('Jigsaw', $jigsaw);
	my $activity = $config->{activity};
	for my $topic ( keys %$activity ) {
	    my $forms = $activity->{$topic};
	    for my $form ( keys %$forms ) {
		my $tables = $forms->{$form};
		return $form if any { $_ eq $group } @$tables;
	    }
	}
	return;
    }

=head3 quizfile

The file system location of the file with the quiz questions and answers for the given jigsaw.

=cut

    method quizfile ( Str $jigsaw ) {
	my $config = $self->config('Jigsaw', $jigsaw);
	return $config->{text};
    }

=head3 quiz

The quiz questions (as an anon array) in the given jigsaw for the given group.

=cut

    method quiz ( Str $jigsaw, Str $group ) {
	my $quizfile = $self->quizfile($jigsaw);
	my $activity;
	try { $activity = $self->inspect( $quizfile ) }
	    catch { warn "No $quizfile jigsaw content file" };
	my $topic = $self->topic( $jigsaw, $group );
	my $form = $self->form( $jigsaw, $group );
	my $quiz = $activity->{$topic}->{jigsaw}->{$form}->{quiz};
    }

=head3 options

    $grades->options( '2/1', 'Purple', 0 ) # [ qw/Deborah Don Dovonna Sue/ ]

The options (as an anon array) to the given question in the given jigsaw for the given group.

=cut

    method options ( Str $jigsaw, Str $group, Int $question ) {
	my $quiz = $self->quiz( $jigsaw, $group );
	my $options = $quiz->[$question]->{options};
	return $options || '';
    }

=head3 qn

The number of questions in the given jigsaw for the given group.

=cut

    method qn ( Str $jigsaw, Str $group ) {
	my $quiz = $self->quiz( $jigsaw, $group );
	return scalar @$quiz;
    }

=head3 responses

The responses of the members of the given group in the given jigsaw (as an anon hash keyed on the ids of the members). In a file in the jigsaw directory called 'response.yaml'.

=cut


    method responses ( Str $jigsaw, Str $group ) {
	my $jigsaws = $self->jigsawdirs;
	my $responses = $self->inspect( "$jigsaws/$jigsaw/response.yaml" );
	return $responses->{$group};
    }

=head3 jigsawGroups

A hash ref of all the groups in the given jigsaw and the names of members of the groups, keyed on groupnames. There may be duplicated names if one player did the activity twice as an 'assistant' for a group with not enough players, and missing names if a player did not do the quiz.

=cut

	method jigsawGroups (Str $jigsaw ) {
		my $config = $self->config('Jigsaw', $jigsaw );
		$config->{group};
	}

=head3 jigsawGroupMembers

An array (was hash ref) of the names of the members of the given group in the given jigsaw, in order of the roles, A..D.

=cut

	method jigsawGroupMembers (Str $jigsaw, Str $group) {
		my $groups = $self->jigsawGroups( $jigsaw );
		my $members = $groups->{$group};
	}

=head3 roles

At the moment, just A .. D.

=cut

	has 'roles' => (is => 'ro', isa => 'ArrayRef[Str]',
	    default => sub { [ qw/A B C D/ ] } );


=head3 idsbyRole

Ids in array, in A-D role order

=cut


    method idsbyRole ( Str $jigsaw, Str $group ) {
	my $members = $self->league->members;
	my %namedMembers = map { $_->{name} => $_ } @$members;
	my $namesbyRole = $self->jigsawGroupMembers( $jigsaw, $group );
	my @idsbyRole = map { $namedMembers{$_}->{id} } @$namesbyRole;
	return \@idsbyRole;
    }

=head3 assistants

A array ref of all the players in the (sub)jigsaw who did the the activity twice to 'assist' groups with not enough (or absent) players, or individuals with no groups, or people who arrived late.

=cut

	method assistants (Str $jigsaw) {
		my $round = $self->config( $jigsaw );
		$round->{assistants};
	}

=head3 jigsawGroupRole

An hash ref of the roles of the members of the given group in the given jigsaw, keyed on the name of the player.

=cut

	method jigsawGroupRole (Str $jigsaw, Str $group) {
		my $members = $self->jigsawGroupMembers( $jigsaw, $group );
		my %roles;
		@roles{ @$members } = $self->roles->flatten;
		return \%roles;
	}

=head3 id2jigsawGroupRole

An hash ref of the roles of the members of the given group in the given jigsaw, keyed on the id of the player.

=cut

	method id2jigsawGroupRole (Str $jigsaw, Str $group) {
		my $members = $self->jigsawGroupMembers( $jigsaw, $group );
		my @ids = map { $self->league->ided($_) } @$members;
		my $roles = $self->roles;
		my %id2role; @id2role{@ids} = @$roles;
		return \%id2role;
	}

=head3 name2jigsawGroup

An array ref of the group(s) to which the given name belonged in the given jigsaw. Normally, the array ref has only one element. But if the player was an assistant an array ref of more than one group is returned. If the player did not do the jigsaw, no groups are returned.

=cut

	method name2jigsawGroup (Str $jigsaw, Str $name) {
		my $groups = $self->jigsawGroups( $jigsaw );
		my @memberships;
		for my $id ( keys %$groups ) {
			my $group = $groups->{$id};
			push @memberships, $id if any { $_ eq $name } @$group;
		}
		return \@memberships;
	}

=head3 rawJigsawScores

The individual scores on the given quiz of each member of the given group, keyed on their roles, no, ids, from the file called 'scores.yaml' in the given jigsaw dir. If the scores in that file have a key which is a role, handle that, but, yes, the keys of the hashref returned here are the players' ids.

=cut

    method rawJigsawScores (Str $round, Str $group) {
        my $data;
	my $jigsaws = $self->jigsawdirs;
	try { $data = $self->inspect( "$jigsaws/$round/scores.yaml"); }
	    catch { warn "No scores for $group group in jigsaw $round."; };
	my $groupdata = $data->{letters}->{$group};
	my $ids       = $self->idsbyRole( $round, $group );
	my $roles     = $self->roles;
	my @keys;
	if (
	    any { my $key = $_; any { $_ eq $key } @$roles; } keys %$groupdata
	) {
	    @keys = @$roles;
	}
        else {
            @keys = grep { my $id = $_; any { $_ eq $id } @$ids }
		    keys %$groupdata;
        }
        my %scores;
	@scores{@keys} = @{$groupdata}{@keys};
	return \%scores;
    }

=head3 jigsawDeduction

Points deducted for undesirable performance elements (ie Chinese use) on the quiz of the given group in the given exam.

=cut

    method jigsawDeduction (Str $jigsaw, Str $group) {
	my $data;
	my $jigsaws = $self->jigsawdirs;
	try { $data = $self->inspect( "$jigsaws/$jigsaw/scores.yaml" ); }
	    catch { warn
		"Deductions for $group group in $jigsaw jigsaw?" };
	my $demerits = $data->{Chinese}->{$group};
	return $demerits;
    }

}


=head2 Grades' Classwork Methods

Classwork is work done in class with everyone and the teacher present. Two classwork approaches are Compcomp and Groupwork. Others are possible. Depending on the league's approach accessor, the methods are delegated to the appropriate Approach object.

=cut

class Classwork {
	use Grades::Types qw/Results/;

=head3 approach

Delegatee handling classwork_total, classworkPercent

=cut

    has 'approach' => ( is => 'ro', isa => 'Approach', required => 1,
	    handles => [ qw/
		series beancans
		all_weeks points
		classwork_total classworkPercent / ] );

}

=head2 Classwork Approach

Handles Classwork's classwork_total and classworkPercent methods. Calls the total or totalPercent methods of the class whose name is in the 'type' accessor.

=cut

class Approach {

=head3 league

The league (object) whose approach this is.

=cut

    has 'league' => (is =>'ro', isa => 'League', required => 1,
				handles => [ 'inspect' ] );

=head3 series

The sessions over the series in which there were different groupings of players.

=cut

    method series {
	my $league = $self->league;
	my $type = $league->approach;
	my $total = $type->new( league => $league )->series;
    }

=head3 all_weeks

All the weeks, or sessions or lessons for which grade data is being assembled from for the grade component.

=cut

    method all_weeks {
	my $league = $self->league;
	my $type = $league->approach;
	my $meta = $type->meta;
	my $total = $type->new( league => $league )->all_weeks;
    }

=head3 points

Week-by-weeks, or session scores for the individual players in the league.

=cut

    method points (Str $week) {
	my $league = $self->league;
	my $type = $league->approach;
	my $meta = $type->meta;
	my $total = $type->new( league => $league )->points( $week );
    }

=head3 classwork_total

Calls the pluginned approach's classwork_total.

=cut

    method classwork_total {
	my $league = $self->league;
	my $type = $league->approach;
	my $total = $type->new( league => $league )->total;
    }

=head3 classworkPercent

Calls the pluginned approach's classworkPercent.

=cut

    method classworkPercent {
	my $league = $self->league;
	my $type = $league->approach;
	my $total = $type->new( league => $league )->totalPercent;
    }
}


=head2 Grades' Compcomp Methods

The comprehension question competition is a Swiss tournament regulated 2-partner conversation competition where players try to understand more of their opponent's information than their partners understand of theirs.

=cut

class Compcomp extends Approach {
    use Try::Tiny;
    use List::MoreUtils qw/any/;
    use Carp qw/carp/;
    use Grades::Types qw/Results/;

=head3 compcompdirs

The directory under which there are subdirectories containing data for the Compcomp rounds.

=cut

    has 'compcompdirs' => (is => 'ro', isa => 'Str', lazy_build => 1 );
    method _build_compcompdirs { 
	my $leaguedir = $self->league->leagues . "/" . $self->league->id;
	my $compcompdir = $leaguedir .'/' . shift->league->yaml->{compcomp};
    }

=head3 all_weeks

The pair conversations over the series (semester). This method returns an arrayref of the numbers of the conversations, in numerical order, of the form, [1, 3 .. 7, 9, 10 .. 99 ]. Results are in sub directories of the same name, under compcompdirs.

=cut

    has 'all_weeks' =>
      ( is => 'ro', isa => 'Maybe[ArrayRef[Int]]', lazy_build => 1 );
    method _build_all_weeks {
        my $dir = $self->compcompdirs;
        my @subdirs = grep { -d } glob "$dir/*";
        [ sort { $a <=> $b } map m/^$dir\/(\d+)$/, @subdirs ];
    }

=head3 config

The round.yaml file with data about the Compcomp activity for the given conversation (directory.)

=cut

    method config( Str $round) {
	my $comp = $self->compcompdirs;
	my $file = "$comp/$round/round.yaml";
        my $config;
	try { $config = $self->inspect($file) }
	    catch { warn "No config file for Compcomp round $round at $file" };
	return $config;
    }

=head3 tables

The tables with players according to their roles for the given round, as an array ref. In the 'activities' mapping in the config file. Not ordered by table name or number. Tables undertaking more than one activity are only listed once.

activities:
  drbean:
    1:
      - White: N9661748
        Black: U9714127
  novak:
    1:
      - White: N9532037
        Black: U9714111
      - White: V9731066
        Black: V9810423

=cut

    method tables ( Str $round ) {
	my $config = $self->config($round);
	my $activities = $config->{activity};
	my @pairs;
	for my $key ( keys %$activities ) {
	    my $topic = $activities->{$key};
	    for my $form ( keys %$topic ) {
		my $pairs = $topic->{$form};
		for my $pair ( @$pairs ) {
		    my @players = values %$pair;
		    my @roles = keys %$pair;
		    push @pairs, $pair unless
			any { my @previous = values %$_;
			    any { my $player=$_;
				any { $player eq $_ } @previous
			    } @players
			} @pairs;
		}
	    }
	}
	return \@pairs;
    }

=head3 compQuizfile

The file system location of the file with the quiz questions and answers for the given Compcomp activity.

=cut

    method compQuizfile ( Str $round ) {
	my $config = $self->config($round);
	return $config->{text};
    }

=head3 compQuizSelection

The comp quiz topics and their associated forms attempted by groups in the round, as the keys of an undef hashref.

=cut

    method compQuizSelection ( Str $round ) {
	my $config = $self->config($round);
	my $activity = $config->{activity};
	my $selection;
	for my $topic ( keys %$activity ) {
	    my $forms = $activity->{$topic};
	    for my $form ( keys %$forms ) {
		$selection->{$topic}->{$form} = undef;
	    }
	}
	return $selection;
    }

=head3 compQuizAttempted

Returns the comp quiz topics and their associated forms attempted by the given group in the round, as an arrayref of hashrefs keyed on 'topic' and 'form'.

=cut

    method compQuizAttempted ( Str $round, Str $table ) {
	my $config = $self->config($round);
	my $activities = $config->{activity};
	my $selection = $self->compQuizSelection;
	my $attempted;
	for my $topic ( keys %$selection ) {
	    my $forms = $selection->{$topic};
	    for my $form ( keys %$forms ) {
		my $tables = $activities->{$topic}->{$form};
		push @$attempted, { topic => $topic, form => $form }
		    if any { $table == $_ } @$tables;
	    }
	}
	return $attempted;
    }

=head3 compQuiz

The compQuiz questions (as an anon array) in the given Compcomp activity for the given table.

=cut

    method compQuiz ( Str $round, Str $table ) {
	my $quizfile = $self->compQuizfile($round);
	my $activity;
	try { $activity = $self->inspect( $quizfile ) }
	    catch { warn "No $quizfile Compcomp content file" };
	my $topic = $self->compTopic( $round, $table );
	my $form = $self->compForm( $round, $table );
	my $quiz = $activity->{$topic}->{compcomp}->{$form}->{quiz};
	carp "No $topic, $form quiz in $quizfile," unless $quiz;
	return $quiz;
    }

=head3 compTopic

The topic of the quiz in the given Compcomp round for the given table. Each table has one and only one quiz.

=cut

    method compTopic ( Str $round, Str $table ) {
	my $config = $self->config($round);
	my $activity = $config->{activity};
	for my $topic ( keys %$activity ) {
	    my $forms = $activity->{$topic};
	    for my $form ( keys %$forms ) {
		my $tables = $forms->{$form};
		return $topic if any { $_ eq $table } @$tables;
	    }
	}
	carp "Topic? No quiz at $table table in round $round,";
	return;
    }

=head3 compForm

The form of the quiz in the given Compcomp round for the given table. Each table has one and only one quiz.

=cut

    method compForm ( Str $round, Str $table ) {
	my $config = $self->config($round);
	my $activity = $config->{activity};
	for my $topic ( keys %$activity ) {
	    my $forms = $activity->{$topic};
	    for my $form ( keys %$forms ) {
		my $tables = $forms->{$form};
		return $form if any { $_ eq $table } @$tables;
	    }
	}
	carp "Form? No quiz at $table table in round $round,";
	return;
    }

=head3 compqn

The number of questions in the given Compcomp quiz for the given pair.

=cut

    method compqn ( Str $round, Str $table ) {
	my $quiz = $self->compQuiz( $round, $table );
	return scalar @$quiz;
    }

=head3 idsbyCompRole

Ids in array, in White, Black role order

=cut


    method idsbyCompRole ( Str $round, Str $table ) {
	my $members = $self->league->members;
	my %namedMembers = map { $_->{name} => $_ } @$members;
	my $config = $self->config( $round );
	my $pair = $config->{pair}->{$table};
	my @idsbyRole = @$pair{qw/White Black/};
	return \@idsbyRole;
    }

=head3 scores

The scores at the tables of the tournament in the given round (as an anon hash keyed on the ids of the members). In a file in the Compcomp round directory called 'result.yaml'.

=cut


    method scores ( Str $round ) {
	my $comp = $self->compcompdirs;
	my $file = "$comp/$round/scores.yaml";
	my $results = $self->inspect( $file );
	return $results;
    }

=head3 compResponses

The responses of the members of the given pair in the given round (as an anon hash keyed on the ids of the members). In a file in the Compcomp round directory called 'response.yaml'.

=cut


    method compResponses ( Str $round, Str $table ) {
	my $comp = $self->compcompdirs;
	my $file = "$comp/$round/response.yaml";
	my $responses = $self->inspect( $file );
	return $responses->{$table};
    }


=head3 byer

The id of the player with the Bye, or the empty string.

=cut

    method byer ( Str $round ) {
	my $config = $self->config( $round );
	my $byer = $config->{bye};
	return $byer if $byer;
	return '';
    }


=head3 opponents

The ids of opponents of the players in the given conversation.

=cut

    method opponents ( Str $round ) {
	my $tables = $self->tables( $round );
	my %opponent;
	for my $table ( @$tables ) {
	    $opponent{$table->{White}} = $table->{Black};
	    $opponent{$table->{Black}} = $table->{White};
	}
	my $byer = $self->byer( $round );
	$opponent{ $byer } = 'bye' if $byer;
	my $league = $self->league;
	my $members = $league->members;
	$opponent{$_->{id}} ||= 'unpaired' for @$members;
	return \%opponent;
    }


=head3 correct

The number of questions correct in the given conversation.

=cut

    method correct ( Str $round ) {
	my $comp = $self->compcompdirs;
	my $file = "$comp/$round/scores.yaml";
	my $tables = $self->inspect( $file );
	my %correct;
	for my $table ( keys %$tables ) {
	    my $scores = $tables->{$table};
	    @correct{keys %$scores} = values %$scores;
	}
	return \%correct;
    }


=head3 points

The points of the players in the given conversation. 5 for a Bye, 1 for Late, 0 for Unpaired, 1 for a non-numerical number correct result, 5 for more correct, 3 for less correct, 4 for the same number correct. Transfers' results are computed from their results in the same round in their old league.

=cut

    method points ( Str $round ) {
	my $config = $self->config( $round );
	my $opponents = $self->opponents( $round );
	my $correct = $self->correct( $round );
	my $points;
	my $late; $late = $config->{late} if exists $config->{late};
	my $byer = $self->byer( $round );
	PLAYER: for my $player ( keys %$opponents ) {
	    if ( $byer and $player eq $byer ) {
		$points->{$player} = 5;
		next PLAYER;
	    }
	    if ( any { $_ eq $player } @$late ) {
		$points->{$player} = 1;
		next PLAYER;
	    }
	    if ( $opponents->{$player} =~ m/unpaired/i ) {
		$points->{$player} = 0;
		next PLAYER;
	    }
	    if ( $opponents->{$player} =~ m/transfer/i ) {
		my $oldleagueId = $self->league->transfer->{$player};
		my $oldleague = League->new( id => $oldleagueId );
		my $oldgrades = Grades->new( league => $oldleague );
		$points->{$player} = $oldgrades->points($round)->{$player};
		next PLAYER;
	    }
	    my $other = $opponents->{$player};
	    my $alterego = $opponents->{$other};
	    die
"${player}'s opponent is $other, but ${other}'s opponent is $alterego"
		unless $other and $alterego and $player eq $alterego;
	    die "No $player quiz card in round $round?" unless exists
		$correct->{$player};
	    my $ourcorrect = $correct->{$player};
	    die "No $other card against $player in round $round?" unless
		exists $correct->{$other};
	    my $theircorrect = $correct->{$other};
	    if ( not defined $ourcorrect ) {
		$points->{$player} = 0;
		next PLAYER;
	    }
	    if ( $correct->{$player} !~ m/^\d+$/ ) {
		$points->{$player} = 1;
		next PLAYER;
	    }
	    if ( not defined $theircorrect ) {
		$points->{$player} = 5;
		next PLAYER;
	    }
	    $points->{$player} = $ourcorrect > $theircorrect? 5:
				$ourcorrect < $theircorrect? 3: 4
	}
	return $points;
    }


=head3 total

The total over the conversations over the series.

=cut

    has 'total' => ( is => 'ro', isa => Results, lazy_build => 1 );
    method _build_total {
	my $rounds = $self->all_weeks;
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


=head3 totalPercent

The total over the conversations over the series expressed as a percentage of the possible score. The average should be 80 percent if every player participates in every comp.

=cut

    has 'totalPercent' => ( is => 'ro', isa => Results, lazy_build => 1 );
    method _build_totalPercent {
	my $rounds = $self->all_weeks;
	my $n = scalar @$rounds;
	my $totals = $self->total;
	my %percentages = map { $_ => $totals->{$_} * 100 / (5*$n) } keys %$totals;
	return \%percentages;
    }

}


=head2 Grades' Groupwork Methods

The idea of Cooperative Learning, giving individual members of a group all the same score, is that individuals are responsible for the behavior of the other members of the group. Absences and tardies of individual members can lower the scores of the members who are present.

Also, grading to the curve is employed so that the average classwork grade over a session for each student is 80 percent.

=cut

class Groupwork extends Approach {
	use List::Util qw/max min sum/;
	use List::MoreUtils qw/any/;
	use Carp;
	use POSIX;
	use Grades::Types qw/Beancans Card Results/;
	use Try::Tiny;

=head3 groupworkdirs

The directory under which there are subdirectories containing data for the groupwork sessions.

=cut

    has 'groupworkdirs' => (is => 'ro', isa => 'Str', lazy_build => 1);
    method _build_groupworkdirs {
	my $league = $self->league->id;
	my $leaguedir = $self->league->leagues . "/" . $league;
	my $basename = shift->league->yaml->{groupwork} || "classwork";
	my $groupworkdirs = $leaguedir .'/' . $basename;
	}

=head3 series

The sessions over the series (semester) in which there was a different grouping (beancans) of players. Everyone in the same beancan for one session gets the same number of beans (classwork score.) This method returns an arrayref of the names of the sessions, in numerical order, of the form, [1, 3 .. 7, 9, 10 .. 99 ]. Results are in sub directories of the same name, under groupworkdirs.

=cut

    has 'series' =>
      ( is => 'ro', isa => 'Maybe[ArrayRef[Int]]', lazy_build => 1 );
    method _build_series {
        my $dir = $self->groupworkdirs;
        my @subdirs = grep { -d } glob "$dir/*";
        [ sort { $a <=> $b } map m/^$dir\/(\d+)$/, @subdirs ];
    }

=head3 beancanseries

The different beancans for each of the sessions in the series. In the directory for each session of the series, there is a file called beancans.yaml, containing mappings of a beancan name to a sequence of PlayerNames, the members of the beancan. If beancans.yaml cannot be found, a file called groups.yaml is used instead. TODO There are 2 identical methods called beancanseries.

=cut

    has 'beancanseries' => ( is => 'ro', lazy_build => 1 );
    method _build_beancanseries {
	my $dir = $self->groupworkdirs;
        my $series = $self->series;
        my $league = $self->league->id;
	my %beancans;
	for my $round ( @$series ) {
	    my $beancanfile = "$dir/$round/beancans.yaml";
	    my $file = -e $beancanfile? $beancanfile: "$dir/$round/groups.yaml";
	    try { $beancans{$round} = $self->inspect( $file ) }
		catch { local $" = ', ';
		    warn "Missing beancans in $league $dir round $round," };
	}
	return \%beancans;
    }

=head3 beancans

A hashref of all the beancans in a given session with the names of the members of each beancan. The number, composition and names of the beancans may change from one session of the series to the next.
	
Players in one beancan all get the same Groupwork grade for that session. The beancan members may be the same as the members of the class group, who work together in class, or may be individuals. Usually in a big class, the beancans will be the same as the groups, and in a small class they will be individuals.

Players in the 'Absent' beancan all get a grade of 0 for the session.

Rather than refactor the class to work with individuals rather than groups, and expand some methods (?) to fall back to league members if it finds them in the weekly files instead of groups, I decided to introduce another file, beancans.yaml, and change all variable and method names mentioning group to beancan.

=cut 

	method beancans (Str $session) { $self->beancanseries->{$session}; }

=head3 allfiles

The files containing classwork points (beans) awarded to beancans. 

=cut


	has 'allfiles'  => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allfiles {
		my $dir = $self->groupworkdirs;
		my $series = $self->series;
		my $league = $self->league->id;
		my $files = [ map { grep m|/(\d+)\.yaml$|,
					glob "$dir/$_/*.yaml" } @$series ];
		croak "${league}'s @$series files: @$files?" unless @$files;
		return $files;
	}

=head3 all_weeks

The weeks (an array ref of integers) in which beans were awarded.

=cut

	has 'all_weeks' => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_all_weeks {
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
		my $weeks = $self->all_weeks;
		max @$weeks;
	}

=head3 data

The beans awarded to the beancans in the individual cards over the weeks of the series (semester.)

=cut

	has 'data' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_data {
		my $files = $self->allfiles;
		my $weeks = $self->all_weeks;
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

	$Groupwork->week2session(15) # fourth

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

	$Groupwork->name2beancan( $week, $playername )

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

	$Groupwork->beancansNotInCard( $beancans, $card, 3)

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

	$Groupwork->beancansNotInCard( $beancans, $card, 3)

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
				carp "$can not in week $week Groupwork"
					unless defined $grade->{$can};
				$sessiontotal{$can} += $grade->{$can};
			}
		}
		\%sessiontotal;
	}

=head3 totalPercent

Running totals for individual ids out of 100, over the whole series.

=cut
	has 'totalPercent' => ( is => 'ro', isa => Results, lazy_build => 1 );
	method _build_totalPercent {
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
						" not in session $session"
						unless defined $grade;
					$grades{$id} += $grade;
				} else {
					carp $member->{name} .
					"'s beancan in session $session?"
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
				carp "$name $id Groupwork?";
				$grades{$id} = 0;
			}
		}
		\%grades;
	}

}

=head2 Grades' GroupworkNoFault Approach

Unlike the Groupwork approach, GroupworkNoFault does not penalize members of a group who are present for the absence of other members who are not present or tardy. Instead the individual members not present get a grade of 0 for that class.

Also, no scaling of the grades (a group's merits) takes place. 

=cut

class GroupworkNoFault extends Approach {
	use List::Util qw/max min sum/;
	use List::MoreUtils qw/any/;
	use Carp;
	use POSIX;
	use Grades::Types qw/Beancans TortCard PlayerNames Results/;
	use Try::Tiny;

=head3 classMax

The maximum score possible in individual lessons for classwork.

=cut

	has 'classMax' => (is => 'ro', isa => 'Int', lazy => 1, required => 1,
			default => sub { shift->league->yaml->{classMax} } );

=head3 groupworkdirs

The directory under which there are subdirectories containing data for the groupwork sessions.

=cut

    has 'groupworkdirs' => (is => 'ro', isa => 'Str', lazy_build => 1);
    method _build_groupworkdirs {
	my $league = $self->league->id;
	my $leaguedir = $self->league->leagues . "/" . $league;
	my $basename = shift->league->yaml->{groupwork} || "classwork";
	my $groupworkdirs = $leaguedir .'/' . $basename;
	}

=head3 series

The sessions over the series (semester) in which there was a different grouping (beancans) of players. Everyone in the same beancan for one session gets the same number of beans (classwork score.) This method returns an arrayref of the names of the sessions, in numerical order, of the form, [1, 3 .. 7, 9, 10 .. 99 ]. Results are in sub directories of the same name, under groupworkdirs.

=cut

    has 'series' =>
      ( is => 'ro', isa => 'Maybe[ArrayRef[Int]]', lazy_build => 1 );
    method _build_series {
        my $dir = $self->groupworkdirs;
        my @subdirs = grep { -d } glob "$dir/*";
        [ sort { $a <=> $b } map m/^$dir\/(\d+)$/, @subdirs ];
    }

=head3 beancanseries

The different beancans for each of the sessions in the series. In the directory for each session of the series, there is a file called beancans.yaml, containing mappings of a beancan name to a sequence of PlayerNames, the members of the beancan. If beancans.yaml cannot be found, a file called groups.yaml is used instead.

=cut

    has 'beancanseries' => ( is => 'ro', isa => Beancans, lazy_build => 1 );
    method _build_beancanseries {
	my $dir = $self->groupworkdirs;
        my $series = $self->series;
        my $league = $self->league->id;
	my %beancans;
	for my $round ( @$series ) {
	    my $beancanfile = "$dir/$round/beancans.yaml";
	    my $file = -e $beancanfile? $beancanfile: "$dir/$round/groups.yaml";
	    try { $beancans{$round} = $self->inspect( $file ) }
		catch { local $" = ', ';
		    warn "Missing beancans in $league $dir round $round," };
	}
	return \%beancans;
    }

=head3 beancans

A hashref of all the beancans in a given session with the names of the members of each beancan. The number, composition and names of the beancans may change from one session of the series to the next.
	
Players in one beancan all get the same Groupwork grade for that session. The beancan members may be the same as the members of the class group, who work together in class, or may be individuals. Usually in a big class, the beancans will be the same as the groups, and in a small class they will be individuals.

Players in the 'Absent' beancan all get a grade of 0 for the session.

Rather than refactor the class to work with individuals rather than groups, and expand some methods (?) to fall back to league members if it finds them in the weekly files instead of groups, I decided to introduce another file, beancans.yaml, and change all variable and method names mentioning group to beancan.

=cut 

	method beancans (Str $session) { $self->beancanseries->{$session}; }

=head3 allfiles

The files containing classwork points (beans) awarded to beancans. 

=cut


	has 'allfiles'  => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_allfiles {
		my $dir = $self->groupworkdirs;
		my $series = $self->series;
		my $league = $self->league->id;
		my $files = [ map { grep m|/(\d+)\.yaml$|,
					glob "$dir/$_/*.yaml" } @$series ];
		croak "${league}'s @$series files: @$files?" unless @$files;
		return $files;
	}

=head3 all_weeks

The weeks (an array ref of integers) in which beans were awarded.

=cut

	has 'all_weeks' => ( is => 'ro', isa => 'ArrayRef', lazy_build => 1 );
	method _build_all_weeks {
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
		my $weeks = $self->all_weeks;
		max @$weeks;
	}

=head3 data

The beans awarded to the beancans in the individual cards over the weeks of the series (semester.)

=cut

	has 'data' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
	method _build_data {
		my $files = $self->allfiles;
		my $weeks = $self->all_weeks;
		+{ map { $weeks->[$_] => $self->inspect( $files->[$_] ) }
			0..$#$weeks };
	}

=head3 card

Classwork beans for each beancan for the given week

=cut

	method card (Num $week) {
		my $card = $self->data->{$week};
		croak "Week $week card probably has undefined or non-numeric Merit, Absence, Tardy scores, or possibly illegal beancan."
		    unless is_TortCard( $card );
		return $card;
	}

=head3 beancans

A hashref of all the beancans in a given session with the names of the members of each beancan. The number, composition and names of the beancans may change from one session of the series to the next.
	
Players in one beancan all get the same Groupwork grade for that session. The beancan members may be the same as the members of the class group, who work together in class, or may be individuals. Usually in a big class, the beancans will be the same as the groups, and in a small class they will be individuals.

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

	$Groupwork->week2session(15) # fourth

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

	$Groupwork->name2beancan( $week, $playername )

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

	$Groupwork->beancansNotInCard( $beancans, $card, 3)

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

	$Groupwork->beancansNotInCard( $beancans, $card, 3)

Test all of the beancans, except Absent, have all the points due them for the week. Duplicates the check done by the TortCard type.

=cut

	method beancanDataOnCard (HashRef $beancans, HashRef $card, Num $week) {
		my @noData = grep { my $beancan = $card->{$_};
			$_ ne 'Absent' and ( 
				not defined $beancan->{merits} or 
					 $beancan->{absent} and not
					 is_PlayerNames( $beancan->{absent} )
				    ) }
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

=head3 absent

The players absent from each beancan in the given week.

=cut

	method absent (Num $week) {
		my $session = $self->week2session($week);
		my $beancans = $self->active($session);
		my $card = $self->card($week);
		$self->beancansNotInCard($beancans, $card, $week);
		$self->beancanDataOnCard($beancans, $card, $week);
		+{ map { $_ => $card->{$_}->{absent} } keys %$beancans };
	}

=head3 points

The merits the beancans gained for the given week, except for those members who were absent, and who get zero. Keyed on player id.

=cut

	method points (Num $week) {
	    my $members = $self->league->members;
	    my %points;
	    for my $member ( @$members ) {
		my $name = $member->{name};
		my $id = $member->{id};
		my $beancan = $self->name2beancan( $week, $name );
		my $absent = $self->absent($week)->{$beancan};
		unless ( $absent and ref $absent eq 'ARRAY' ) {
		    $points{$id} = $self->merits($week)->{$beancan}; 
		}
		else {
		    $points{$id} = ( any { $name eq $_ } @$absent ) ?
			2 : $self->merits($week)->{$beancan};
		}
	    }
	    return \%points;
	}

=head3 sessionMerits

The merits the beancans gained for the given session, with the Absent beancan getting zero. Keyed on beancan.

=cut

    method sessionMerits (Num $session) {
	my $weeks = $self->weeks($session);
	my $beancans = $self->beancans($session);
	my %merits;
	for my $week ( @$weeks ) {
	    my $merits = $self->merits($week);
	    $merits->{Absent} = 0;
	    $merits{$_} += $merits->{$_} for keys %$beancans;
	}
	$merits{Absent} = 0;
	return \%merits;
    }

=head3 grades4session

Totals for the beancans over the given session, keyed on individual names.

=cut

    method grades4session (Str $session) {
	my $weeks = $self->weeks($session);
	my $beancans = $self->beancans($session);
	my %tally;
	for my $week ( @$weeks ) {
	    my $grade = $self->merits($week);
	    my $absent = $self->absent($week);
	    for my $can ( keys %$beancans ) {
		my $members = $beancans->{$can};
		if ( $can =~ m/absent/i ) {
		    my @missing = @$members;
			$tally{$_} = 0 for @missing;
			next;
		}
		carp "$can not in week $week Groupwork"
			unless defined $grade->{$can};
		my $absent = $self->absent($week)->{$can};
		for my $member ( @$members ) {
		    $tally{$member} += $grade->{$can}
			unless any { $member eq $_ } @$absent;
		}
	    }
	}
	\%tally;
    }

=head3 total

Totals for individual ids, over the whole series.

=cut

    has 'total' => ( is => 'ro', isa => Results, lazy_build => 1 );
    method _build_total {
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
		    my $grade = $grade->{$name};
		    carp $member->{name} .
			" not in session $session"
			unless defined $grade;
		    $grades{$id} += $grade;
		} else {
		    carp $member->{name} .
		    "'s beancan in session $session?"
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
		carp "$name $id Groupwork?";
		$grades{$id} = 0;
	    }
	}
	\%grades;
    }

=head3 totalPercent

Running totals for individual ids out of 100, over the whole series.

=cut
	has 'totalPercent' => ( is => 'ro', isa => Results, lazy_build => 1 );
	method _build_totalPercent {
		my $members = $self->league->members;
		my $weeks = $self->all_weeks;
		my $weeklyMax = $self->classMax;
		my $totalMax = $weeklyMax * @$weeks;
		my $grades = $self->total;
		my $series = $self->series;
		my %percent;
		for my $member ( @$members ) {
		    my $id = $member->{id};
		    my $score = 100 * $grades->{$id} / $totalMax ;
		    warn "$member->{name}: ${id}'s classwork score of $score"
			if $score > 100;
		    $percent{$id} = $score;
		}
		return \%percent;
	}

}


=head2 Grades' Exams Methods
=cut

role Exams {
	use List::Util qw/max sum/;
	use List::MoreUtils qw/any all/;
	use Carp;
	use Grades::Types qw/Exam/;

=head3 examdirs

The directory where the exams are.

=cut

    has 'examdirs' => (is => 'ro', isa => 'Str', lazy_build => 1);
    method _build_examdirs {
	my $league = $self->league->id;
	my $leaguedir = $self->league->leagues . "/" . $league;
	my $basename = $self->league->yaml->{jigsaw} ||
			$self->league->yaml->{exams} || "exams";
	my $examdirs = $leaguedir .'/' . $basename;
    }

=head3 examids

An arrayref of the ids of the exams for which there are grades for players in the league, in numerical order, of the form, [1, 3 .. 7, 9, 10 .. 99 ]. Results are in sub directories of the same name, under examdir.

=cut

    has 'examids',
      ( is => 'ro', isa => 'Maybe[ArrayRef[Int]]', lazy_build => 1 );
    method _build_examids {
        my $examdirs = $self->examdirs;
        my @exams   = grep { -d } glob "$examdirs/[0-9] $examdirs/[1-9][0-9]";
        [ sort { $a <=> $b } map m/^$examdirs\/(\d+)$/, @exams ];
    }

=head3 examrounds

The rounds over which the given exam was conducted. Should be an array ref. If there were no rounds, ie the exam was conducted in one round, a null anonymous array is returned. The results for the rounds are in sub directories underneath the 'examid' directory named, in numerical order, 1 .. 99.

=cut

    method examrounds( Str $exam ) {
	my $examdirs = $self->examdirs;
        my $examids = $self->examids;
        carp "No exam $exam in exams @$examids"
	    unless any { $_ eq $exam } @$examids;
        my @rounds = glob "$examdirs/$exam/[0-9] $examdirs/$exam/[0-9][0-9]";
        [ sort { $a <=> $b } map m/^$examdirs\/$exam\/(\d+)$/, @rounds ];
      }

=head3 examMax

The maximum score possible in each individual exam. That is, what the exam is out of.

=cut

	has 'examMax' => (is => 'ro', isa => 'Int', lazy => 1, required => 1,
			default => sub { shift->league->yaml->{examMax} } );

=head3 exam

    $grades->exam($id)

The scores of the players on an individual (round of an) exam (in a 'g.yaml file in the $id subdir of the league dir.

=cut

	method exam ( Str $id ) {
	    my $examdirs = $self->examdirs;
	    my $exam = $self->inspect( "$examdirs/$id/g.yaml" );
	    if ( is_Exam($exam) ) {
		return $exam ;
	    }
	    else {
		croak
"Exam $id probably has undefined or non-numeric Exam scores, or possibly illegal PlayerIds." ;
	    }
	}

=head3 examResults

A hash ref of the ids of the players and arrays of their results over the exam series, ie examids, in files named 'g.yaml', TODO but only if such a file exists in all examdirs. Otherwise, calculate from raw 'response.yaml' files. Croak if any result is larger than examMax.

=cut

    has 'examResults' => ( is => 'ro', isa => 'HashRef', lazy_build => 1 );
    method _build_examResults {
        my $examids = $self->examids;
	my $members = $self->league->members;
	my @playerids = map { $_->{id} } @$members;
	my %results;
	for my $id  ( @$examids ) {
	    my $exam    = $self->exam( $id );
	    my $max      = $self->examMax;
	    for my $playerid ( @playerids ) {
		my $result = $exam->{$playerid};
		carp "No exam $id results for $playerid,"
		  unless defined $result;
		croak "${playerid}'s $result greater than exam max, $max"
		  if defined $result and $result > $max;
		my $results = $results{$playerid};
		push @$results, $result;
		$results{$playerid} = $results;
	    }
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
		my @ids = keys %$scores;
		my $max = $self->examMax;
		my %percent =  map { my $id = $_; my $myscores = $scores->{$id};
		    $id => [ map { ($_||0) * (100/$max) } @$myscores ] } @ids;
		return \%percent;
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
	my %totals = map {
		my $numbers=$grades->{$_};
		$_ => sum(@$numbers)/@{$numbers} } keys %$grades;
	return \%totals;
    }

}


=head2 Grades' Core Methods

=cut

class Grades with Homework with Exams with Jigsaw
{
#    with 'Jigsaw'
#	=> { -alias => { config => 'jigsaw_config' }, -excludes => 'config' };
	use Carp;
	use Grades::Types qw/Weights/;

=head3 BUILDARGS

Have Moose find out the classwork approach the league has adopted and create an object of that approach for the classwork accessor. This is preferable to requiring the user to create the object and pass it at construction time.

=cut

    around BUILDARGS (ClassName $class: HashRef $args) {
        my $league = $args->{league} or die "$args->{league} league?";
        my $approach = $league->approach or die "approach?";
        my $classwork = $approach->new( league => $league ) or die "classwork?";
        $args->{classwork} = $classwork;
        return $class->$orig({ league => $league, classwork => $classwork });
    }
    # around BUILDARGS(@args) { $self->$orig(@args) }

=head3 classwork

An accessor for the object that handles classwork methods. Required at construction time.

=cut

	has 'classwork' => ( is => 'ro', isa => 'Approach', required => 1,
		handles => [ 'series', 'beancans',
			    'points', 'all_weeks',
		    'classwork_total', 'classworkPercent' ] );

=head3 config

The possible grades config files. Including Jigsaw, Compcomp.

=cut

	method config ( $role, $round ) {
	    my $config = "${role}::config"; $self->$config( $round );
	}

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
		my $homework = $self->homeworkPercent;
		my $classcomponent = $league->approach;
		my $classwork = $self->classworkPercent;
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

Copyright 2009 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut


# vim: set ts=8 sts=4 sw=4 noet:
__END__
