use strict;
use warnings;
use Test::More;
use FindBin qw/$Bin/;

plan tests => 46;
plan skip_all => 'unset env var NO_TEST to enable this test' if $ENV{NO_TEST};

use lib 'lib';
use Grades;

my $l = League->new( id => 't/emile' );
my $g = Grades->new( league => $l );

# classwork

is_deeply( $g->series, [qw/first second third fourth/], '4-session series' );
is_deeply($g->beancanseries, {
	   first  => { one  => [ 'Emile', 'Sophie', ] },
           fourth => { one  => [ 'Emile', 'Sophie', ] },
           second => { one  => [ 'Emile', 'Sophie', ] },
           third  => { one  => [ 'Emile', 'Sophie', ] },
         }, 'beancans for 4 sessions' ); 
is_deeply( $g->allfiles, [ qw[
	t/emile/first/2.yaml t/emile/first/3.yaml t/emile/second/5.yaml
	t/emile/second/6.yaml t/emile/second/7.yaml t/emile/second/8.yaml
	t/emile/third/10.yaml t/emile/third/11.yaml t/emile/third/12.yaml
	t/emile/fourth/14.yaml t/emile/fourth/15.yaml t/emile/fourth/16.yaml
	] ], 'all 12 files in 4 sessions');
is_deeply( $g->allweeks, [2,3,5,6,7,8,10..12,14..16], "all 12 weeks");
is_deeply( $g->lastweek, 16, "last week");
is_deeply( $g->beancans('third'), {one  => ['Emile', 'Sophie'] }, "beancans" );
is_deeply( $g->weeks('fourth'), [14..16], 'weeks in fourth session');
is( $g->week2session(15), 'fourth', '15th week in fourth session');
is_deeply( $g->names2beancans('fourth'), { Emile  => 'one', Sophie  => 'one' },
	'names2beancans in 4th session');
is( $g->name2beancan(12, 'Emile'), 'one', 'beancan of Emile in week12');
is( $g->name2beancan(6, 'Sophie'), 'one', 'beancan of Sophie in week 6');
is( eval{ $g->name2beancan(3, 'KarlMarx') }, undef, 'Which group is Karl Marx in in week 3, but dies.');
is_deeply( $g->merits(14), { one => 3 }, "points for good things in Week 14");
is_deeply( $g->absences(2), { one => 1 }, "absences in each group in week 2");
is_deeply( $g->tardies(2), { one => 1 }, "people late in each group in week 2");
is( $g->payout('second'), 5, "If the total paid to players this week is 5, the average grade over the semester should be 80.");
is_deeply( $g->demerits(3), { one => 3 }, "2*absences+tardies");
is_deeply( $g->favor(2), { one => 1 }, "favor to avoid 0");
is( $g->maxDemerit(2), 3, "group with most absence, tardy demerits");
is_deeply( $g->meritDemerit(2), { one => 4 }, "merits - demerits");

# homework

is( $g->hwdir, 't/emile/homework', 'hwdirectory' );
is_deeply( $g->rounds, [6..14,16,17], 'homework rounds' );
is_deeply( $g->hwbyround, {
           6  => { 34113 => 0, S09413 => 1 },
           7  => { 34113 => 0, S09413 => 1 },
           8  => { 34113 => 0, S09413 => 1 },
           9  => { 34113 => 0, S09413 => 1 },
           10  => { 34113 => 0, S09413 => 1 },
           11  => { 34113 => 0, S09413 => 1 },
           12  => { 34113 => 0, S09413 => 1 },
           13  => { 34113 => 0, S09413 => 1 },
           14  => { 34113 => 0, S09413 => 1 },
           16  => { 34113 => 0, S09413 => 1 },
           17  => { 34113 => 0, S09413 => 1 },
         }, 'homework' );
is($g->roundMax, 2, 'max hw score per round');
is($g->totalMax, 22, 'maximum possible homework score');
is_deeply($g->hwforid(34113), [ (0) x 11 ], 'no hw score for Emile');
is_deeply($g->homework, { 34113 => 0, S09413 => 11 }, "Emile 0, Sophie 11");
is_deeply($g->homeworkPercent, { 34113 => 0, S09413 => 50 }, "Emile 0%, Sophie 50%");

# jigsaw
my $quizfile = "/home/$ENV{USER}/class/beans/t/emile/activities.yaml";

is_deeply( $g->jigsawConfig( 't/emile/exams/2/1'), $g->inspect('t/emile/exams/2/1/round.yaml'), 'Config file.');
is( $g->topic( 't/emile/exams/2/2', 'Brown' ), 'citrus', 'Topic of exam text');
is( $g->form( 't/emile/exams/3/2', 'Brown' ), 2, 'Form of exam text');
is( $g->quizfile( 't/emile/exams/1/1' ), $quizfile, 'Location of exam text');
is( $g->quizfile( 't/emile/exams/4/1' ), $quizfile, 'Location of exam text');
is_deeply($g->quiz( 't/emile/exams/4/1', 'Brown' ),
	$g->inspect('t/emile/activities.yaml')->{cars}->{jigsaw}->{2}->{quiz},
	'Quiz content');
is_deeply( $g->options( 't/emile/exams/1/1', 'Brown', 0 ), ['True','False'],
	'Options');
is( $g->qn( 't/emile/exams/1/2', 'Brown' ), 8, 'Number of exam questions' );
my ($emile, $rousseau, $sophie, $therese);
@$emile{1..9} = ( (0,1) x 4, 0 ); @$rousseau{1..9} = (0,0,1) x 3;
@$sophie{1..9} = ( (0,0,0,1) x 2, 0 ); @$therese{1..9} = ( (0) x 4, 1, (0) x 4);
is_deeply( $g->responses( 't/emile/exams/4/2', 'Brown' ),
	{ 34113 => $emile, 1 => $rousseau, S09413 => $sophie, 222 => $therese },
	"Question responses" );
is_deeply( $g->jigsawGroups( 't/emile/exams/2/2'),
	{ Brown => { A => 'Emile', B => 'Rousseau',
		C => 'Sophie', D => 'Therese' } }, "Jigsaw groups" );
is_deeply( $g->jigsawGroupMembers( 't/emile/exams/3/1', 'Brown' ),
	{ A => 'Emile', B => 'Rousseau', C => 'Sophie', D => 'Therese' },
	"Jigsaw groups" );
is_deeply( $g->idsbyRole( 't/emile/exams/2/1', 'Brown' ),
	[ 34113, 1, 'S09413', 222 ], 'Ids in array, in A-D role order' );
is_deeply( $g->jigsawGroupRole('t/emile/exams/1/2', 'Brown' ),
	{ Emile => 'A', Rousseau => 'B', Sophie => 'C', Therese => 'D' },
	 "Members' roles" );
is_deeply( $g->id2jigsawGroupRole('t/emile/exams/3/2', 'Brown' ),
	{ 34113 => 'A', 1 => 'B', S09413 => 'C', 222 => 'D' }, 'Id to role' );
is_deeply( $g->name2jigsawGroup('t/emile/exams/3/1', 'Rousseau'), [ 'Brown' ],
	'Name in which groups?');
is_deeply( $g->rawJigsawScores('t/emile/exams/3/1', 'Brown'),
	{ 1 => 4, 222 => 4, 34113 => 6, S09413 => 2 }, 'Raw scores');
is_deeply( $g->rawJigsawScores('t/emile/exams/4/2', 'Brown'),
	{ 34113 => 5, 1 => 5, S09413 => 3, 222 => 2 }, 'Jigsaw scores' );

# exams
is( $g->examdirs, qw{t/emile/exams}, 'examdirs' );
