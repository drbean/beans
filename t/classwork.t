use strict;
use warnings;
use Test::More;

plan tests => 16;
plan skip_all => 'unset NO_TEST to enable this test' if $ENV{NO_TEST};

use lib 'lib';
use Bean;

my $l = Classwork->new(leagueId => 't/league');

is_deeply( $l->series, [qw/first second third fourth/], '4-session series' );


is_deeply($l->groupseries, {
	   first  => { Black  => [ 'Rien', 'Eric', 'Tina', 'Rick' ],
                       BlackBlack => [ 'Sophie', 'Brian' ],
                       Brown  => [ 'Adam', 'Jessica', 'Michelle2', 'Alan' ],
                       DarkBlue => [ 'Elvy', 'Elain', 'Vincent', 'Steven' ],
                       DarkGreen => [ 'David', 'Scott1', 'Steve', 'Vita' ],
                       Gray   => [ 'Scott2', 'Emma', 'Hua', 'Raise' ],
                       LightBlue => [ 'Sherry1', 'Fang', 'Seika', 'Icecream' ],
                       LightGreen => [ 'Vicky', 'Nancy', 'Karen', 'Lisa' ],
                       Orange => [ 'Judy', 'Dolores', 'Penny1', 'Jill1' ],
                       Pink   => [ 'Anny', 'Bella', 'Stella', 'YuChieh' ],
                       Purple => [ 'Jackie', 'Ivy', 'Loota', 'Wendy' ],
                       Red    => [ 'Michelle1', 'Alice', 'Justine', 'Helen' ],
                       White  => [ 'Penny2', 'Julie', 'Sherry2', 'Mary' ],
                       Yellow => [ 'Riva', 'Crystal', 'Jill2', 'Rita' ] },
           fourth => { Black  => [ 'Michelle2', 'Vicky', 'Rick', 'Fang' ],
                       BlackBlack => [ 'Hua', 'Julie', 'Jill2' ],
                       Brown  => [ 'Steven', 'Elvy', 'Justine', 'Icecream' ],
                       DarkBlue => [ 'Eric', 'Anny', 'Steve', 'Judy' ],
                       DarkGreen => [ 'Riva', 'Emma', 'Alan', 'Helen' ],
                       Gray   => [ 'Jessica', 'Vita', 'Penny1', 'Wendy' ],
                       LightBlue => [ 'Crystal', 'Seika', 'Rien', 'Lisa' ],
                       LightGreen => [ 'Jill1', 'Adam', 'Brian', 'Sophie' ],
                       Orange => [ 'Scott2', 'Michelle1', 'Raise', 'Ivy' ],
                       Pink   => [ 'Vincent', 'Sherry1', 'Karen', 'Tina' ],
                       Purple => [ 'Loota', 'Alice', 'Rita', 'Stella' ],
                       Red    => [ 'Sherry2', 'Dolores', 'YuChieh', 'Penny2' ],
                       White  => [ 'Nancy', 'Mary', 'David', 'Bella' ],
                       Yellow => [ 'Scott1', 'Jackie', 'Elain' ] },
           second => { Black  => [ 'Loota', 'Seika', 'Wendy', 'Fang' ],
                       BlackBlack => [ 'Mary', 'Scott1', 'Crystal' ],
                       Brown  => [ 'Ivy', 'Icecream', 'Vicky', 'Sherry1' ],
                       DarkBlue => [ 'Jackie', 'Nancy', 'Rien', 'Jill1' ],
		       DarkGreen => [ 'Michelle2', 'Eric', 'Karen', 'Penny1' ],
                       Gray   => [ 'Jessica', 'Tina', 'Lisa', 'Dolores' ],
                       LightBlue => [ 'Adam', 'Rick', 'Michelle1', 'Judy' ],
                       LightGreen => [ 'Brian', 'Scott2', 'Alice', 'Steven' ],
                       Orange => [ 'Sophie', 'Emma', 'Justine', 'Vincent' ],
                       Pink   => [ 'Jill2', 'Hua', 'Helen', 'Elain' ],
                       Purple => [ 'Rita', 'Raise', 'Penny2', 'Elvy' ],
                       Red    => [ 'YuChieh', 'Anny', 'Julie', 'Vita' ],
                       White  => [ 'Alan', 'Bella', 'Sherry2', 'Steve' ],
                       Yellow => [ 'Riva', 'Stella', 'David' ] },
           third  => { Black  => [ 'Crystal', 'Hua', 'Penny2', 'Lisa' ],
                       BlackBlack => [ 'Raise', 'Elain', 'Vicky' ],
                       Brown  => [ 'Steven', 'Icecream', 'Brian', 'Helen' ],
                       DarkBlue => [ 'Riva', 'Sherry1', 'Scott1', 'Jill2' ],
                       DarkGreen => [ 'Nancy', 'Michelle1', 'Penny1', 'Tina' ],
                       Gray   => [ 'Eric', 'Bella', 'Justine', 'Rick' ],
                       LightBlue => [ 'Scott2', 'Seika', 'Dolores', 'Alice' ],
                       LightGreen => [ 'David', 'Anny', 'Karen', 'Stella' ],
                       Orange => [ 'Michelle2', 'Alan', 'Jill1', 'Loota' ],
                       Pink   => [ 'YuChieh', 'Adam', 'Rita', 'Wendy' ],
                       Purple => [ 'Steve', 'Ivy', 'Judy', 'Mary' ],
                       Red    => [ 'Julie', 'Sherry2', 'Jackie', 'Vincent' ],
                       White  => [ 'Fang', 'Emma', 'Sophie', 'Vita' ],
                       Yellow => [ 'Jessica', 'Elvy', 'Rien' ] }
         }, 'groups for 4 sessions' ); 
is_deeply( $l->allfiles, [ qw[
	t/league/first/2.yaml t/league/first/3.yaml t/league/second/5.yaml
	t/league/second/6.yaml t/league/second/7.yaml t/league/second/8.yaml
	t/league/third/10.yaml t/league/third/11.yaml t/league/third/12.yaml
	t/league/fourth/14.yaml t/league/fourth/15.yaml t/league/fourth/16.yaml
	] ], 'all 12 files in 4 sessions');
is_deeply( $l->allweeks, [2,3,5,6,7,8,10..12,14..16], "all 12 weeks");
is_deeply( $l->lastweek, 16, "last week");
is_deeply( $l->groups('third'), {
                       Black  => [ 'Crystal', 'Hua', 'Penny2', 'Lisa' ],
                       BlackBlack => [ 'Raise', 'Elain', 'Vicky' ],
                       Brown  => [ 'Steven', 'Icecream', 'Brian', 'Helen' ],
                       DarkBlue => [ 'Riva', 'Sherry1', 'Scott1', 'Jill2' ],
                       DarkGreen => [ 'Nancy', 'Michelle1', 'Penny1', 'Tina' ],
                       Gray   => [ 'Eric', 'Bella', 'Justine', 'Rick' ],
                       LightBlue => [ 'Scott2', 'Seika', 'Dolores', 'Alice' ],
                       LightGreen => [ 'David', 'Anny', 'Karen', 'Stella' ],
                       Orange => [ 'Michelle2', 'Alan', 'Jill1', 'Loota' ],
                       Pink   => [ 'YuChieh', 'Adam', 'Rita', 'Wendy' ],
                       Purple => [ 'Steve', 'Ivy', 'Judy', 'Mary' ],
                       Red    => [ 'Julie', 'Sherry2', 'Jackie', 'Vincent' ],
                       White  => [ 'Fang', 'Emma', 'Sophie', 'Vita' ],
                       Yellow => [ 'Jessica', 'Elvy', 'Rien' ] }
	, "3rd-session groups" );
is_deeply( $l->weeks('fourth'), [14..16], 'weeks in fourth session');
is( $l->week2session(15), 'fourth', '15th week in fourth session');
is_deeply( $l->names2groups('fourth'), {
           Adam  => 'LightGreen', Alan  => 'DarkGreen', Alice => 'Purple',
           Anny  => 'DarkBlue', Bella => 'White', Brian => 'LightGreen',
           Crystal => 'LightBlue', David => 'White', Dolores => 'Red',
           Elain => 'Yellow', Elvy  => 'Brown', Emma  => 'DarkGreen',
           Eric  => 'DarkBlue', Fang  => 'Black', Helen => 'DarkGreen',
           Hua   => 'BlackBlack', Icecream => 'Brown', Ivy   => 'Orange',
           Jackie => 'Yellow', Jessica => 'Gray', Jill1 => 'LightGreen',
           Jill2 => 'BlackBlack', Judy  => 'DarkBlue', Julie => 'BlackBlack',
           Justine => 'Brown', Karen => 'Pink', Lisa  => 'LightBlue',
           Loota => 'Purple', Mary  => 'White', Michelle1 => 'Orange',
           Michelle2 => 'Black', Nancy => 'White', Penny1 => 'Gray',
           Penny2 => 'Red', Raise => 'Orange', Rick  => 'Black',
           Rien  => 'LightBlue', Rita  => 'Purple', Riva  => 'DarkGreen',
           Scott1 => 'Yellow', Scott2 => 'Orange', Seika => 'LightBlue',
           Sherry1 => 'Pink', Sherry2 => 'Red', Sophie => 'LightGreen',
           Stella => 'Purple', Steve => 'DarkBlue', Steven => 'Brown',
           Tina  => 'Pink', Vicky => 'Black', Vincent => 'Pink',
           Vita  => 'Gray', Wendy => 'Gray', YuChieh => 'Red'
         }, 'map names to groups in 4th session');
is( $l->name2group('third', 'Rick'), 'Gray', 'Which group is Rick in in 3th session');
is( $l->name2group('second', 'Nancy'), 'DarkBlue', 'Which group is Nancy in in 2th session');
is( eval{ $l->name2group('second', 'KarlMarx') }, undef, 'Which group is Karl Marx in in 2th session');
is_deeply( $l->merits(14), {
    Black  => 9, BlackBlack => 4, Brown  => 2, DarkBlue => 4, DarkGreen => 11,
    Gray   => 6, LightBlue => 1, LightGreen => 3, Orange => 6, Pink  => 6,
    Purple => 1, Red => 7, White  => 4, Yellow => 7
         }, "What points for good things gotten in Week 14");
is_deeply( $l->absences(2), {
    Black  => 1, BlackBlack => 2, Brown  => 2, DarkBlue => 1, DarkGreen => 2,
    Gray   => 1, LightBlue => 2, LightGreen => 1, Orange => 1, Pink   => 3,
    Purple => 1, Red    => 0, White  => 2, Yellow => 3
         }, "How many people absent in each group in week 2");
is_deeply( $l->tardies(2), {
   Black  => 1, BlackBlack => 2, Brown  => 0, DarkBlue => 1, DarkGreen => 0,
   Gray   => 1, LightBlue => 2, LightGreen => 1, Orange => 3, Pink   => 1,
   Purple => 1, Red    => 0, White  => 0, Yellow => 1 }, "How many people late in each group in week 2");
is( $l->payout('second'), 70, "If the total paid to players this week is 70, the average grade over the semester should be 80.");
