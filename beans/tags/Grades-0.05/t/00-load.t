#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Bean' );
}

diag( "Testing Bean $Bean::VERSION, Perl $], $^X" );
