#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Bean::Counter' );
}

diag( "Testing Bean::Counter $Bean::Counter::VERSION, Perl $], $^X" );
