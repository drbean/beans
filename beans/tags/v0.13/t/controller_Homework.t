use strict;
use warnings;
use Test::More tests => 3;

BEGIN { use_ok 'Catalyst::Test', 'beans' }
BEGIN { use_ok 'beans::Controller::Homework' }

ok( request('/homework')->is_success, 'Request should succeed' );


