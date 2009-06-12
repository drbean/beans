use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Bean::Counter',
    license             => 'perl',
    dist_author         => 'Dr Bean <drbean at (@] cpan dot [.) org>',
    dist_version   => 0.01,
    dist_abstract => 
	    'Ongoing grading of a league of players (ie class of students.)',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Bean-Counter-*' ],
    meta_merge => {
      resources => {
          bugtracker => 'http://rt.cpan.org/NoAuth/Bugs.html?Dist=Bean::Counter',
          repository  => 'http://svn.openfoundry.org/ttb/beans/'
		}
	},
    script_files	=>  [qw[
    				script_files/homework
    				script_files/classwork
				script_files/grades]],
);
# script_files/deposit script_files/beans script_files/hwtotal script_files/checkbeans script_files/id2name

$builder->create_build_script();
