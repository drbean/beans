use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Grades',
    license             => 'perl',
    dist_author         => 'Dr Bean <drbean, followed by the at mark (@), cpan, then a dot, and finally, org>',
    dist_version   => 0.07,
    dist_abstract => 
	    'Grading of a league of players (ie class of students.)',
    requires => {
	    'Moose' => 0,
	    'MooseX::Declare' => 0,
	    'MooseX::Getopt' => 0,
	    'List::Util' => 0,
	    'List::MoreUtils' => 0,
	    'YAML' => 0,
	    'Carp' => 0,
	    'POSIX' => 0,
    },
    build_requires => {
        'Test::More' => 0,
	'Pod::Coverage::Moose' => 0,
    },
    create_makefile_pl => 'traditional',
    script_files        => [qw(
                            script_files/grades
                            script_files/classwork
                            script_files/homework
                            script_files/scantron.pl
                            script_files/checkAnswers.pl
                                )],

    add_to_cleanup      => [ 'Grades-*' ],
    meta_merge => {
      resources => {
          bugtracker => 'http://rt.cpan.org/NoAuth/Bugs.html?Dist=Grades',
          repository  => 'http://svn.openfoundry.org/ttb/beans/'
		}
	},
);

$builder->create_build_script();
