use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Bean::Counter',
    license             => 'perl',
    dist_author         => 'Dr Bean <drbean@cpan.org>',
    dist_version   => 0.01,
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Bean-Counter-*' ],
    script_files	=>  [qw[script_files/beans script_files/hwtotal
    				script_files/cumulative
    				script_files/classwork
    				script_files/checkbeans
    				script_files/deposit
				script_files/grades script_files/id2name]],
);

$builder->create_build_script();
