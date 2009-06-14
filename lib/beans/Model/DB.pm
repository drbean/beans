package beans::Model::DB;

use strict;
use base 'Catalyst::Model::DBIC::Schema';

use Catalyst;
use beans;

my $dicpath = beans->config->{dicpath};
my $name = beans->config->{database};
my $db = beans->path_to( $dicpath, 'db', $name );

my $connect_info;
if ( $^O eq 'linux' ) { $connect_info = [ "dbi:SQLite:$db", '', '', ]; }
# if ( $^O eq 'linux' ) { $connect_info = [ "dbi:SQLite:/home/drbean/dic/db/week2", '', '', ]; }

elsif ( $^O eq 'MSWin32' ) {
	$connect_info = [ "dbi:ODBC:Driver={SQL Server};Server=LocalServer;Network=DBMSSOCN;Address=127.0.0.1;Database=$name" ];
	# $connect_info = [ "dbi:ODBC:DSN=dictation" ];
}

__PACKAGE__->config(
    schema_class => 'beans::Schema',
    connect_info => $connect_info,
    # connect_info => ['dbi:SQLite:db/demo','','']
);

=head1 NAME

beans::Model::DB - Catalyst DBIC Schema Model
=head1 SYNOPSIS

See L<dic>

=head1 DESCRIPTION

L<Catalyst::Model::DBIC::Schema> Model using schema L<dic::Schema>

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
