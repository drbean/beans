#!/usr/bin/perl

# Created: 西元2010年04月04日 18時44分24秒
# Last Edit: 2010  4月 04, 21時14分18秒
# $Id$

=head1 NAME

scantron.pl - A YAML form to enter group's responses to jigsaw questions

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

use strict;
use warnings;
use List::MoreUtils qw/all/;

use YAML qw/Bless Dump/;
use Grades;
use Cwd; use File::Basename;

=head1 SYNOPSIS

scantron.pl -l emile -r 3 > exams/3/response.yaml

=cut

my $scantron = Grades::Script->new_with_options;
my $id = $scantron->league || basename( getcwd );
my $exam = $scantron->round;


=head1 DESCRIPTION

Makes it easy to transfer from paper form in ttb/forms/compcomp/response.tex, to exams/3/response.yaml. Also use nn, nun autogroups (:do nn User, :do nun User) for one-touch data entry.

=cut

my $league = League->new( id => $id );
my $grades = Grades->new( league => $league );

my $members = $league->members;

my %members = map { $_->{name} => $_ } @$members;
my $groups = $grades->jigsawGroups( $exam );

my $response;
for my $group ( keys %$groups ) {
	my $idsbyRole = $grades->idsbyRole( $exam, $group);
	warn "Only @$idsbyRole in $group," if not all { defined } @$idsbyRole;
	my $qn = $grades->qn( $exam, $group );
	my $groupresponse = $response->{$group};
	my %questions; @questions{1..$qn } = ( undef ) x $qn;
	@$groupresponse{@$idsbyRole} = ( \%questions ) x @$idsbyRole;
	$response->{$group} = $groupresponse;
	Bless( $response->{$group}->{$_} )->keys( [1..$qn] ) for @$idsbyRole;
	Bless( $response->{$group} )->keys( $idsbyRole );
}
$YAML::UseAliases = 0;
print Dump $response;

=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2010 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of scantron.pl

# vim: set ts=8 sts=4 sw=4 noet:
