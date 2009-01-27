package Script;
use Moose;
with 'MooseX::Getopt';

has 'man' => (is => 'ro', isa => 'Bool');
has 'help' => (is => 'ro', isa => 'Bool');
has 'league' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'l',);
has 'player' => (traits => ['Getopt'], is => 'ro', isa => 'Str',
		cmd_aliases => 'p',);

package League;
use Moose;
use YAML qw/LoadFile DumpFile/;
use List::MoreUtils qw/any/;

# extends 'Script';

has 'leagueId' => (is => 'ro', isa => 'Str', required => 1);
has 'yaml' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_yaml {
		my ($instance) = @_;
		my $league = $instance->leagueId;
		LoadFile "$league/league.yaml";
}

has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $data = $self->yaml;
	$data->{league};
}

has 'hwdir' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_hwdir {
	my $self = shift;
	my $league = $self->leagueId;
	my $data = $self->yaml;
	my $hwdir = $data->{hw} || "$league/homework"
}
has 'rounds' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_rounds {
	my $self = shift;
	my $hwdir = $self->hwdir;
	my @hw = glob "$hwdir/*.yaml";
	[ sort {$a<=>$b} map m/^$hwdir\/(\d+)\.yaml$/, @hw ];
}
has 'hw' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_hw {
	my $self = shift;
	my $hwdir = $self->hwdir;
	my $rounds = $self->rounds;
	+{ map { $_ => LoadFile "$hwdir/$_.yaml" } @$rounds };
}
has 'hwMax' => (is => 'ro', isa => 'Int', lazy => 1, default =>
				sub { shift->yaml->{hwMax} } );
has 'totalMax' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_totalMax {
	my $self = shift;
	my $rounds = $self->rounds;
	my $hwMax = $self->hwMax;
	$hwMax * @$rounds;
}
has 'totalScores' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_totalScores {
	my $self = shift;
	my $hwdir = $self->hwdir;
	LoadFile "$hwdir/total.yaml";
}
has 'totalPercent' => (is => 'ro', isa => 'HashRef', lazy_build => 1);
sub _build_totalPercent {
	my $self = shift;
	my $hwdir = $self->hwdir;
	LoadFile "$hwdir/percent.yaml";
}
has 'members' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_members {
	my $self = shift;
	my $data = $self->yaml;
	my @members = sort { $a->{id} cmp $b->{id} } @{$data->{member}};
	[ map { Player->new(league => $self, id=>$_->{id},name=>$_->{name})
			} @members ];
	# $data->{member};
}

sub is_member {
	my $self = shift;
	my $id = shift;
	my $data = $self->yaml;
	any { $_->{id} eq $id } @{$data->{member}};
}

sub save {
	my $self = shift;
	DumpFile shift(), shift();
}
	


package Player;
use Moose;
# extends 'League';
use List::MoreUtils qw/firstval/;
use List::Util qw/sum/;
use POSIX;

has 'league' => (is => 'ro', isa => 'League', required => 1);
has 'id' => (is => 'ro', isa => 'Str', required => 1);
has 'name' => (is => 'ro', isa => 'Str', lazy_build => 1);
sub _build_name {
	my $self = shift;
	my $league = $self->league;
	my $id = $self->id;
	my $members = $league->members;
	my $member = firstval { $_->id eq $id } @$members;
	$member->name;
}
has 'Chinese' => (is => 'ro', isa => 'Str');
has 'grades' => (is => 'ro', isa => 'ArrayRef', lazy_build => 1);
sub _build_grades {
	my $self = shift;
	my $id = $self->id;
	my $league = $self->league;
	my $hw = $league->hw;
	my $rounds = $league->rounds;
	[ map { $hw->{$_}->{$id} } @$rounds ];
}
has 'total' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_total {
	my $self = shift;
	my $grades = $self->grades;
	sum @$grades;
}
has 'percent' => (is => 'ro', isa => 'Int', lazy_build => 1);
sub _build_percent {
	my $self = shift;
	my $grade = $self->total;
	my $league = $self->league;
	my $totalMax = $league->totalMax;
	floor (100 * $grade / $totalMax);
}

1;

__END__

=head1 NAME

cumulative - Add player results in individual rounds to get a cumulative total and show present standing

=head1 SYNOPSIS

hwtotal [options] 

Options:

--help            This help message

--man            A man page

--league m/j	The league whose results these are

=head1 OPTIONS

=over 8

=item B<-league>

The league to which the redeemer belongs

=back

=head1 DESCRIPTION

B<hwtotal> tallies individuals' scores in the files in the hw directory recorded in league.yaml. It stores the total in cumulative.yaml, in the same directory.

=cut

