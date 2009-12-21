package beans::Schema::League;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("leagues");
__PACKAGE__->add_columns(
  "id",
  { data_type => "VARCHAR", is_nullable => 0, size => 15 },
  "name",
  { data_type => "VARCHAR", is_nullable => 0, size => 25 },
  "field",
  { data_type => "VARCHAR", is_nullable => 0, size => 25 },
);
__PACKAGE__->set_primary_key("id");


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2008-08-26 18:19:13
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/5o//066/F807YLrv/lB0w

#
# Set relationships:
#

# belongs_to():
#   args:
#     1) Name of relationship, DBIC will create accessor with this name
#     2) Name of the model class referenced by this relationship
#     3) Column name in *this* table
# __PACKAGE__->belongs_to(genre => 'dic::Schema::Leaguegenre', 'id');
#__PACKAGE__->has_one(genre => 'dic::Schema::Leaguegenre',
#        { 'foreign.league' => 'self.id'});

# has_many():
#   args:
#     1) Name of relationship, DBIC will create accessor with this name
#     2) Name of the model class referenced by this relationship
#     3) Column name in *foreign* table
#__PACKAGE__->has_many(members => 'dic::Schema::Member', 'league');

# many_to_many():
#   args:
#     1) Name of relationship, DBIC will create accessor with this name
#     2) Name of has_many() relationship this many_to_many() is shortcut for
#     3) Name of belongs_to() relationship in model class of has_many() above 
#   You must already have the has_many() defined to use a many_to_many().
#__PACKAGE__->many_to_many(players => 'members', 'player');

# You can replace this text with custom content, and it will be preserved on regeneration
1;
