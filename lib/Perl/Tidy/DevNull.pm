#####################################################################
#
# The Perl::Tidy::DevNull class supplies a dummy print method
#
#####################################################################

package Perl::Tidy::DevNull;
use strict;
use warnings;
our $VERSION = '20220217';
sub new   { my $self = shift; return bless {}, $self }
sub print { return }
sub close { return }

1;

