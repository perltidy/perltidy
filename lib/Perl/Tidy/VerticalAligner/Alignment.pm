#####################################################################
#
# the Perl::Tidy::VerticalAligner::Alignment class holds information
# on a single column being aligned
#
#####################################################################

package Perl::Tidy::VerticalAligner::Alignment;
use strict;
use warnings;

our $VERSION = '20250912.02';

sub new {

    my ( $class, $rhash ) = @_;

    # Create a new VerticalAligner::Alignment object
    # Given:
    #   $rhash = ref to hash for an alignment column
    # There are just two values in the hash:
    #   {column}       is the alignment column
    #   {saved_column} is used to save a column

    my $self = bless $rhash, $class;
    return $self;
} ## end sub new

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    my $my_package = __PACKAGE__;
    print {*STDERR} <<EOM;
======================================================================
Error detected in package '$my_package', version $VERSION
Received unexpected AUTOLOAD call for sub '$AUTOLOAD'
Called from package: '$pkg'
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
} ## end sub AUTOLOAD

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub get_column {
    my $self = shift;
    return $self->{'column'};
}

sub increment_column {
    my ( $self, $pad ) = @_;
    $self->{'column'} += $pad;
    return;
}

sub save_column {
    my $self = shift;
    $self->{'saved_column'} = $self->{'column'};
    return;
}

sub restore_column {
    my $self = shift;
    $self->{'column'} = $self->{'saved_column'};
    return;
}
1;
