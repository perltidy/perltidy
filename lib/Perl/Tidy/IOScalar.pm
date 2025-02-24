#####################################################################
#
# This is a stripped down version of IO::Scalar
# Given a reference to a scalar, it supplies either:
# a getline method which reads lines (mode='r'), or
# a print method which reads lines (mode='w')
#
#####################################################################
package Perl::Tidy::IOScalar;
use strict;
use warnings;
use Carp;
our $VERSION = '20250214.02';

use constant DEVEL_MODE   => 0;
use constant EMPTY_STRING => q{};

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );

    # Originally there was a dummy sub close. All calls to it should have been
    # eliminated, but for safety we will check for them here.
    return 1 if ( $AUTOLOAD =~ /\bclose$/ && !DEVEL_MODE );
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

sub new {
    my ( $package, $rscalar, $mode ) = @_;
    my $ref = ref($rscalar);
    if ( $ref ne 'SCALAR' ) {
        confess <<EOM;
------------------------------------------------------------------------
expecting ref to SCALAR but got ref to ($ref); trace follows:
------------------------------------------------------------------------
EOM

    }
    if ( $mode eq 'w' ) {
        ${$rscalar} = EMPTY_STRING;
        return bless [ $rscalar, $mode ], $package;
    }
    elsif ( $mode eq 'r' ) {

        # Convert a scalar to an array.
        # This avoids looking for "\n" on each call to getline
        my @array;
        if ( $rscalar && ${$rscalar} ) {
            @array = split /^/, ${$rscalar};
        }
        my $i_next = 0;
        return bless [ \@array, $mode, $i_next ], $package;
    }
    else {
        confess <<EOM;
------------------------------------------------------------------------
expecting mode = 'r' or 'w' but got mode ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
} ## end sub new

sub getline {
    my $self = shift;
    my $mode = $self->[1];
    if ( $mode ne 'r' ) {
        confess <<EOM;
------------------------------------------------------------------------
getline call requires mode = 'r' but mode = ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
    my $i = $self->[2]++;
    return $self->[0]->[$i];
} ## end sub getline

sub print {
    my ( $self, $msg ) = @_;
    my $mode = $self->[1];
    if ( $mode ne 'w' ) {
        confess <<EOM;
------------------------------------------------------------------------
print call requires mode = 'w' but mode = ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
    ${ $self->[0] } .= $msg;
    return;
} ## end sub print
1;
