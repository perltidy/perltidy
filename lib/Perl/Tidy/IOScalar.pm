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
our $VERSION = '20210111';

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    my $my_package = __PACKAGE__;
    print STDERR <<EOM;
======================================================================
Error detected in package '$my_package', version $VERSION
Received unexpected AUTOLOAD call for sub '$AUTOLOAD'
Called from package: '$pkg'  
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
}

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub new {
    my ( $package, $rscalar, $mode ) = @_;
    my $ref = ref $rscalar;
    if ( $ref ne 'SCALAR' ) {
        confess <<EOM;
------------------------------------------------------------------------
expecting ref to SCALAR but got ref to ($ref); trace follows:
------------------------------------------------------------------------
EOM

    }
    if ( $mode eq 'w' ) {
        ${$rscalar} = "";
        return bless [ $rscalar, $mode ], $package;
    }
    elsif ( $mode eq 'r' ) {

        # Convert a scalar to an array.
        # This avoids looking for "\n" on each call to getline
        #
        # NOTES: The -1 count is needed to avoid loss of trailing blank lines
        # (which might be important in a DATA section).
        my @array;
        if ( $rscalar && ${$rscalar} ) {

            #@array = map { $_ .= "\n" } split /\n/, ${$rscalar}, -1;
            @array = map { $_ . "\n" } split /\n/, ${$rscalar}, -1;

            # remove possible extra blank line introduced with split
            if ( @array && $array[-1] eq "\n" ) { pop @array }
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
}

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
}

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
}
sub close { return }
1;

