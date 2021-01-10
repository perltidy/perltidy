#####################################################################
#
# This is a stripped down version of IO::ScalarArray
# Given a reference to an array, it supplies either:
# a getline method which reads lines (mode='r'), or
# a print method which reads lines (mode='w')
#
# NOTE: this routine assumes that there aren't any embedded
# newlines within any of the array elements.  There are no checks
# for that.
#
#####################################################################
package Perl::Tidy::IOScalarArray;
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
    my ( $package, $rarray, $mode ) = @_;
    my $ref = ref $rarray;
    if ( $ref ne 'ARRAY' ) {
        confess <<EOM;
------------------------------------------------------------------------
expecting ref to ARRAY but got ref to ($ref); trace follows:
------------------------------------------------------------------------
EOM

    }
    if ( $mode eq 'w' ) {
        @{$rarray} = ();
        return bless [ $rarray, $mode ], $package;
    }
    elsif ( $mode eq 'r' ) {
        my $i_next = 0;
        return bless [ $rarray, $mode, $i_next ], $package;
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
getline requires mode = 'r' but mode = ($mode); trace follows:
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
print requires mode = 'w' but mode = ($mode); trace follows:
------------------------------------------------------------------------
EOM
    }
    push @{ $self->[0] }, $msg;
    return;
}
sub close { return }
1;

