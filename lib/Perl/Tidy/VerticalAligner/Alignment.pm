#####################################################################
#
# the Perl::Tidy::VerticalAligner::Alignment class holds information
# on a single column being aligned
#
#####################################################################
package Perl::Tidy::VerticalAligner::Alignment;
use strict;
use warnings;

{ #<<< A non-indenting brace

our $VERSION = '20230912';

sub new {
    my ( $class, $rarg ) = @_;
    my $self = bless $rarg, $class;
    return $self;
}

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
}

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub get_column {
    return $_[0]->{'column'};
}

sub increment_column {
    $_[0]->{'column'} += $_[1];

    return;
}

sub save_column {
    $_[0]->{'saved_column'} = $_[0]->{'column'};
    return;
}

sub restore_column {
    $_[0]->{'column'} = $_[0]->{'saved_column'};
    return;
}
} ## end of package VerticalAligner::Alignment
1;
