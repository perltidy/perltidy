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

our $VERSION = '20210111';

#    _column_          # the current column number
#    _saved_column_    # a place for temporary storage
my $i = 0;
use constant {
    _column_       => $i++,
    _saved_column_ => $i++,
};

sub new {
    my ( $class, $rarg ) = @_;
    my $self = bless [], $class;
    $self->[_column_]       = $rarg->{column};
    $self->[_saved_column_] = $rarg->{saved_column};
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

sub get_column {
    return $_[0]->[_column_];
}

sub increment_column {
    $_[0]->[_column_] += $_[1];
    return;
}

sub save_column {
    $_[0]->[_saved_column_] = $_[0]->[_column_];
    return;
}

sub restore_column {
    $_[0]->[_column_] = $_[0]->[_saved_column_];
    return;
}
} ## end of package VerticalAligner::Alignment
1;

