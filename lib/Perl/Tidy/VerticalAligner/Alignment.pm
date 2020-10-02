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

our $VERSION = '20201001';

#    _column          # the current column number
#    _starting_column # column number when created
#    _saved_column    # a place for temporary storage

sub new {
    my ( $caller, %arg ) = @_;
    my $caller_is_obj = ref($caller);
    my $class         = $caller_is_obj || $caller;
    my $self          = bless {}, $class;
    $self->{_column}          = $arg{column};
    $self->{_starting_column} = $arg{starting_column};
    $self->{_saved_column}    = $arg{saved_column};
    if ( !defined( $self->{_starting_column} ) ) {
        $self->{_starting_column} = $self->{_column};
    }
    return $self;
}

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~/\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    print STDERR <<EOM;
    ======================================================================
    Unexpected call to Autoload looking for sub $AUTOLOAD
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

sub get_column { my $self = shift; return $self->{_column} }

sub get_starting_column {
    my $self = shift;
    return $self->{_starting_column};
}

sub set_column { my ( $self, $val ) = @_; $self->{_column} = $val; return }

sub set_starting_column {
    my ( $self, $val ) = @_;
    $self->{_starting_column} = $val;
    return;
}

sub increment_column {
    my ( $self, $val ) = @_;
    $self->{_column} += $val;
    return;
}

sub save_column {
    my $self = shift;
    $self->{_saved_column} = $self->{_column};
    return;
}

sub restore_column {
    my $self = shift;
    $self->{_column} = $self->{_saved_column};
    return;
}
} ## end of package VerticalAligner::Alignment
1;

