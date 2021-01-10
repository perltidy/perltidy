#####################################################################
#
# The Perl::Tidy::LineBuffer class supplies a 'get_line()'
# method for returning the next line to be parsed, as well as a
# 'peek_ahead()' method
#
# The input parameter is an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineBuffer;
use strict;
use warnings;
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

    my ( $class, $line_source_object ) = @_;

    return bless {
        _line_source_object => $line_source_object,
        _rlookahead_buffer  => [],
    }, $class;
}

sub peek_ahead {
    my ( $self, $buffer_index ) = @_;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};
    if ( $buffer_index < scalar( @{$rlookahead_buffer} ) ) {
        $line = $rlookahead_buffer->[$buffer_index];
    }
    else {
        $line = $line_source_object->get_line();
        push( @{$rlookahead_buffer}, $line );
    }
    return $line;
}

sub get_line {
    my $self               = shift;
    my $line               = undef;
    my $line_source_object = $self->{_line_source_object};
    my $rlookahead_buffer  = $self->{_rlookahead_buffer};

    if ( scalar( @{$rlookahead_buffer} ) ) {
        $line = shift @{$rlookahead_buffer};
    }
    else {
        $line = $line_source_object->get_line();
    }
    return $line;
}
1;

