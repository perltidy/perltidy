#####################################################################
#
# The Perl::Tidy::VerticalAligner::Line class supplies an object to
# contain a single output line.  It allows manipulation of the
# alignment columns on that line.
#
#####################################################################

package Perl::Tidy::VerticalAligner::Line;
use strict;
use warnings;

our $VERSION = '20250214.02';
use English qw( -no_match_vars );

{
    # List of hash keys to prevent -duk from listing them.
    my @unique_hash_keys_uu = qw( maximum_line_length );
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
} ## end sub AUTOLOAD

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

# Constructor may be called as a class method
sub new {
    my ( $class, $ri ) = @_;
    my $self = bless $ri, $class;
    return $self;
}

sub get_column {
    my ( $self, $j ) = @_;
    my $alignment = $self->{ralignments}->[$j];
    return unless defined($alignment);
    return $alignment->get_column();
} ## end sub get_column

sub current_field_width {
    my ( $self, $j ) = @_;

    # Return number of columns of space between alignments $j and $j-1

    my $alignment_j = $self->{ralignments}->[$j];
    my $col_j       = defined($alignment_j) ? $alignment_j->get_column() : 0;
    return $col_j if ( $j == 0 );

    my $alignment_jm = $self->{ralignments}->[ $j - 1 ];
    my $col_jm       = defined($alignment_jm) ? $alignment_jm->get_column() : 0;
    return $col_j - $col_jm;

} ## end sub current_field_width

sub increase_field_width {

    my ( $self, $j, $pad ) = @_;

    # Increase the width of alignment field $j by $pad spaces
    my $jmax = $self->{jmax};
    foreach ( $j .. $jmax ) {
        my $alignment = $self->{ralignments}->[$_];
        if ( defined($alignment) ) {
            $alignment->increment_column($pad);
        }
    }
    return;
} ## end sub increase_field_width

sub get_available_space_on_right {
    my $self = shift;
    my $jmax = $self->{jmax};
    return $self->{maximum_line_length} - $self->get_column($jmax);
}
1;
