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
use English qw( -no_match_vars );
our $VERSION = '20230912';

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

{

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
        my $col_j  = 0;
        my $col_jm = 0;

        my $alignment_j = $self->{ralignments}->[$j];
        $col_j = $alignment_j->get_column() if defined($alignment_j);

        if ( $j > 0 ) {
            my $alignment_jm = $self->{ralignments}->[ $j - 1 ];
            $col_jm = $alignment_jm->get_column() if defined($alignment_jm);
        }
        return $col_j - $col_jm;
    } ## end sub current_field_width

    sub increase_field_width {

        my ( $self, $j, $pad ) = @_;
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
        my $jmax = $_[0]->{jmax};
        return $_[0]->{maximum_line_length} - $_[0]->get_column($jmax);
    }
}

1;
