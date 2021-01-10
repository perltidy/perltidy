#####################################################################
#
# the Perl::Tidy::VerticalAligner::Line class supplies an object to
# contain a single output line
#
#####################################################################

package Perl::Tidy::VerticalAligner::Line;
use strict;
use warnings;
our $VERSION = '20210111';

BEGIN {
    my $i = 0;
    use constant {
        _jmax_                      => $i++,
        _rtokens_                   => $i++,
        _rfields_                   => $i++,
        _rfield_lengths_            => $i++,
        _rpatterns_                 => $i++,
        _indentation_               => $i++,
        _leading_space_count_       => $i++,
        _outdent_long_lines_        => $i++,
        _list_seqno_                => $i++,
        _list_type_                 => $i++,
        _is_hanging_side_comment_   => $i++,
        _ralignments_               => $i++,
        _maximum_line_length_       => $i++,
        _rvertical_tightness_flags_ => $i++,
        _is_terminal_ternary_       => $i++,
        _j_terminal_match_          => $i++,
        _end_group_                 => $i++,
        _Kend_                      => $i++,
        _ci_level_                  => $i++,
        _imax_pair_                 => $i++,
    };
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

{

    ##use Carp;

    # Constructor may be called as a class method
    sub new {
        my ( $class, $ri ) = @_;
        my $self = bless [], $class;

        $self->[_jmax_]                      = $ri->{jmax};
        $self->[_rtokens_]                   = $ri->{rtokens};
        $self->[_rfields_]                   = $ri->{rfields};
        $self->[_rfield_lengths_]            = $ri->{rfield_lengths};
        $self->[_rpatterns_]                 = $ri->{rpatterns};
        $self->[_indentation_]               = $ri->{indentation};
        $self->[_leading_space_count_]       = $ri->{leading_space_count};
        $self->[_outdent_long_lines_]        = $ri->{outdent_long_lines};
        $self->[_list_type_]                 = $ri->{list_type};
        $self->[_list_seqno_]                = $ri->{list_seqno};
        $self->[_is_hanging_side_comment_]   = $ri->{is_hanging_side_comment};
        $self->[_maximum_line_length_]       = $ri->{maximum_line_length};
        $self->[_rvertical_tightness_flags_] = $ri->{rvertical_tightness_flags};
        $self->[_is_terminal_ternary_]       = $ri->{is_terminal_ternary};
        $self->[_j_terminal_match_]          = $ri->{j_terminal_match};
        $self->[_end_group_]                 = $ri->{end_group};
        $self->[_Kend_]                      = $ri->{Kend};
        $self->[_ci_level_]                  = $ri->{ci_level};
        $self->[_imax_pair_]                 = $ri->{imax_pair};

        $self->[_ralignments_] = [];

        return $self;
    }

    sub get_jmax { return $_[0]->[_jmax_] }

    sub get_rtokens        { return $_[0]->[_rtokens_] }
    sub get_rfields        { return $_[0]->[_rfields_] }
    sub get_rfield_lengths { return $_[0]->[_rfield_lengths_] }
    sub get_rpatterns      { return $_[0]->[_rpatterns_] }
    sub get_indentation    { return $_[0]->[_indentation_] }
    sub get_Kend           { return $_[0]->[_Kend_] }
    sub get_ci_level       { return $_[0]->[_ci_level_] }
    sub get_list_seqno     { return $_[0]->[_list_seqno_] }

    sub get_imax_pair { return $_[0]->[_imax_pair_] }

    sub set_imax_pair {
        my ( $self, $val ) = @_;
        $self->[_imax_pair_] = $val;
        return;
    }

    sub get_j_terminal_match {
        return $_[0]->[_j_terminal_match_];
    }

    sub set_j_terminal_match {
        my ( $self, $val ) = @_;
        $self->[_j_terminal_match_] = $val;
        return;
    }

    sub get_is_terminal_ternary {
        return $_[0]->[_is_terminal_ternary_];
    }

    sub get_leading_space_count {
        return $_[0]->[_leading_space_count_];
    }

    sub get_outdent_long_lines {
        return $_[0]->[_outdent_long_lines_];
    }
    sub get_list_type { return $_[0]->[_list_type_] }

    sub get_is_hanging_side_comment {
        return $_[0]->[_is_hanging_side_comment_];
    }

    sub get_rvertical_tightness_flags {
        return $_[0]->[_rvertical_tightness_flags_];
    }

    sub get_alignment {
        my ( $self, $j ) = @_;
        return $self->[_ralignments_]->[$j];
    }
    sub get_alignments { return @{ $_[0]->[_ralignments_] } }

    sub get_column {
        ##my ( $self, $j ) = @_;
        my $alignment = $_[0]->[_ralignments_]->[ $_[1] ];
        return unless defined($alignment);
        return $alignment->get_column();
    }

    sub set_alignments {
        my ( $self, @args ) = @_;
        @{ $self->[_ralignments_] } = @args;
        return;
    }

    sub current_field_width {
        my ( $self, $j ) = @_;
        my $col_j  = 0;
        my $col_jm = 0;

        my $alignment_j = $self->[_ralignments_]->[$j];
        $col_j = $alignment_j->get_column() if defined($alignment_j);

        if ( $j > 0 ) {
            my $alignment_jm = $self->[_ralignments_]->[ $j - 1 ];
            $col_jm = $alignment_jm->get_column() if defined($alignment_jm);
        }
        return $col_j - $col_jm;
    }

    sub increase_field_width {

        my ( $self, $j, $pad ) = @_;
        my $jmax = $self->[_jmax_];
        foreach ( $j .. $jmax ) {
            my $alignment = $self->[_ralignments_]->[$_];
            if ( defined($alignment) ) {
                $alignment->increment_column($pad);
            }
        }
        return;
    }

    sub get_available_space_on_right {
        my $jmax = $_[0]->[_jmax_];
        return $_[0]->[_maximum_line_length_] - $_[0]->get_column($jmax);
    }

    sub set_jmax { my ( $self, $val ) = @_; $self->[_jmax_] = $val; return }

    sub set_rtokens {
        my ( $self, $val ) = @_;
        $self->[_rtokens_] = $val;
        return;
    }

    sub set_rfields {
        my ( $self, $val ) = @_;
        $self->[_rfields_] = $val;
        return;
    }

    sub set_rfield_lengths {
        my ( $self, $val ) = @_;
        $self->[_rfield_lengths_] = $val;
        return;
    }

    sub set_rpatterns {
        my ( $self, $val ) = @_;
        $self->[_rpatterns_] = $val;
        return;
    }

    sub set_list_type {
        my ( $self, $val ) = @_;
        $self->[_list_type_] = $val;
        return;
    }

    sub set_is_hanging_side_comment {
        my ( $self, $val ) = @_;
        $self->[_is_hanging_side_comment_] = $val;
        return;
    }

    sub set_alignment {
        my ( $self, $j, $val ) = @_;
        $self->[_ralignments_]->[$j] = $val;
        return;
    }

    sub get_end_group { return $_[0]->[_end_group_] }

    sub set_end_group {
        my ( $self, $val ) = @_;
        $self->[_end_group_] = $val;
        return;
    }
}

1;
