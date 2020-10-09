#####################################################################
#
# the Perl::Tidy::VerticalAligner::Line class supplies an object to
# contain a single output line
#
#####################################################################

package Perl::Tidy::VerticalAligner::Line;
use strict;
use warnings;
our $VERSION = '20201001.01';

BEGIN {
    my $i = 0;
    use constant {
        _jmax_                      => $i++,
        _jmax_original_line_        => $i++,
        _rtokens_                   => $i++,
        _rfields_                   => $i++,
        _rfield_lengths_            => $i++,
        _rpatterns_                 => $i++,
        _indentation_               => $i++,
        _leading_space_count_       => $i++,
        _outdent_long_lines_        => $i++,
        _list_type_                 => $i++,
        _is_hanging_side_comment_   => $i++,
        _ralignments_               => $i++,
        _maximum_line_length_       => $i++,
        _rvertical_tightness_flags_ => $i++,
        _is_terminal_ternary_       => $i++,
        _is_terminal_else_          => $i++,
        _j_terminal_match_          => $i++,
        _is_forced_break_           => $i++,
        _end_group_                 => $i++,
    };
}

{

    ##use Carp;

    # Constructor may be called as a class method
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class         = $caller_is_obj || $caller;
        ##no strict "refs";
        my $self = bless [], $class;

        $self->[_jmax_]                      = $arg{jmax};
        $self->[_jmax_original_line_]        = $arg{jmax_original_line};
        $self->[_rtokens_]                   = $arg{rtokens};
        $self->[_rfields_]                   = $arg{rfields};
        $self->[_rfield_lengths_]            = $arg{rfield_lengths};
        $self->[_rpatterns_]                 = $arg{rpatterns};
        $self->[_indentation_]               = $arg{indentation};
        $self->[_leading_space_count_]       = $arg{leading_space_count};
        $self->[_outdent_long_lines_]        = $arg{outdent_long_lines};
        $self->[_list_type_]                 = $arg{list_type};
        $self->[_is_hanging_side_comment_]   = $arg{is_hanging_side_comment};
        $self->[_maximum_line_length_]       = $arg{maximum_line_length};
        $self->[_rvertical_tightness_flags_] = $arg{rvertical_tightness_flags};
        $self->[_is_terminal_ternary_]       = $arg{is_terminal_ternary};
        $self->[_is_terminal_else_]          = $arg{is_terminal_else};
        $self->[_j_terminal_match_]          = $arg{j_terminal_match};
        $self->[_is_forced_break_]           = $arg{is_forced_break};
        $self->[_end_group_]                 = $arg{end_group};

        $self->[_ralignments_] = [];

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

    sub get_jmax { return $_[0]->[_jmax_] }

    sub get_jmax_original_line {
        return $_[0]->[_jmax_original_line_];
    }
    sub get_rtokens        { return $_[0]->[_rtokens_] }
    sub get_rfields        { return $_[0]->[_rfields_] }
    sub get_rfield_lengths { return $_[0]->[_rfield_lengths_] }
    sub get_rpatterns      { return $_[0]->[_rpatterns_] }
    sub get_indentation    { return $_[0]->[_indentation_] }

    sub get_j_terminal_match {
        return $_[0]->[_j_terminal_match_];
    }

    sub set_j_terminal_match {
        my ( $self, $val ) = @_;
        $self->[_j_terminal_match_] = $val;
        return;
    }

    sub get_is_terminal_else {
        return $_[0]->[_is_terminal_else_];
    }

    sub get_is_terminal_ternary {
        return $_[0]->[_is_terminal_ternary_];
    }

    sub get_is_forced_break {
        return $_[0]->[_is_forced_break_];
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

    sub set_column {
        ## FIXME: does caller ever supply $val??
        my ( $self, $j, $val ) = @_;
        return $self->[_ralignments_]->[$j]->set_column($val);
    }

    sub get_alignment {
        my ( $self, $j ) = @_;
        return $self->[_ralignments_]->[$j];
    }
    sub get_alignments { return @{ $_[0]->[_ralignments_] } }

    sub get_column {
        my ( $self, $j ) = @_;
        my $col;
        my $alignment = $self->[_ralignments_]->[$j];
        if ( defined($alignment) ) {
            $col = $alignment->get_column();
        }
        return $col;
    }

    sub get_starting_column {
        my ( $self, $j ) = @_;
        my $col;
        my $alignment = $self->[_ralignments_]->[$j];
        if ( defined($alignment) ) {
            $col = $alignment->get_starting_column();
        }
        return $col;
    }

    sub increment_column {
        my ( $self, $k, $pad ) = @_;
        my $alignment = $self->[_ralignments_]->[$k];
        if ( defined($alignment) ) {
            $alignment->increment_column($pad);
        }
        return;
    }

    sub set_alignments {
        my ( $self, @args ) = @_;
        @{ $self->[_ralignments_] } = @args;
        return;
    }

    sub current_field_width {
        my ( $self, $j ) = @_;
        if ( $j == 0 ) {
            return $self->get_column($j);
        }
        else {
            return $self->get_column($j) - $self->get_column( $j - 1 );
        }
    }

    sub field_width_growth {
        my ( $self, $j ) = @_;
        return $self->get_column($j) - $self->get_starting_column($j);
    }

    sub starting_field_width {
        my ( $self, $j ) = @_;
        if ( $j == 0 ) {
            return $self->get_starting_column($j);
        }
        else {
            return $self->get_starting_column($j) -
              $self->get_starting_column( $j - 1 );
        }
    }

    sub increase_field_width {

        my ( $self, $j, $pad ) = @_;
        my $jmax = $self->get_jmax();
        for my $k ( $j .. $jmax ) {
            $self->increment_column( $k, $pad );
        }
        return;
    }

    sub get_available_space_on_right {
        my $jmax = $_[0]->get_jmax();
        return $_[0]->[_maximum_line_length_] - $_[0]->get_column($jmax);
    }

    sub set_jmax { my ( $self, $val ) = @_; $self->[_jmax_] = $val; return }

    sub set_jmax_original_line {
        my ( $self, $val ) = @_;
        $self->[_jmax_original_line_] = $val;
        return;
    }

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

    sub set_indentation {
        my ( $self, $val ) = @_;
        $self->[_indentation_] = $val;
        return;
    }

    sub set_leading_space_count {
        my ( $self, $val ) = @_;
        $self->[_leading_space_count_] = $val;
        return;
    }

    sub set_outdent_long_lines {
        my ( $self, $val ) = @_;
        $self->[_outdent_long_lines_] = $val;
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
        my ( $self, $j, $val ) = @_;
        $self->[_end_group_]->[$j] = $val;
        return;
    }

}

1;

