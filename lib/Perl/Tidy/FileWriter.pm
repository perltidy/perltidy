#####################################################################
#
# The Perl::Tidy::FileWriter class writes the output file created
# by the formatter. It receives each output line and performs some
# important monitoring services. These include:
#
# - Verifying that lines do not go out with tokens in the wrong order
# - Checking for obvious iteration convergence when all output tokens
#   match all input tokens
# - Keeping track of consecutive blank and non-blank lines
# - Looking for line lengths which exceed the maximum requested length
# - Reporting results to the log file
#
#####################################################################

package Perl::Tidy::FileWriter;
use strict;
use warnings;
our $VERSION = '20250214.02';
use Carp;

use constant DEVEL_MODE   => 0;
use constant EMPTY_STRING => q{};

# A limit on message length when a fault is detected
use constant LONG_MESSAGE => 256;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

my @unique_hash_keys_uu = qw( indent-columns );

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

BEGIN {

    # Array index names for variables.
    # Do not combine with other BEGIN blocks (c101).
    my $i = 0;
    use constant {
        _logger_object_               => $i++,
        _rOpts_                       => $i++,
        _output_line_number_          => $i++,
        _consecutive_blank_lines_     => $i++,
        _consecutive_nonblank_lines_  => $i++,
        _consecutive_new_blank_lines_ => $i++,
        _first_line_length_error_     => $i++,
        _max_line_length_error_       => $i++,
        _last_line_length_error_      => $i++,
        _first_line_length_error_at_  => $i++,
        _max_line_length_error_at_    => $i++,
        _last_line_length_error_at_   => $i++,
        _line_length_error_count_     => $i++,
        _max_output_line_length_      => $i++,
        _max_output_line_length_at_   => $i++,
        _rK_checklist_                => $i++,
        _K_arrival_order_matches_     => $i++,
        _K_sequence_error_msg_        => $i++,
        _K_last_arrival_              => $i++,
        _save_logfile_                => $i++,
        _routput_string_              => $i++,
        _input_stream_name_           => $i++,
    };
} ## end BEGIN

sub Die {
    my ($msg) = @_;
    Perl::Tidy::Die($msg);
    croak "unexpected return from Perl::Tidy::Die";
}

sub Fault {
    my ( $self, $msg ) = @_;

    # This routine is called for errors that really should not occur
    # except if there has been a bug introduced by a recent program change.
    # Please add comments at calls to Fault to explain why the call
    # should not occur, and where to look to fix it.
    my ( $package0_uu, $filename0_uu, $line0,    $subroutine0_uu ) = caller(0);
    my ( $package1_uu, $filename1,    $line1,    $subroutine1 )    = caller(1);
    my ( $package2_uu, $filename2_uu, $line2_uu, $subroutine2 )    = caller(2);
    my $pkg = __PACKAGE__;

    # Catch potential error of Fault not called as a method
    my $input_stream_name;
    if ( !ref($self) ) {
        $input_stream_name = "(UNKNOWN)";
        $msg               = "Fault not called as a method - please fix\n";
        if ( $self && length($self) < LONG_MESSAGE ) { $msg .= $self }
        $self = undef;
    }
    else {
        $input_stream_name = $self->[_input_stream_name_];
    }

    Die(<<EOM);
==============================================================================
While operating on input stream with name: '$input_stream_name'
A fault was detected at line $line0 of sub '$subroutine1'
in file '$filename1'
which was called from line $line1 of sub '$subroutine2'
Message: '$msg'
This is probably an error introduced by a recent programming change.
$pkg reports VERSION='$VERSION'.
==============================================================================
EOM

    croak "unexpected return from sub Die";
} ## end sub Fault

sub warning {
    my ( $self, $msg ) = @_;

    # log a warning message from any caller
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) { $logger_object->warning($msg); }
    return;
} ## end sub warning

sub write_logfile_entry {
    my ( $self, $msg ) = @_;
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->write_logfile_entry($msg);
    }
    return;
} ## end sub write_logfile_entry

sub new {
    my ( $class, $line_sink_object, $rOpts, $logger_object ) = @_;

    my $self = [];
    bless $self, $class;
    $self->[_logger_object_]               = $logger_object;
    $self->[_rOpts_]                       = $rOpts;
    $self->[_output_line_number_]          = 1;
    $self->[_consecutive_blank_lines_]     = 0;
    $self->[_consecutive_nonblank_lines_]  = 0;
    $self->[_consecutive_new_blank_lines_] = 0;
    $self->[_first_line_length_error_]     = 0;
    $self->[_max_line_length_error_]       = 0;
    $self->[_last_line_length_error_]      = 0;
    $self->[_first_line_length_error_at_]  = 0;
    $self->[_max_line_length_error_at_]    = 0;
    $self->[_last_line_length_error_at_]   = 0;
    $self->[_line_length_error_count_]     = 0;
    $self->[_max_output_line_length_]      = 0;
    $self->[_max_output_line_length_at_]   = 0;
    $self->[_rK_checklist_]                = [];
    $self->[_K_arrival_order_matches_]     = 0;
    $self->[_K_sequence_error_msg_]        = EMPTY_STRING;
    $self->[_K_last_arrival_]              = -1;
    $self->[_save_logfile_] =
      defined($logger_object) && $logger_object->get_save_logfile();

    # '$line_sink_object' is a SCALAR ref which receives the lines.
    my $ref = ref($line_sink_object);
    if ( !$ref ) {
        $self->Fault("FileWriter expects line_sink_object to be a ref\n");
    }
    elsif ( $ref eq 'SCALAR' ) {
        $self->[_routput_string_] = $line_sink_object;
    }
    else {
        my $str = $ref;
        if ( length($str) > 63 ) { $str = substr( $str, 0, 60 ) . '...' }
        $self->Fault(<<EOM);
FileWriter expects 'line_sink_object' to be ref to SCALAR but it is ref to:
$str
EOM
    }

    my $input_stream_name = EMPTY_STRING;
    if ($logger_object) {
        $input_stream_name = $logger_object->get_input_stream_name();
    }
    $self->[_input_stream_name_] = $input_stream_name;

    return $self;
} ## end sub new

sub setup_convergence_test {
    my ( $self, $rlist ) = @_;

    # Setup the convergence test,

    # Given:
    #  $rlist = a reference to a list of line-ending token indexes 'K' of
    #  the input stream. We will compare these with the line-ending token
    #  indexes of the output stream. If they are identical, then we have
    #  convergence.
    if ( @{$rlist} ) {

        # We are going to destroy the list, so make a copy and put in
        # reverse order so we can pop values as they arrive
        my @list = @{$rlist};
        if ( $list[0] < $list[-1] ) {
            @list = reverse @list;
        }
        $self->[_rK_checklist_] = \@list;
    }

    # We will zero this flag on any error in arrival order:
    $self->[_K_arrival_order_matches_] = 1;
    $self->[_K_sequence_error_msg_]    = EMPTY_STRING;
    $self->[_K_last_arrival_]          = -1;
    return;
} ## end sub setup_convergence_test

sub get_convergence_check {
    my ($self) = @_;

    # converged if:
    # - all expected indexes arrived
    # - and in correct order
    return !@{ $self->[_rK_checklist_] }
      && $self->[_K_arrival_order_matches_];

} ## end sub get_convergence_check

sub get_output_line_number {
    my $self = shift;
    return $self->[_output_line_number_];
}

sub decrement_output_line_number {
    my $self = shift;
    $self->[_output_line_number_]--;
    return;
}

sub get_consecutive_nonblank_lines {
    my $self = shift;
    return $self->[_consecutive_nonblank_lines_];
}

sub get_consecutive_blank_lines {
    my $self = shift;
    return $self->[_consecutive_blank_lines_];
}

sub reset_consecutive_blank_lines {
    my $self = shift;
    $self->[_consecutive_blank_lines_] = 0;
    return;
}

sub want_blank_line {
    my $self = shift;
    if ( !$self->[_consecutive_blank_lines_] ) {
        $self->write_blank_code_line();
    }
    return;
} ## end sub want_blank_line

sub require_blank_code_lines {
    my ( $self, $count ) = @_;

    # Given:
    #   $count = number of blank lines to write
    # Write out $count blank lines regardless of the value of -mbl
    # unless -mbl=0.  This allows extra blank lines to be written for subs and
    # packages even with the default -mbl=1
    my $need   = $count - $self->[_consecutive_blank_lines_];
    my $rOpts  = $self->[_rOpts_];
    my $forced = $rOpts->{'maximum-consecutive-blank-lines'} > 0;
    foreach ( 0 .. $need - 1 ) {
        $self->write_blank_code_line($forced);
    }
    return;
} ## end sub require_blank_code_lines

sub write_blank_code_line {
    my ( $self, ($forced) ) = @_;

    # Write a blank line of code, given:
    #  $forced = optional flag which, if set, forces the blank line
    #    to be written. This allows the -mbl flag to be temporarily
    #    exceeded.

    my $rOpts = $self->[_rOpts_];
    return
      if (!$forced
        && $self->[_consecutive_blank_lines_] >=
        $rOpts->{'maximum-consecutive-blank-lines'} );

    $self->[_consecutive_nonblank_lines_] = 0;

    # Balance old blanks against new (forced) blanks instead of writing them.
    # This fixes case b1073.
    if ( !$forced && $self->[_consecutive_new_blank_lines_] > 0 ) {
        $self->[_consecutive_new_blank_lines_]--;
        return;
    }

    ${ $self->[_routput_string_] } .= "\n";

    $self->[_output_line_number_]++;
    $self->[_consecutive_blank_lines_]++;
    $self->[_consecutive_new_blank_lines_]++ if ($forced);

    return;
} ## end sub write_blank_code_line

use constant MAX_PRINTED_CHARS => 80;

sub write_code_line {
    my ( $self, $str, $K ) = @_;

    # Write a line of code, given
    #  $str = the line of code
    #  $K   = an optional check integer which, if if given, must
    #       increase monotonically. This was added to catch cache
    #       sequence errors in the vertical aligner.

    $self->[_consecutive_blank_lines_]     = 0;
    $self->[_consecutive_new_blank_lines_] = 0;
    $self->[_consecutive_nonblank_lines_]++;
    $self->[_output_line_number_]++;

    ${ $self->[_routput_string_] } .= $str;

    if ( $self->[_save_logfile_] ) { $self->check_line_lengths($str) }

    #----------------------------
    # Convergence and error check
    #----------------------------
    if ( defined($K) ) {

        # Convergence check: we are checking if all defined K values arrive in
        # the order which was defined by the caller.  Quit checking if any
        # unexpected K value arrives.
        if ( $self->[_K_arrival_order_matches_] ) {
            my $Kt = pop @{ $self->[_rK_checklist_] };
            if ( !defined($Kt) || $Kt != $K ) {
                $self->[_K_arrival_order_matches_] = 0;
            }
        }

        # Check for out-of-order arrivals of index K. The K values are the
        # token indexes of the last token of code lines, and they should come
        # out in increasing order.  Otherwise something is seriously wrong.
        # Most likely a recent programming change to VerticalAligner.pm has
        # caused lines to go out in the wrong order.  This could happen if
        # either the cache or buffer that it uses are emptied in the wrong
        # order.
        if ( $K < $self->[_K_last_arrival_]
            && !$self->[_K_sequence_error_msg_] )
        {
            my $K_prev = $self->[_K_last_arrival_];

            chomp $str;
            if ( length($str) > MAX_PRINTED_CHARS ) {
                $str = substr( $str, 0, MAX_PRINTED_CHARS ) . "...";
            }

            my $msg = <<EOM;
Lines have arrived out of order in sub 'write_code_line'
as detected by token index K=$K arriving after index K=$K_prev in the following line:
$str
This is probably due to a recent programming change and needs to be fixed.
EOM

            # Always die during development, this needs to be fixed
            if (DEVEL_MODE) { $self->Fault($msg) }

            # Otherwise warn if string is not empty (added for b1378)
            $self->warning($msg) if ( length($str) );

            # Only issue this warning once
            $self->[_K_sequence_error_msg_] = $msg;

        }
        $self->[_K_last_arrival_] = $K;
    }
    return;
} ## end sub write_code_line

sub write_line {
    my ( $self, $str ) = @_;

    # Write a line directly to the output, without any counting of blank or
    # non-blank lines.

    # Given:
    #   $str = line of text to write

    ${ $self->[_routput_string_] } .= $str;

    if ( chomp $str )              { $self->[_output_line_number_]++; }
    if ( $self->[_save_logfile_] ) { $self->check_line_lengths($str) }

    return;
} ## end sub write_line

sub check_line_lengths {
    my ( $self, $str ) = @_;

    # Collect info on line lengths for logfile
    # Given:
    #   $str = line of text being written

    # This calculation of excess line length ignores any internal tabs
    my $rOpts = $self->[_rOpts_];
    chomp $str;
    my $len_str = length($str);
    my $exceed  = $len_str - $rOpts->{'maximum-line-length'};
    if ( $str && substr( $str, 0, 1 ) eq "\t" && $str =~ /^\t+/g ) {
        $exceed += pos($str) * $rOpts->{'indent-columns'};
    }

    # Note that we just incremented output line number to future value
    # so we must subtract 1 for current line number
    if ( $len_str > $self->[_max_output_line_length_] ) {
        $self->[_max_output_line_length_] = $len_str;
        $self->[_max_output_line_length_at_] =
          $self->[_output_line_number_] - 1;
    }

    if ( $exceed > 0 ) {
        my $output_line_number = $self->[_output_line_number_];
        $self->[_last_line_length_error_]    = $exceed;
        $self->[_last_line_length_error_at_] = $output_line_number - 1;
        if ( $self->[_line_length_error_count_] == 0 ) {
            $self->[_first_line_length_error_]    = $exceed;
            $self->[_first_line_length_error_at_] = $output_line_number - 1;
        }

        if ( $self->[_last_line_length_error_] >
            $self->[_max_line_length_error_] )
        {
            $self->[_max_line_length_error_]    = $exceed;
            $self->[_max_line_length_error_at_] = $output_line_number - 1;
        }

        if ( $self->[_line_length_error_count_] < MAX_NAG_MESSAGES ) {
            $self->write_logfile_entry(
                "Line length exceeded by $exceed characters\n");
        }
        $self->[_line_length_error_count_]++;
    }
    return;
} ## end sub check_line_lengths

sub report_line_length_errors {
    my $self = shift;

    # Write summary info about line lengths to the log file

    my $rOpts                   = $self->[_rOpts_];
    my $line_length_error_count = $self->[_line_length_error_count_];
    if ( $line_length_error_count == 0 ) {
        $self->write_logfile_entry(
            "No lines exceeded $rOpts->{'maximum-line-length'} characters\n");
        my $max_output_line_length    = $self->[_max_output_line_length_];
        my $max_output_line_length_at = $self->[_max_output_line_length_at_];
        $self->write_logfile_entry(
"  Maximum output line length was $max_output_line_length at line $max_output_line_length_at\n"
        );

    }
    else {

        my $word = ( $line_length_error_count > 1 ) ? "s" : EMPTY_STRING;
        $self->write_logfile_entry(
"$line_length_error_count output line$word exceeded $rOpts->{'maximum-line-length'} characters:\n"
        );

        $word = ( $line_length_error_count > 1 ) ? "First" : EMPTY_STRING;
        my $first_line_length_error    = $self->[_first_line_length_error_];
        my $first_line_length_error_at = $self->[_first_line_length_error_at_];
        $self->write_logfile_entry(
" $word at line $first_line_length_error_at by $first_line_length_error characters\n"
        );

        if ( $line_length_error_count > 1 ) {
            my $max_line_length_error    = $self->[_max_line_length_error_];
            my $max_line_length_error_at = $self->[_max_line_length_error_at_];
            my $last_line_length_error   = $self->[_last_line_length_error_];
            my $last_line_length_error_at =
              $self->[_last_line_length_error_at_];
            $self->write_logfile_entry(
" Maximum at line $max_line_length_error_at by $max_line_length_error characters\n"
            );
            $self->write_logfile_entry(
" Last at line $last_line_length_error_at by $last_line_length_error characters\n"
            );
        }
    }
    return;
} ## end sub report_line_length_errors
1;
