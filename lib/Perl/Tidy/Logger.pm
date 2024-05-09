#####################################################################
#
# The Perl::Tidy::Logger class writes any .LOG and .ERR files
# and supplies some basic run information for error handling.
#
#####################################################################

package Perl::Tidy::Logger;
use strict;
use warnings;
our $VERSION = '20240511';
use Carp;
use English qw( -no_match_vars );

use constant DEVEL_MODE   => 0;
use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };

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

use constant DEFAULT_LOGFILE_GAP => 50;

sub new {

    my ( $class, @arglist ) = @_;
    if ( @arglist % 2 ) { croak "Odd number of items in arg hash list\n" }

    my %defaults = (
        rOpts           => undef,
        log_file        => undef,
        warning_file    => undef,
        fh_stderr       => undef,
        display_name    => undef,
        is_encoded_data => undef,
    );

    my %args = ( %defaults, @arglist );

    my $rOpts           = $args{rOpts};
    my $log_file        = $args{log_file};
    my $warning_file    = $args{warning_file};
    my $fh_stderr       = $args{fh_stderr};
    my $display_name    = $args{display_name};
    my $is_encoded_data = $args{is_encoded_data};

    my $fh_warnings = $rOpts->{'standard-error-output'} ? $fh_stderr : undef;

    # remove any old error output file if we might write a new one
    if ( !$fh_warnings && !ref($warning_file) ) {
        if ( -e $warning_file ) {
            unlink($warning_file)
              or Perl::Tidy::Die(
                "couldn't unlink warning file $warning_file: $OS_ERROR\n");
        }
    }

    my $logfile_gap =
      defined( $rOpts->{'logfile-gap'} )
      ? $rOpts->{'logfile-gap'}
      : DEFAULT_LOGFILE_GAP;
    if ( $logfile_gap == 0 ) { $logfile_gap = 1 }

    my $filename_stamp    = $display_name ? $display_name . ':' : "??";
    my $input_stream_name = $display_name ? $display_name       : "??";
    return bless {
        _log_file                      => $log_file,
        _logfile_gap                   => $logfile_gap,
        _rOpts                         => $rOpts,
        _fh_warnings                   => $fh_warnings,
        _last_input_line_written       => 0,
        _last_input_line_number        => undef,
        _at_end_of_file                => 0,
        _use_prefix                    => 1,
        _block_log_output              => 0,
        _line_of_tokens                => undef,
        _output_line_number            => undef,
        _wrote_line_information_string => 0,
        _wrote_column_headings         => 0,
        _warning_file                  => $warning_file,
        _warning_count                 => 0,
        _complaint_count               => 0,
        _is_encoded_data               => $is_encoded_data,
        _saw_code_bug      => -1,                    # -1=no 0=maybe 1=for sure
        _saw_brace_error   =>  0,
        _output_array      => [],
        _input_stream_name => $input_stream_name,
        _filename_stamp    => $filename_stamp,
        _save_logfile      => $rOpts->{'logfile'},
    }, $class;
} ## end sub new

sub get_input_stream_name {
    my $self = shift;
    return $self->{_input_stream_name};
}

sub set_last_input_line_number {
    my ( $self, $lno ) = @_;
    $self->{_last_input_line_number} = $lno;
    return;
}

sub get_warning_count {
    my $self = shift;
    return $self->{_warning_count};
}

sub get_use_prefix {
    my $self = shift;
    return $self->{_use_prefix};
}

sub block_log_output {
    my $self = shift;
    $self->{_block_log_output} = 1;
    return;
}

sub unblock_log_output {
    my $self = shift;
    $self->{_block_log_output} = 0;
    return;
}

sub interrupt_logfile {
    my $self = shift;
    $self->{_use_prefix} = 0;
    $self->warning("\n");
    $self->write_logfile_entry( '#' x 24 . "  WARNING  " . '#' x 25 . "\n" );
    return;
} ## end sub interrupt_logfile

sub resume_logfile {
    my $self = shift;
    $self->write_logfile_entry( '#' x 60 . "\n" );
    $self->{_use_prefix} = 1;
    return;
} ## end sub resume_logfile

sub we_are_at_the_last_line {
    my $self = shift;
    if ( !$self->{_wrote_line_information_string} ) {
        $self->write_logfile_entry("Last line\n\n");
    }
    $self->{_at_end_of_file} = 1;
    return;
} ## end sub we_are_at_the_last_line

# record some stuff in case we go down in flames
use constant MAX_PRINTED_CHARS => 35;

sub black_box {
    my ( $self, $line_of_tokens, $output_line_number ) = @_;

    # This routine saves information comparing the indentation of input
    # and output lines when a detailed logfile is requested.
    # This was very useful during the initial development of perltidy.

    my $input_line        = $line_of_tokens->{_line_text};
    my $input_line_number = $line_of_tokens->{_line_number};

    $self->{_line_of_tokens}                = $line_of_tokens;
    $self->{_output_line_number}            = $output_line_number;
    $self->{_wrote_line_information_string} = 0;

    my $last_input_line_written = $self->{_last_input_line_written};
    if (
        (
            ( $input_line_number - $last_input_line_written ) >=
            $self->{_logfile_gap}
        )
        || ( $input_line =~ /^\s*(sub|package)\s+(\w+)/ )
      )
    {
        my $structural_indentation_level = $line_of_tokens->{_level_0};
        $structural_indentation_level = 0
          if ( $structural_indentation_level < 0 );
        $self->{_last_input_line_written} = $input_line_number;
        ( my $out_str = $input_line ) =~ s/^\s+//;
        chomp $out_str;

        $out_str = ( '.' x $structural_indentation_level ) . $out_str;

        if ( length($out_str) > MAX_PRINTED_CHARS ) {
            $out_str = substr( $out_str, 0, MAX_PRINTED_CHARS ) . " ....";
        }
        $self->logfile_output( EMPTY_STRING, "$out_str\n" );
    }
    return;
} ## end sub black_box

sub write_logfile_entry {

    my ( $self, @msg ) = @_;

    # add leading >>> to avoid confusing error messages and code
    $self->logfile_output( ">>>", "@msg" );
    return;
} ## end sub write_logfile_entry

sub write_column_headings {
    my $self = shift;

    $self->{_wrote_column_headings} = 1;
    my $routput_array = $self->{_output_array};
    push @{$routput_array}, <<EOM;

Starting formatting pass...
The nesting depths in the table below are at the start of the lines.
The indicated output line numbers are not always exact.
ci = levels of continuation indentation; bk = 1 if in BLOCK, 0 if not.

in:out indent c b  nesting   code + messages; (messages begin with >>>)
lines  levels i k            (code begins with one '.' per indent level)
------  ----- - - --------   -------------------------------------------
EOM
    return;
} ## end sub write_column_headings

sub make_line_information_string {

    # make columns of information when a logfile message needs to go out
    my $self                    = shift;
    my $line_of_tokens          = $self->{_line_of_tokens};
    my $input_line_number       = $line_of_tokens->{_line_number};
    my $line_information_string = EMPTY_STRING;
    if ($input_line_number) {

        my $output_line_number   = $self->{_output_line_number};
        my $brace_depth          = $line_of_tokens->{_curly_brace_depth};
        my $paren_depth          = $line_of_tokens->{_paren_depth};
        my $square_bracket_depth = $line_of_tokens->{_square_bracket_depth};
        my $guessed_indentation_level =
          $line_of_tokens->{_guessed_indentation_level};

        my $structural_indentation_level = $line_of_tokens->{_level_0};

        $self->write_column_headings() unless $self->{_wrote_column_headings};

        # keep logfile columns aligned for scripts up to 999 lines;
        # for longer scripts it doesn't really matter
        my $extra_space = EMPTY_STRING;
        $extra_space .=
            ( $input_line_number < 10 )  ? SPACE x 2
          : ( $input_line_number < 100 ) ? SPACE
          :                                EMPTY_STRING;
        $extra_space .=
            ( $output_line_number < 10 )  ? SPACE x 2
          : ( $output_line_number < 100 ) ? SPACE
          :                                 EMPTY_STRING;

        # there are 2 possible nesting strings:
        # the original which looks like this:  (0 [1 {2
        # the new one, which looks like this:  {{[
        # the new one is easier to read, and shows the order, but
        # could be arbitrarily long, so we use it unless it is too long
        my $nesting_string =
          "($paren_depth [$square_bracket_depth {$brace_depth";
        my $nesting_string_new = $line_of_tokens->{_nesting_tokens_0};
        my $ci_level           = $line_of_tokens->{_ci_level_0};
        if ( $ci_level > 9 ) { $ci_level = '*' }
        my $bk = ( $line_of_tokens->{_nesting_blocks_0} =~ /1$/ ) ? '1' : '0';

        if ( length($nesting_string_new) <= 8 ) {
            $nesting_string =
              $nesting_string_new . SPACE x ( 8 - length($nesting_string_new) );
        }
        $line_information_string =
"L$input_line_number:$output_line_number$extra_space i$guessed_indentation_level:$structural_indentation_level $ci_level $bk $nesting_string";
    }
    return $line_information_string;
} ## end sub make_line_information_string

sub logfile_output {
    my ( $self, $prompt, $msg ) = @_;
    return if ( $self->{_block_log_output} );

    my $routput_array = $self->{_output_array};
    if ( $self->{_at_end_of_file} || !$self->{_use_prefix} ) {
        push @{$routput_array}, "$msg";
    }
    else {
        my $line_information_string = $self->make_line_information_string();
        $self->{_wrote_line_information_string} = 1;

        if ($line_information_string) {
            push @{$routput_array}, "$line_information_string   $prompt$msg";
        }
        else {
            push @{$routput_array}, "$msg";
        }
    }
    return;
} ## end sub logfile_output

sub get_saw_brace_error {
    my $self = shift;
    return $self->{_saw_brace_error};
}

sub increment_brace_error {
    my $self = shift;
    $self->{_saw_brace_error}++;
    return;
}

sub brace_warning {
    my ( $self, $msg, $msg_line_number ) = @_;

    use constant BRACE_WARNING_LIMIT => 10;
    my $saw_brace_error = $self->{_saw_brace_error};

    if ( $saw_brace_error < BRACE_WARNING_LIMIT ) {
        $self->warning( $msg, $msg_line_number );
    }
    $saw_brace_error++;
    $self->{_saw_brace_error} = $saw_brace_error;

    if ( $saw_brace_error == BRACE_WARNING_LIMIT ) {
        $self->warning("No further warnings of this type will be given\n");
    }
    return;
} ## end sub brace_warning

sub complain {

    # handle non-critical warning messages based on input flag
    my ( $self, $msg, $msg_line_number ) = @_;
    my $rOpts = $self->{_rOpts};

    # these appear in .ERR output only if -w flag is used
    if ( $rOpts->{'warning-output'} ) {
        $self->warning( $msg, $msg_line_number );
    }

    # otherwise, they go to the .LOG file
    else {
        $self->{_complaint_count}++;
        if ($msg_line_number) {

            # TODO: consider using same prefix as warning()
            $msg = $msg_line_number . ':' . $msg;
        }
        $self->write_logfile_entry($msg);
    }
    return;
} ## end sub complain

sub warning {

    my ( $self, $msg, $msg_line_number ) = @_;

    # Report errors to .ERR file (or stdout)
    # Given:
    #    $msg             = a string with the warning message
    #    $msg_line_number = optional line number prefix

    use constant WARNING_LIMIT => 50;

    # Always bump the warn count, even if no message goes out
    Perl::Tidy::Warn_count_bump();

    my $rOpts = $self->{_rOpts};
    if ( !$rOpts->{'quiet'} ) {

        my $warning_count   = $self->{_warning_count};
        my $fh_warnings     = $self->{_fh_warnings};
        my $is_encoded_data = $self->{_is_encoded_data};
        if ( !$fh_warnings ) {
            my $warning_file = $self->{_warning_file};
            $fh_warnings =
              Perl::Tidy::streamhandle( $warning_file, 'w', $is_encoded_data );
            if ( !$fh_warnings ) {
                Perl::Tidy::Die("couldn't open warning file '$warning_file'\n");
            }
            Perl::Tidy::Warn_msg("## Please see file $warning_file\n")
              unless ref($warning_file);
            $self->{_fh_warnings} = $fh_warnings;
            $fh_warnings->print("Perltidy version is $Perl::Tidy::VERSION\n");
        }

        my $filename_stamp = $self->{_filename_stamp};

        if ( $warning_count < WARNING_LIMIT ) {

            if ( !$warning_count ) {

                # On first error always write a line with the filename.  Note
                # that the filename will be 'perltidy' if input is from stdin
                # or from a data structure.
                if ($filename_stamp) {
                    $fh_warnings->print(
                        "\n$filename_stamp Begin Error Output Stream\n");
                }

                # Turn off filename stamping unless error output is directed
                # to the standard error output (with -se flag)
                if ( !$rOpts->{'standard-error-output'} ) {
                    $filename_stamp = EMPTY_STRING;
                    $self->{_filename_stamp} = $filename_stamp;
                }
            }

            if ( $self->get_use_prefix() > 0 && defined($msg_line_number) ) {
                $self->write_logfile_entry("WARNING: $msg");

                # add prefix 'filename:line_no: ' to message lines
                my $pre_string = $filename_stamp . $msg_line_number . ': ';
                chomp $msg;
                $msg =~ s/\n/\n$pre_string/g;
                $msg = $pre_string . $msg . "\n";

                $fh_warnings->print($msg);

            }
            else {
                $self->write_logfile_entry($msg);

                # add prefix 'filename: ' to message lines
                if ($filename_stamp) {
                    my $pre_string = $filename_stamp . SPACE;
                    chomp $msg;
                    $msg =~ s/\n/\n$pre_string/g;
                    $msg = $pre_string . $msg . "\n";
                }

                $fh_warnings->print($msg);
            }
        }
        $warning_count++;
        $self->{_warning_count} = $warning_count;

        if ( $warning_count == WARNING_LIMIT ) {
            $fh_warnings->print(
                $filename_stamp . "No further warnings will be given\n" );
        }
    }
    return;
} ## end sub warning

sub report_definite_bug {
    my $self = shift;
    $self->{_saw_code_bug} = 1;
    return;
}

sub get_save_logfile {
    my $self = shift;
    return $self->{_save_logfile};
}

sub finish {

    # called after all formatting to summarize errors
    my ($self) = @_;

    my $warning_count   = $self->{_warning_count};
    my $save_logfile    = $self->{_save_logfile};
    my $log_file        = $self->{_log_file};
    my $msg_line_number = $self->{_last_input_line_number};

    if ($warning_count) {
        if ($save_logfile) {
            $self->block_log_output();    # avoid echoing this to the logfile
            $self->warning(
                "The logfile $log_file may contain useful information\n",
                $msg_line_number );
            $self->unblock_log_output();
        }

        if ( $self->{_complaint_count} > 0 ) {
            $self->warning(
"To see $self->{_complaint_count} non-critical warnings rerun with -w\n",
                $msg_line_number
            );
        }

        if ( $self->{_saw_brace_error}
            && ( $self->{_logfile_gap} > 1 || !$save_logfile ) )
        {
            $self->warning( "To save a full .LOG file rerun with -g\n",
                $msg_line_number );
        }
    }

    if ($save_logfile) {
        my $is_encoded_data = $self->{_is_encoded_data};
        my $fh = Perl::Tidy::streamhandle( $log_file, 'w', $is_encoded_data );
        if ( !$fh ) {
            Perl::Tidy::Warn("unable to open log file '$log_file'\n");
        }
        else {
            my $routput_array = $self->{_output_array};
            foreach my $line ( @{$routput_array} ) { $fh->print($line) }
            if (   $fh->can('close')
                && !ref($log_file) ne '-'
                && $log_file ne '-' )
            {
                $fh->close()
                  or Perl::Tidy::Warn(
                    "Error closing LOG file '$log_file': $OS_ERROR\n");
            }
        }
    }
    return;
} ## end sub finish
1;
