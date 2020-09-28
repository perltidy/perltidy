#####################################################################
#
# the Perl::Tidy::FileWriter class writes the output file
#
#####################################################################

package Perl::Tidy::FileWriter;
use strict;
use warnings;
our $VERSION = '20201001';

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD eq 'DESTROY' );
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

# Maximum number of little messages; probably need not be changed.
my $MAX_NAG_MESSAGES = 6;

BEGIN {

    # Array index names for variables
    my $i = 0;
    use constant {
        _line_sink_object_           => $i++,
        _logger_object_              => $i++,
        _rOpts_                      => $i++,
        _output_line_number_         => $i++,
        _consecutive_blank_lines_    => $i++,
        _consecutive_nonblank_lines_ => $i++,
        _first_line_length_error_    => $i++,
        _max_line_length_error_      => $i++,
        _last_line_length_error_     => $i++,
        _first_line_length_error_at_ => $i++,
        _max_line_length_error_at_   => $i++,
        _last_line_length_error_at_  => $i++,
        _line_length_error_count_    => $i++,
        _max_output_line_length_     => $i++,
        _max_output_line_length_at_  => $i++,
    };
}

sub write_logfile_entry {
    my ( $self, $msg ) = @_;
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->write_logfile_entry($msg);
    }
    return;
}

sub new {
    my ( $class, $line_sink_object, $rOpts, $logger_object ) = @_;

    my $self = [];
    $self->[_line_sink_object_]           = $line_sink_object;
    $self->[_logger_object_]              = $logger_object;
    $self->[_rOpts_]                      = $rOpts;
    $self->[_output_line_number_]         = 1;
    $self->[_consecutive_blank_lines_]    = 0;
    $self->[_consecutive_nonblank_lines_] = 0;
    $self->[_first_line_length_error_]    = 0;
    $self->[_max_line_length_error_]      = 0;
    $self->[_last_line_length_error_]     = 0;
    $self->[_first_line_length_error_at_] = 0;
    $self->[_max_line_length_error_at_]   = 0;
    $self->[_last_line_length_error_at_]  = 0;
    $self->[_line_length_error_count_]    = 0;
    $self->[_max_output_line_length_]     = 0;
    $self->[_max_output_line_length_at_]  = 0;
    bless $self, $class;
    return $self;
}

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

sub reset_consecutive_blank_lines {
    my $self = shift;
    $self->[_consecutive_blank_lines_] = 0;
    return;
}

sub want_blank_line {
    my $self = shift;
    unless ( $self->[_consecutive_blank_lines_] ) {
        $self->write_blank_code_line();
    }
    return;
}

sub require_blank_code_lines {

    # write out the requested number of blanks regardless of the value of -mbl
    # unless -mbl=0.  This allows extra blank lines to be written for subs and
    # packages even with the default -mbl=1
    my ( $self, $count ) = @_;
    my $need   = $count - $self->[_consecutive_blank_lines_];
    my $rOpts  = $self->[_rOpts_];
    my $forced = $rOpts->{'maximum-consecutive-blank-lines'} > 0;
    foreach my $i ( 0 .. $need - 1 ) {
        $self->write_blank_code_line($forced);
    }
    return;
}

sub write_blank_code_line {
    my $self   = shift;
    my $forced = shift;
    my $rOpts  = $self->[_rOpts_];
    return
      if (!$forced
        && $self->[_consecutive_blank_lines_] >=
        $rOpts->{'maximum-consecutive-blank-lines'} );
    $self->[_consecutive_blank_lines_]++;
    $self->[_consecutive_nonblank_lines_] = 0;
    $self->write_line("\n");
    return;
}

sub write_code_line {
    my $self = shift;
    my $a    = shift;

    if ( $a =~ /^\s*$/ ) {
        my $rOpts = $self->[_rOpts_];
        return
          if ( $self->[_consecutive_blank_lines_] >=
            $rOpts->{'maximum-consecutive-blank-lines'} );
        $self->[_consecutive_blank_lines_]++;
        $self->[_consecutive_nonblank_lines_] = 0;
    }
    else {
        $self->[_consecutive_blank_lines_] = 0;
        $self->[_consecutive_nonblank_lines_]++;
    }
    $self->write_line($a);
    return;
}

sub write_line {
    my ( $self, $a ) = @_;

    # TODO: go through and see if the test is necessary here
    if ( $a =~ /\n$/ ) { $self->[_output_line_number_]++; }

    $self->[_line_sink_object_]->write_line($a);

    # This calculation of excess line length ignores any internal tabs
    my $rOpts  = $self->[_rOpts_];
    my $exceed = length($a) - $rOpts->{'maximum-line-length'} - 1;
    if ( $a =~ /^\t+/g ) {
        $exceed += pos($a) * ( $rOpts->{'indent-columns'} - 1 );
    }

    # Note that we just incremented output line number to future value
    # so we must subtract 1 for current line number
    if ( length($a) > 1 + $self->[_max_output_line_length_] ) {
        $self->[_max_output_line_length_] = length($a) - 1;
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

        if ( $self->[_line_length_error_count_] < $MAX_NAG_MESSAGES ) {
            $self->write_logfile_entry(
                "Line length exceeded by $exceed characters\n");
        }
        $self->[_line_length_error_count_]++;
    }
    return;
}

sub report_line_length_errors {
    my $self                    = shift;
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

        my $word = ( $line_length_error_count > 1 ) ? "s" : "";
        $self->write_logfile_entry(
"$line_length_error_count output line$word exceeded $rOpts->{'maximum-line-length'} characters:\n"
        );

        $word = ( $line_length_error_count > 1 ) ? "First" : "";
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
}
1;

