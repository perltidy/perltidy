#####################################################################
#
# The Perl::Tidy::Diagnostics class writes the DIAGNOSTICS file, which is
# useful for program development.
#
# Only one such file is created regardless of the number of input
# files processed.  This allows the results of processing many files
# to be summarized in a single file.

# Output messages go to a file named DIAGNOSTICS, where
# they are labeled by file and line.  This allows many files to be
# scanned at once for some particular condition of interest.  It was
# particularly useful for developing guessing strategies.
#
#####################################################################

package Perl::Tidy::Diagnostics;
use strict;
use warnings;
use English qw( -no_match_vars );
our $VERSION = '20250214.02';

use constant EMPTY_STRING => q{};

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

sub new {

    my $class = shift;
    return bless {
        _write_diagnostics_count => 0,
        _last_diagnostic_file    => EMPTY_STRING,
        _input_file              => EMPTY_STRING,
        _fh                      => undef,
    }, $class;
} ## end sub new

sub set_input_file {
    my ( $self, $input_file ) = @_;
    $self->{_input_file} = $input_file;
    return;
}

sub write_diagnostics {
    my ( $self, $msg, $line_number ) = @_;

    # Write a message to the diagnostics file
    # Input parameters:
    #  $msg = string describing the event
    #  $line_number = optional line number

    if ( !$self->{_write_diagnostics_count} ) {
        open( $self->{_fh}, ">", "DIAGNOSTICS" )
          or Perl::Tidy::Die("couldn't open DIAGNOSTICS: $OS_ERROR\n");
    }

    if ( defined($line_number) ) {
        $msg = "$line_number:\t$msg";
    }

    my $fh                   = $self->{_fh};
    my $last_diagnostic_file = $self->{_last_diagnostic_file};
    my $input_file           = $self->{_input_file};
    if ( $last_diagnostic_file ne $input_file ) {
        $fh->print("\nFILE:$input_file\n");
    }
    $self->{_last_diagnostic_file} = $input_file;
    $fh->print($msg);
    $self->{_write_diagnostics_count}++;
    return;
} ## end sub write_diagnostics

1;
