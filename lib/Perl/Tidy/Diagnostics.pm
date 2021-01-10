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
# NOTE: This feature is deactivated in final releases but can be
# reactivated for debugging by un-commenting the 'I' options flag
#
#####################################################################

package Perl::Tidy::Diagnostics;
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

    my $class = shift;
    return bless {
        _write_diagnostics_count => 0,
        _last_diagnostic_file    => "",
        _input_file              => "",
        _fh                      => undef,
    }, $class;
}

sub set_input_file {
    my ( $self, $input_file ) = @_;
    $self->{_input_file} = $input_file;
    return;
}

sub write_diagnostics {
    my ( $self, $msg ) = @_;

    unless ( $self->{_write_diagnostics_count} ) {
        open( $self->{_fh}, ">", "DIAGNOSTICS" )
          or Perl::Tidy::Die("couldn't open DIAGNOSTICS: $!\n");
    }

    my $fh                   = $self->{_fh};
    my $last_diagnostic_file = $self->{_last_diagnostic_file};
    my $input_file           = $self->{_input_file};
    if ( $last_diagnostic_file ne $input_file ) {
        $fh->print("\nFILE:$input_file\n");
    }
    $self->{_last_diagnostic_file} = $input_file;
    my $input_line_number = Perl::Tidy::Tokenizer::get_input_line_number();
    $fh->print("$input_line_number:\t$msg");
    $self->{_write_diagnostics_count}++;
    return;
}

1;

