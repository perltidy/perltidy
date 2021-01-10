#####################################################################
#
# the Perl::Tidy::LineSink class supplies a write_line method for
# actual file writing
#
#####################################################################

package Perl::Tidy::LineSink;
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

    my ( $class, @args ) = @_;

    my %defaults = (
        output_file              => undef,
        line_separator           => undef,
        rOpts                    => undef,
        rpending_logfile_message => undef,
        is_encoded_data          => undef,
    );
    my %args = ( %defaults, @args );

    my $output_file              = $args{output_file};
    my $line_separator           = $args{line_separator};
    my $rOpts                    = $args{rOpts};
    my $rpending_logfile_message = $args{rpending_logfile_message};
    my $is_encoded_data          = $args{is_encoded_data};

    my $fh = undef;

    my $output_file_open = 0;

    if ( $rOpts->{'format'} eq 'tidy' ) {
        ( $fh, $output_file ) =
          Perl::Tidy::streamhandle( $output_file, 'w', $is_encoded_data );
        unless ($fh) { Perl::Tidy::Die("Cannot write to output stream\n"); }
        $output_file_open = 1;
    }

    # in order to check output syntax when standard output is used,
    # or when it is an object, we have to make a copy of the file
    if ( $output_file eq '-' || ref $output_file ) {
        if ( $rOpts->{'check-syntax'} ) {

            # Turning off syntax check when standard output is used.
            # The reason is that temporary files cause problems on
            # on many systems.
            $rOpts->{'check-syntax'} = 0;
            ${$rpending_logfile_message} .= <<EOM;
Note: --syntax check will be skipped because standard output is used
EOM

        }
    }

    return bless {
        _fh               => $fh,
        _output_file      => $output_file,
        _output_file_open => $output_file_open,
        _line_separator   => $line_separator,
        _is_encoded_data  => $is_encoded_data,
    }, $class;
}

sub write_line {

    my ( $self, $line ) = @_;
    my $fh = $self->{_fh};

    my $output_file_open = $self->{_output_file_open};
    chomp $line;
    $line .= $self->{_line_separator};

    $fh->print($line) if ( $self->{_output_file_open} );

    return;
}

sub close_output_file {
    my $self = shift;

    # Only close physical files, not STDOUT and other objects
    my $output_file = $self->{_output_file};
    if ( $output_file ne '-' && !ref $output_file ) {
        $self->{_fh}->close() if $self->{_output_file_open};
    }
    return;
}

1;
