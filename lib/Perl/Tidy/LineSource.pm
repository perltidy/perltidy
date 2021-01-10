#####################################################################
#
# the Perl::Tidy::LineSource class supplies an object with a 'get_line()' method
# which returns the next line to be parsed
#
#####################################################################

package Perl::Tidy::LineSource;
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
        input_file               => undef,
        rOpts                    => undef,
        rpending_logfile_message => undef,
    );

    my %args = ( %defaults, @args );

    my $input_file               = $args{input_file};
    my $rOpts                    = $args{rOpts};
    my $rpending_logfile_message = $args{rpending_logfile_message};

    my $input_line_ending;
    if ( $rOpts->{'preserve-line-endings'} ) {
        $input_line_ending = Perl::Tidy::find_input_line_ending($input_file);
    }

    ( my $fh, $input_file ) = Perl::Tidy::streamhandle( $input_file, 'r' );
    return unless $fh;

    # in order to check output syntax when standard output is used,
    # or when it is an object, we have to make a copy of the file
    if ( ( $input_file eq '-' || ref $input_file ) && $rOpts->{'check-syntax'} )
    {

        # Turning off syntax check when input output is used.
        # The reason is that temporary files cause problems on
        # on many systems.
        $rOpts->{'check-syntax'} = 0;

        ${$rpending_logfile_message} .= <<EOM;
Note: --syntax check will be skipped because standard input is used
EOM

    }

    return bless {
        _fh                => $fh,
        _filename          => $input_file,
        _input_line_ending => $input_line_ending,
        _rinput_buffer     => [],
        _started           => 0,
    }, $class;
}

sub close_input_file {
    my $self = shift;

    # Only close physical files, not STDIN and other objects
    my $filename = $self->{_filename};
    if ( $filename ne '-' && !ref $filename ) {
        eval { $self->{_fh}->close() };
    }
    return;
}

sub get_line {
    my $self          = shift;
    my $line          = undef;
    my $fh            = $self->{_fh};
    my $rinput_buffer = $self->{_rinput_buffer};

    if ( scalar( @{$rinput_buffer} ) ) {
        $line = shift @{$rinput_buffer};
    }
    else {
        $line = $fh->getline();

        # patch to read raw mac files under unix, dos
        # see if the first line has embedded \r's
        if ( $line && !$self->{_started} ) {
            if ( $line =~ /[\015][^\015\012]/ ) {

                # found one -- break the line up and store in a buffer
                @{$rinput_buffer} = map { $_ . "\n" } split /\015/, $line;
                my $count = @{$rinput_buffer};
                $line = shift @{$rinput_buffer};
            }
            $self->{_started}++;
        }
    }
    return $line;
}
1;

