#!/usr/bin/perl -w
use strict;

# Example script for removing trailing blank lines of code from a perl script
# This is from the examples/ directory of the perltidy distribution and may
# be modified as needed.

# This was written in response to RT #118553, "leave only one newline at end of file".
# Adding the requested feature to perltidy itself would have very undesirable
# side-effects when perltidy is operated from within an editor. So it is best
# done with a separate filter script on entire files.

# usage:
# delete_ending_blank_lines.pl myfile.pl >myfile.new
# delete_ending_blank_lines.pl <myfile.pl >myfile.new
use Getopt::Std;
use Perl::Tidy;
use IO::File;
$| = 1;
use vars qw($opt_h);
main();

sub main {
    my $usage = <<EOM;
   usage: $0 filename >outfile
EOM
    getopts('h') or die "$usage";
    if ($opt_h) { die $usage }

    # Make the source for perltidy, which will be a filehandle
    # or just '-' if the source is stdin
    my ( $file, $fh, $source );
    if ( @ARGV == 0 ) {
        $source = '-';
    }
    elsif ( @ARGV == 1 ) {
        $file = $ARGV[0];
        $fh   = IO::File->new( $file, 'r' );
        unless ($fh) { die "cannot open '$file': $!\n" }
        $source = $fh;
    }
    else { die $usage }

    # make the callback object
    my $formatter = MyFormatter->new();

    my $dest;

    # start perltidy, which will start calling our write_line()
    my $err = perltidy(
        'formatter'   => $formatter,     # callback object
        'source'      => $source,
        'destination' => \$dest,         # (not really needed)
        'argv'        => "-npro -se",    # dont need .perltidyrc
                                         # errors to STDOUT
    );
    if ($err) {
        die "Error calling perltidy\n";
    }
    $fh->close() if $fh;
    return;
}

package MyFormatter;

my @lines;

sub new {
    my ($class) = @_;
    bless {}, $class;
}

sub write_line {

    # This is called from perltidy line-by-line; we just save lines
    my $self           = shift;
    my $line_of_tokens = shift;
    push @lines, $line_of_tokens;
}

# called once after the last line of a file
sub finish_formatting {
    my $self = shift;

    # remove all trailing blank lines of code
    while ( my $line_of_tokens = pop(@lines) ) {
        my $line_type  = $line_of_tokens->{_line_type};
        my $input_line = $line_of_tokens->{_line_text};
        if ( $line_type eq 'CODE' ) {
            chomp $input_line;
            next unless ($input_line);
        }
        push @lines, $line_of_tokens;
        last;
    }

    # write remaining lines
    foreach my $line_of_tokens (@lines) {
        my $line_type  = $line_of_tokens->{_line_type};
        my $input_line = $line_of_tokens->{_line_text};
        print $input_line;
    }
    return;
}
