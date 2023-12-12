#!/usr/bin/perl -w
use strict;
use warnings;

# This is a utility to stress test perltidy by inserting as many side comments
# into a script as possible.  It has helped locate several bugs.

# Usage:

# 1. Important: work in a new temporary empty directory below a directory full of
# perl scripts.

# 2. Then enter:
#
#      side_comment_test.pl
#
#   to operate on all regular files in the parent directory, i.e. '../*'

#   More generally:
#      side_comment_test.pl file1 [ file2 ...
#   where file1 .. are the files to operate on

# 3. Look at any files named '*.2.ERR' and try to resolve the problem

# 4. When done, remove the temporary directory

main();

sub main { #<<<
my $cmd;
my @files = @ARGV;
if ( !@files ) { @files = glob('../*'); }
foreach my $file (@files) {

    unless ( -e $file && -f $file && -s $file ) {
        print STDERR "skipping $file\n";
        next;
    }

    # Best to skip files written by perltidy
    if ( $file =~ /\.(tdy|ERR|LOG|DEBUG)$/ ) {
        print STDERR "skipping $file\n";
        next;
    }

    my $basename = $file;
    if ( $basename =~ /^(.*)\/([^\/]+)$/ ) { $basename = $2 }
    my $file1 = "$basename.1";
    my $file2 = "$basename.2";
    my $file3 = "$basename.3";

    # Start by extruding the input file into as many lines as possible
    $cmd = "perltidy -extrude <$file -o $file1";
    system($cmd);

    # Skip if starting file has some kind of error
    if ( -e "perltidy.ERR" ) {
        unlink $file1;
        next;
    }

    # Add side comments and see if perltidy indicates an error
    add_side_comments( $file1, $file2 );
    $cmd = "perltidy $file2 -o $file3";
    system($cmd);

    # Clean up if no error
    if ( !-e $file2 . ".ERR" ) {
        unlink $file1;
        unlink $file2;
        unlink $file3;
    }

    # Otherwise, leave the files for analysis
}
}

sub add_side_comments {
    my ( $ifile, $ofile ) = @_;

    # Given file named $ifile,
    # add as many side comments as possible and write result to $ofile

    # create a mask for use in avoiding placing side comments in unsafe places
    my ( @mask, @lines );
    PerlMask::perlmask(
        _source         => $ifile,
        _rmasked_file   => \@mask,
        _roriginal_file => \@lines,
        _compression    => undef
    );

    # Check: be sure the source and mask arrays are the same size
    my $num_mask   = @mask;
    my $num_source = @lines;
    if ( $num_mask != $num_source ) {
        print STDERR
          "num_source=$num_source != num_mask=$num_mask for file '$ifile'\n";
        exit 1;
    }

    # Loop over lines to add side comments where safe to do so
    my $ix = -1;
    foreach my $line (@lines) {
        $ix++;
        chomp $line;

        # Do not put side comments on blank lines
        next unless ($line);

        # Do not put side comments on patterns, here targets, __END__,
        # __DATA__, format end, comments, etc.  This is essential to make this
        # error free.
        next if ( $mask[$ix] =~ /[Qq#]\s*$/ );

        # Add a space if needed to avoid creating a punctuation variable
        if ( $line =~ /[\@\%\$\*]$/ ) { $line .= " " }

        # Append the comment
        $line .= "#sc#";
    }
    my $string2 = join "\n", @lines;
    write_file( $ofile, $string2 );
}

sub write_file {
    my ( $fname, $string, $msg ) = @_;
    open my $fh, '>', $fname or die "cannot open $fname: $!\n";
    if ( utf8::is_utf8($string) ) {
        binmode $fh, ":raw:encoding(UTF-8)";
    }
    $fh->print($string);
    $fh->close();
    print STDERR "Wrote $fname\n" if ($msg);
    return;
}

#####################################################################
#
# The PerlMask package is an interface to perltidy which accepts a
# source filehandle and returns a 'masked' version of the source as
# a string or array.  It can also optionally return the original file
# as a string or array.
#
# It works by making a callback object with a write_line() method to
# receive tokenized lines from perltidy.  This write_line method
# selectively replaces tokens with either their original text or with a
# benign masking character (such as '#' or 'Q').
#
# Usage:
#
#   PerlMask::perlmask(
#       _source         => $fh,             # required source
#       _rmasked_file   => \$masked_file,   # required ref to ARRAY or SCALAR
#       _roriginal_file => \$original_file, # optional ref to ARRAY or SCALAR
#       _compression    => $opt_c           # optional
#   );
#
# _source is any source that perltidy will accept, including a
# filehandle or reference to SCALAR or ARRAY
#
# The compression flag may have these values:
#  0 all mask file line numbers and character positions agree with
#    original file (DEFAULT)
#  1 line numbers agree and character positions agree within lines of code
#  2 line numbers agree but character positions do not
#  3 no correspondence between line numbers or character positions
#
#####################################################################

package PerlMask;
use Carp;
use Perl::Tidy;

sub perlmask {

    my %args  = ( _compression => 0, @_ );
    my $rfile = $args{_rmasked_file};
    unless ( defined($rfile) ) {
        croak
          "Missing required parameter '_rmasked_file' in call to perlmask\n";
    }
    my $ref = ref($rfile);
    unless ( $ref =~ /^(SCALAR|ARRAY)$/ ) {
        croak <<EOM;
Expecting _rmasked_file = ref to SCALAR or ARRAY in perlmask but got : ($ref)
EOM
    }

    # run perltidy, which will call $formatter's write_line() for each line
    my $err = perltidy(
        'source'    => $args{_source},
        'formatter' => bless( \%args, __PACKAGE__ ),    # callback object
        'argv'      => "-npro -se",    # -npro : ignore .perltidyrc,
                                       # -se   : errors to STDOUT
    );
    if ($err) {
        my $name = $args{_source};
        print STDERR "perltidy returns error flag for source=$name\n";
        return;
    }
}

sub print_line {

    # called from write_line to dispatch one line (either masked or original)..
    # here we'll either append it to a string or array, as appropriate
    my ( $rfile, $line ) = @_;
    if ( defined($rfile) ) {
        if ( ref($rfile) eq 'SCALAR' ) {
            $$rfile .= $line . "\n";
        }
        elsif ( ref($rfile) eq 'ARRAY' ) {
            push @{$rfile}, $line . "\n";
        }
    }
}

sub write_line {

    # This is called from perltidy line-by-line
    my ( $self, $line_of_tokens ) = @_;
    my $rmasked_file   = $self->{_rmasked_file};
    my $roriginal_file = $self->{_roriginal_file};
    my $opt_c          = $self->{_compression};
    $opt_c = 0 unless defined($opt_c);

    my $line_type         = $line_of_tokens->{_line_type};
    my $input_line_number = $line_of_tokens->{_line_number};
    my $input_line        = $line_of_tokens->{_line_text};
    my $rtoken_type       = $line_of_tokens->{_rtoken_type};
    my $rtokens           = $line_of_tokens->{_rtokens};
    chomp $input_line;

    # mask non-CODE lines
    if ( $line_type ne 'CODE' ) {
        return if ( $opt_c == 3 );
        my $len = length($input_line);
        if ( $opt_c == 0 && $len > 0 ) {
            print_line( $roriginal_file, $input_line ) if $roriginal_file;
            print_line( $rmasked_file,   '#' x $len );
        }
        else {
            print_line( $roriginal_file, $input_line ) if $roriginal_file;
            print_line( $rmasked_file,   "" );
        }
        return;
    }

    # we'll build the masked line token by token
    my $masked_line = "";

    # add leading spaces if not in a higher compression mode
    if ( $opt_c <= 1 ) {

        # Find leading whitespace.  But be careful..we don't want the
        # whitespace if it is part of quoted text, because it will
        # already be contained in a token.
        if ( $input_line =~ /^(\s+)/ && !$line_of_tokens->{_starting_in_quote} )
        {
            $masked_line = $1;
        }
    }

    # loop over tokens to construct one masked line
    for ( my $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        # Mask certain token types by replacing them with their type code:
        # type  definition
        # ----  ----------
        # Q     quote or pattern
        # q     qw quote
        # h     << here doc operator
        # #     comment
        #
        # This choice will produce a mask file that has balanced
        # container tokens and does not cause parsing problems.
        if ( $$rtoken_type[$j] =~ /^[Qqh]$/ ) {
            if ( $opt_c <= 1 ) {
                $masked_line .= $$rtoken_type[$j] x length( $$rtokens[$j] );
            }
            else {
                $masked_line .= $$rtoken_type[$j];
            }
        }

        # Mask a comment
        elsif ( $$rtoken_type[$j] eq '#' ) {
            if ( $opt_c == 0 ) {
                $masked_line .= '#' x length( $$rtokens[$j] );
            }
        }

        # All other tokens go out verbatim
        else {
            $masked_line .= $$rtokens[$j];
        }
    }
    print_line( $roriginal_file, $input_line ) if $roriginal_file;
    print_line( $rmasked_file,   $masked_line );

    # self-check lengths; this error should never happen
    if ( $opt_c == 0 && length($masked_line) != length($input_line) ) {
        my $lmask  = length($masked_line);
        my $linput = length($input_line);
        print STDERR
"$input_line_number: length ERROR, masked length=$lmask but input length=$linput\n";
    }
}

# called once after the last line of a file
sub finish_formatting {
    my $self = shift;
    return;
}
