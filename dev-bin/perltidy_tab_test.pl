#!/usr/bin/perl -w
use strict;
use warnings;

# This is a utility to stress test perltidy by changing all code blank characters
# into tabs.

# Usage:

# 1. Important: work in a new temporary empty directory below a directory full of
# perl scripts.

# 2. Then enter:
#
#      perltidy_tab_test.pl >out.txt 2>err.txt
#
#   to operate on all regular files in the parent directory, i.e. '../*'

#   More generally:
#      perltidy_tab_test.pl file1 [ file2 ...
#   where file1 .. are the files to operate on

# 3. Look at the err.txt and any files remaining after the run.

# 4. When done, remove the temporary directory

# NOTE:
#   Files with format-skipping will be reported as errors since they will
#   get tabs which will not be removed.  These should be the only results
#   reported with errors.

my @files = @ARGV;
if ( !@files ) { @files = glob('../*'); }
my $total_error_count = 0;
foreach my $file (@files) {

    unless ( -e $file && -f $file && -s $file ) {
        print "skipping $file: not a regular file\n";
        next;
    }

    # Best to skip files written by perltidy
    if ( $file =~ /\.(tdy|ERR|LOG|DEBUG)$/ ) {
        print "skipping $file: wrong extension\n";
        next;
    }

    my $basename = $file;
    if ( $basename =~ /^(.*)\/([^\/]+)$/ ) { $basename = $2 }
    my $file0 = "$basename.0";
    my $file1 = "$basename.1";
    my $file2 = "$basename.2";

    # Skip a file which produces an error
    my $efile = "$file0.ERR";
    my $cmd   = "perltidy $file -o $file0 -se 2>$efile";
    system($cmd);
    if ( -e $efile ) {
        if ( -z $efile ) {
            unlink $efile;
        }
        else {
            unlink $efile;
            unlink $file0;
            print "Skipping $file: produces error\n";
            next;
        }
    }

    my $tab_count_0 = file_tab_count($file0);
    add_tabs( $file0, $file1 );
    my $tab_count_1 = file_tab_count($file1);

    $cmd = "perltidy $file1 -o $file2";
    system($cmd);
    my $tab_count_2 = file_tab_count($file2);

    # If the tab count increases, note this as an error to be checked.
    my $status = "OK";
    if ( $tab_count_2 > $tab_count_0 ) {
        $status = "ERROR";
        $total_error_count++;
    }

    print "$file: $tab_count_0 $tab_count_1 $tab_count_2 $status\n";
    if ( $status ne "OK" ) {
        print STDERR "$file: $tab_count_0 $tab_count_1 $tab_count_2 $status\n";
    }

    # Clean up files if no error, otherwise leave them
    if ( $status eq "OK" && !-e $file2 . ".ERR" ) {
        unlink $file0;
        unlink $file1;
        unlink $file2;
    }

}

if ($total_error_count) {
    print "\nERROR COUNT: $total_error_count\n";
    print STDERR "\nERROR COUNT: $total_error_count\n";
}
else {
    print "\nOK\n";
}

sub get_string {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";
    local $/ = undef;
    my $string = <$fh>;
    close $fh;
    return $string;
}

sub file_tab_count {
    my ($file) = @_;
    my $str = get_string($file);
    return string_tab_count($str);
}

sub string_tab_count {
    my ($str) = @_;
    return 0 unless ($str);
    return $str =~ tr /\t//;
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

use Perl::Tidy;

sub add_tabs {
    my ( $ifile, $ofile ) = @_;

    # Given file named $ifile,
    # convert code spaces to tabs and write result to $ofile

    # create a mask for use in avoiding placing tabs in unsafe places
    my (@lines);

    my %args = (
        _source         => $ifile,
        _roriginal_file => \@lines,
    );

    # run perltidy, which will call $formatter's write_line() for each line
    my $err = perltidy(
        'source'    => $ifile,
        'formatter' => bless( \%args, __PACKAGE__ ),    # callback object
        'argv'      => "-npro -se",    # -npro : ignore .perltidyrc,
                                       # -se   : errors to STDOUT
    );

    if ($err) {
        my $name = $args{_source};
        print STDERR "perltidy returns error flag for source=$name\n";
    }

    my $string2 = join "", @lines;
    write_file( $ofile, $string2 );

    return;
}

sub print_line {

    # called from write_line to dispatch one line
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
    my $roriginal_file = $self->{_roriginal_file};

    my $line_type         = $line_of_tokens->{_line_type};
    my $input_line_number = $line_of_tokens->{_line_number};
    my $input_line        = $line_of_tokens->{_line_text};
    my $rtoken_type       = $line_of_tokens->{_rtoken_type};
    my $rtokens           = $line_of_tokens->{_rtokens};
    chomp $input_line;

    # Output non-CODE lines unchanged
    if ( $line_type ne 'CODE' ) {
        print_line( $roriginal_file, $input_line ) if $roriginal_file;
        return;
    }

    my $tab_line = "";

    # loop over tokens to make the changes
    for ( my $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        my $token = $rtokens->[$j];

        ######################
        # Convert space to tab
        ######################
        if ( $$rtoken_type[$j] eq 'b' ) {
            $token =~ s/ /\t/g;
        }
        elsif ( $$rtoken_type[$j] eq 'i' ) {
            $token =~ s/ /\t/g;
        }

        $tab_line .= $token;
    }
    print_line( $roriginal_file, $tab_line ) if $roriginal_file;
}

# called once after the last line of a file
sub finish_formatting {
    my $self = shift;
    return;
}
