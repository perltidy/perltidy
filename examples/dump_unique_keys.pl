#!/usr/bin/perl
use strict;
use warnings;
use File::Temp qw{ tempfile };

# Run perltidy --dump-unique-keys on multiple files, and
# show hash keys which just appear in one file.
# Requires Perl::Tidy version 20240903.09 or higher

# The latest version of this file should be at:
# https://github.com/perltidy/perltidy/blob/master/examples/dump_unique_keys.pl

my ( $fh_tmp, $tmpfile );

END {
    if ( defined($tmpfile) && -e $tmpfile ) {
        unlink($tmpfile) or warn "Could not unlink $tmpfile: $!";
    }
}

main();

sub main {

    my $usage = <<EOM;
Run perltidy --dump-unique-keys on multiple files
Usage:
  $0 [ --warn-unique-keys-cutoff=N ] file1 file2 ...
  $0 [ -wutc=N ] file1 file2 ...
  If no files are given, look for MANIFEST and use files lib/.../*.pm
  N is the maximum number of unique keys, per set of related keys, to show.
  The default value of N is 1. It may be increased to show more unused keys.
EOM

    my @files;
    my @args;
    foreach my $str (@ARGV) {
        if   ( substr( $str, 0, 1 ) eq '-' ) { push @args,  $str }
        else                                 { push @files, $str }
    }
    my $argstr = join ' ', @args;

    if ( !@files ) {
        my $MANIFEST = "MANIFEST";
        if ( -e $MANIFEST && -f $MANIFEST ) {
            my $rfiles = read_MANIFEST($MANIFEST);
            @files = @{$rfiles};
            my $num = @files;
            print STDERR "Reading $MANIFEST...found $num files\n";
        }
    }

    if ( !@files ) { die $usage }

    foreach my $file (@files) {
        if ( !-e $file ) { die "file '$file' not found\n" }
    }

    ( $fh_tmp, $tmpfile ) = tempfile();
    if ( !$fh_tmp ) {
        die "unable to open temporary file $tmpfile\n";
    }

    # Loop to run perltidy -duk on each file:
    # - capture standard output to a file for further processing
    # - any error messages go to the standard error output
    my %seen;
    my $saw_error;
    foreach my $file (@files) {
        next if ( $seen{$file}++ );
        next if ( !-e $file || -z $file );
        my $cmd = "perltidy -npro -duk $argstr $file >>$tmpfile -se";
        my $err = system($cmd);
        if ($err) { $saw_error++; warn "perltidy returned error for '$file'\n" }
    }

    my $fh;
    if ( !open( $fh, '<', $tmpfile ) ) {
        die "cannot open my temp file '$tmpfile': $!\n";
    }

    # read the captured output and find duplicate words
    my %word_count;
    my @lines;
    foreach my $line (<$fh>) {
        my $word;
        if ( $line =~ /^(.*),(\d+)\s*$/ ) {
            $word = $1;
            if ( !defined( $word_count{$word} ) ) {
                $word_count{$word} = 1;
            }
            else {
                $word_count{$word}++;
            }
        }
        push @lines, [ $line, $word ];
    }
    $fh->close();

    # remove duplicate words
    my @dups = grep { $word_count{$_} > 1 } keys %word_count;
    my %is_dup;
    @is_dup{@dups} = (1) x scalar(@dups);

    my $last_word = "START";
    my @new_lines;
    foreach my $item (@lines) {
        my ( $line, $word ) = @{$item};
        if ( defined($word) ) {

            # line with word: skip duplicate words
            next if ( $is_dup{$word} );
        }
        else {

            # line with filename: remove previous line if it also was a filename
            if ( !defined($last_word) ) { pop @new_lines }
        }
        $last_word = $word;
        push @new_lines, $line;
    }

    my $output_string .= join "", @new_lines;
    print {*STDOUT} $output_string;

} ## end sub main

sub read_MANIFEST {
    my ($MANIFEST) = @_;

    # scan MANIFEST for existing files of the form 'lib/.../*.pm'
    my $fh;
    if ( !open( $fh, '<', $MANIFEST ) ) {
        die "cannot open '$MANIFEST': $!\n";
    }
    my @files;
    foreach my $line (<$fh>) {
        chomp $line;
        next unless $line;
        my @parts = split '/', $line;
        if ( $parts[0] ne 'lib' )     { next }
        if ( $parts[-1] !~ /\.pm$/i ) { next }
        if ( -e $line )               { push @files, $line }
    }
    return \@files;
} ## end sub read_MANIFEST
