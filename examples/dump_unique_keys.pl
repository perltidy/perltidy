#!/usr/bin/perl
use strict;
use warnings;
use File::Temp qw{ tempfile };

# Run perltidy --dump-unique-keys on multiple files, and
# show hash keys which just appear in one file.
# Requires Perl::Tidy version 20240903.08 or higher

main();

sub main {

    my $usage = <<EOM;
Run perltidy --dump-unique-keys on multiple files
Usage: $0 file1 file2 ...
  if no files are given, look for MANIFEST and use files lib/.../*.pm
EOM

    my @files = @ARGV;

    if ( !@files ) {
        my $MANIFEST = "MANIFEST";
        if ( -e $MANIFEST && -f $MANIFEST ) {
            my $rfiles = read_MANIFEST($MANIFEST);
            @files = @{$rfiles};
        }
    }

    if ( !@files ) { die $usage }

    foreach my $file (@files) {
        if ( !-e $file ) { die "file '$file' not found\n" }
    }

    my ( $fh_tmp, $tmpfile ) = tempfile();
    if ( !$fh_tmp ) {
        die "unable to open temporary file $tmpfile\n";
    }

    # Loop to run perltidy -duk on each file
    my %seen;
    foreach my $file (@files) {
        next if ( $seen{$file}++ );
        my $cmd = "perltidy -npro -duk $file >>$tmpfile";
        my $err = system($cmd);
        if ($err) { die "perltidy return error $err\n" }
    }

    my $fh;
    if ( !open( $fh, '<', $tmpfile ) ) {
        die "cannot open my temp file '$tmpfile': $!\n";
    }

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

    my @dups = grep { $word_count{$_} > 1 } keys %word_count;
    my %is_dup;
    @is_dup{@dups} = (1) x scalar(@dups);

    my $output_string = "";
    foreach my $item (@lines) {
        my ( $line, $word ) = @{$item};
        next if ( defined($word) && $is_dup{$word} );
        $output_string .= $line;
    }

    print {*STDOUT} $output_string;

    END {
        if ( defined($tmpfile) && -e $tmpfile ) {
            unlink($tmpfile) or warn "Could not unlink $tmpfile: $!";
        }
    }
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
