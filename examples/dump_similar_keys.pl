#!/usr/bin/perl

# Run perltidy --dump-hash-keys on one or more files, and report pairs of
# hash keys which are very similar but different.

# This is one of the perltidy example files; the latest should be at:
# The latest version of this file should be at:
# https://github.com/perltidy/perltidy/blob/master/examples/dump_similar_keys.pl

use strict;
use warnings;
use File::Temp qw{ tempfile };
use Getopt::Long;

# Defaults:
# - ignore keys with length < $minimum_length
# - do not report key pairs whose differences exceed $maximum_differences
my $minimum_length      = 3;
my $maximum_differences = 1;

my $usage = <<EOM;
Run perltidy --dump-similar-keys on one or more files and show similar hash keys
Usage:
  $0 [args] [file1 [ file2 ...
  If no files are given, look for MANIFEST and use files lib/.../*.pm

  [args] are optional control parameters [defaults shown]:
   -h or --help                    :this help message
   -l=n or --minimum_length=n      :ignore keys with less than n chars [l=3]
   -m=n or --maximum_differences=n :n=max differences for similarity [-m=1]

  Notes:
   Keys consisting of pure punctuation are ignored
   Keys with a different first letter are considered dissimilar, 
     where the first letter ignores case and any preceding _ or punctuation
   Transposed characters count as 1 difference
EOM

my $help = 0;
GetOptions(
    'minimum_length|l:i'      => \$minimum_length,
    'maximum_differences|m:i' => \$maximum_differences,
    'help|h|?'                => \$help,
);
if ($help) {
    die $usage;
}

if ( $maximum_differences < 1 ) { die "maximum_differences must be positive\n" }
if ( $minimum_length < 1 )      { die "minimum_length must be positive\n" }

my ( $fh_tmp, $tmpfile );

END {
    if ( defined($tmpfile) && -e $tmpfile ) {
        unlink($tmpfile) or warn "Could not unlink $tmpfile: $!";
    }
}

main();

sub main {

    my @files;
    my @args;
    foreach my $str (@ARGV) {
        if   ( substr( $str, 0, 1 ) eq '-' ) { push @args,  $str }
        else                                 { push @files, $str }
    }

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

    # TODO: @args not yet used

    foreach my $file (@files) {
        if ( !-e $file ) { die "file '$file' not found\n" }
    }

    ( $fh_tmp, $tmpfile ) = tempfile();
    if ( !$fh_tmp ) {
        die "unable to open temporary file $tmpfile\n";
    }

    # Loop to run perltidy -dhk on each file:
    # - capture standard output to a file for further processing
    # - any error messages go to the standard error output
    my %seen;
    my $saw_error;
    foreach my $file (@files) {
        next if ( $seen{$file}++ );
        next if ( !-e $file || -z $file );
        my $cmd = "perltidy -npro -dhk $file >>$tmpfile -se";
        my $err = system($cmd);
        if ($err) { $saw_error++; warn "perltidy returned error for '$file'\n" }
    }

    my $fh;
    if ( !open( $fh, '<', $tmpfile ) ) {
        die "cannot open my temp file '$tmpfile': $!\n";
    }

    # read the captured output and update the counts
    my %word_info;
    foreach my $line (<$fh>) {
        if ( $line =~ /^(.*),(\d+)\s*$/ ) {
            my $word          = $1;
            my $count         = $2;
            my $string_length = length($word);
            next if ( $string_length < $minimum_length );
            if ( !defined( $word_info{$word} ) ) {
                if ( $word =~ /([^\W_])/ ) {
                    $word_info{$word} = {
                        count         => $count,
                        first_letter  => $1,
                        string_length => $string_length,
                    };
                }
            }
            else {
                $word_info{$word}->{count} += $count;
            }
        }
    }
    $fh->close();

    # Keys with a different 'first_letter' are considered dissimilar here,
    # where the first letter ignores case and preceding _ or punctuation
    my @sorted_words = sort {
             $word_info{$a}->{first_letter} cmp $word_info{$b}->{first_letter}
          || $word_info{$a}->{string_length} <=> $word_info{$b}->{string_length}
    } ( keys %word_info );

    my @word_pairs;
  WORD:
    while (@sorted_words) {
        my $word         = shift @sorted_words;
        my $first_letter = $word_info{$word}->{first_letter};
        my $word_length  = $word_info{$word}->{string_length};
        foreach my $word2 (@sorted_words) {

            # sorted order allows us to bail out as soon as possible
            next WORD
              if ( $first_letter ne $word_info{$word2}->{first_letter} );
            next WORD
              if ( $word_info{$word2}->{string_length} - $word_length >
                $maximum_differences );

            if ( string_approximate_match( $word, $word2, $maximum_differences )
              )
            {
                push @word_pairs, [ $word, $word2 ];
            }
        }
    } ## end WORD: while (@sorted_words)

    if (@word_pairs) {
        print "key1,key2,count1,count2\n";
        foreach my $pair (@word_pairs) {
            my ( $w1, $w2 ) = @{$pair};
            my $count1 = $word_info{$w1}->{count};
            my $count2 = $word_info{$w2}->{count};
            if ( $count2 < $count1 ) {
                ( $w1,     $w2 )     = ( $w2,     $w1 );
                ( $count1, $count2 ) = ( $count2, $count1 );
            }
            print "$w1,$w2,$count1,$count2\n";
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

sub string_approximate_match {
    my ( $s1, $s2, $max_diff ) = @_;

    # Given two strings $s1 and $s2
    # Return:
    #   true if the number of differences is <= $max_diff
    #   false otherwise

    my $len1 = length($s1);
    my $len2 = length($s2);
    my $diff_count;
    if ( $len1 > $len2 ) {
        $diff_count = $len1 - $len2;
        return if ( $diff_count > $max_diff );
        $s1 = substr( $s1, 0, $len2 );
    }
    elsif ( $len2 > $len1 ) {
        $diff_count = $len2 - $len1;
        return if ( $diff_count > $max_diff );
        $s2 = substr( $s2, 0, $len1 );
    }
    else {
        $diff_count = 0;
    }
    my $posm;
    my $mask = $s1 ^ $s2;
    while ( $mask =~ /[^\0]/g ) {
        $diff_count++;

        # Count a transposition as just 1 diff
        my $pos = pos($mask);
        if ( $posm && $posm + 1 == $pos ) {
            my $l11 = substr( $s1, $posm - 1, 1 );
            my $l22 = substr( $s2, $pos - 1,  1 );
            if ( $l11 eq $l22 ) {
                my $l12 = substr( $s1, $pos - 1,  1 );
                my $l21 = substr( $s2, $posm - 1, 1 );
                if ( $l12 eq $l21 ) {
                    $diff_count--;
                }
            }
        }
        $posm = $pos;

        if ( $diff_count > $max_diff ) {
            return;
        }
    } ## end while ( $mask =~ /[^\0]/g)
    return 1;
} ## end sub string_approximate_match

