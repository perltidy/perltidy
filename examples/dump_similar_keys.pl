#!/usr/bin/perl

# Run perltidy --dump-hash-keys on one or more files, and report pairs of
# hash keys which are very similar but different.

# This is one of the perltidy example files; the latest should be at:
# https://github.com/perltidy/perltidy/blob/master/examples/dump_similar_keys.pl

use strict;
use warnings;
use File::Temp qw{ tempfile };
use Getopt::Long;
use constant SPACE => q{ };

# Defaults:
# - ignore keys with length < $minimum_length
# - do not report key pairs whose differences exceed $maximum_differences
my $minimum_length      = 4;
my $maximum_differences = 1;

my $usage = <<EOM;
Run perltidy --dump-similar-keys on one or more files and show similar hash keys
Usage:
  $0 [args] [file1 [ file2 ...
  If no files are given, look for MANIFEST and use files lib/.../*.pm

  [args] are optional control parameters [defaults shown]:
  -h or --help                 :this help message
  -l=n or --minimum_length=n   :ignore keys with less than n chars [default 4]
  -m=n or --maximum_differences=n :n=max differences for similarity [default 1]

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

    my @files = @ARGV;

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

    # Read the captured output and update the counts
    my %word_info;
    foreach my $line (<$fh>) {
        if ( $line =~ /^(.*),(\d+)\s*$/ ) {
            my $word          = $1;
            my $count         = $2;
            my $string_length = length($word);
            next if ( $string_length < $minimum_length );
            if ( !defined( $word_info{$word} ) ) {
                if ( $word =~ /([^\W_])/g ) {
                    $word_info{$word} = {
                        count         => $count,
                        first_letter  => lc($1),
                        string_length => $string_length,
                        offset        => pos($word) - 1,
                    };
                }
            }
            else {
                $word_info{$word}->{count} += $count;
            }
        }
    }
    $fh->close();

    my @sorted_words = sort {
             $word_info{$a}->{first_letter} cmp $word_info{$b}->{first_letter}
          || $word_info{$a}->{string_length} <=> $word_info{$b}->{string_length}
          || $a cmp $b
    } ( keys %word_info );

    # Loop to find pairs of similar hash keys
    my @word_pairs;
  WORD:
    while (@sorted_words) {
        my $word         = shift @sorted_words;
        my $first_letter = $word_info{$word}->{first_letter};
        my $offset       = $word_info{$word}->{offset};
        my $word_length  = $word_info{$word}->{string_length};
        foreach my $word2 (@sorted_words) {

            # sorted order allows us to bail out as soon as possible
            next WORD
              if ( $first_letter ne $word_info{$word2}->{first_letter} );
            next WORD
              if ( $word_info{$word2}->{string_length} - $word_length >
                $maximum_differences );

            my $offset2 = $word_info{$word2}->{offset};
            if (
                string_approximate_match(
                    $word, $word2, $offset, $offset2, $maximum_differences
                )
              )
            {
                my $w1     = $word;
                my $w2     = $word2;
                my $count1 = $word_info{$w1}->{count};
                my $count2 = $word_info{$w2}->{count};
                if ( $count2 < $count1 ) {
                    ( $w1,     $w2 )     = ( $w2,     $w1 );
                    ( $count1, $count2 ) = ( $count2, $count1 );
                }
                push @word_pairs, [ $w1, $w2, $count1, $count2 ];
            }
        }
    } ## end WORD: while (@sorted_words)

    if (@word_pairs) {
        print "key1,key2,count1,count2\n";
        foreach my $pair (@word_pairs) {
            my ( $w1, $w2, $count1, $count2 ) = @{$pair};
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
    my ( $s1, $s2, $o1, $o2, $max_diff ) = @_;

    # Given
    #   $s1 and $s2 = 2 strings
    #   $o1 and $o2 = their offsets to leading alphanumeric character
    # Return:
    #   true if the number of differences is <= $max_diff
    #   false otherwise

    # Basic rules for calculating the number of differences:
    #  1. transposed characters            = 1 difference
    #  2. a missing or extra character     = 1 difference
    #  3. repeated same-character changes  = 1 difference
    #  4. otherwise, different characters  = 1 difference
    my $s1_in = $s1;
    my $s2_in = $s2;

    # Pad with leading spaces to get alignment at first alphanumeric character
    my $odiff = $o2 - $o1;
    if ( $odiff > 0 ) {
        $s1 = ( SPACE x $odiff ) . $s1;
    }
    elsif ( $odiff < 0 ) {
        $s2 = ( SPACE x -$odiff ) . $s2;
    }
    else {
        ## words align at first char
    }

    # Pad with spaces on right to get equal lengths.
    # If we find missing characters, we will move them to those locations.
    my $len1 = length($s1);
    my $len2 = length($s2);
    my $pad1 = 0;
    my $pad2 = 0;
    if ( $len1 > $len2 ) {
        $pad2 = $len1 - $len2;
        $s2 .= SPACE x $pad2;
    }
    elsif ( $len2 > $len1 ) {
        $pad1 = $len2 - $len1;
        $s1 .= SPACE x $pad1;
    }
    else {
        ## equal lengths
    }

    # Loop to count character differences, which are at null characters in the
    # xor mask. Give up if the max diff count is exceeded.
    my %saw_change;
    my $diff_count = 0;
    my $posm;
    my $mask = $s1 ^ $s2;
    while ( $mask =~ /[^\0]/g ) {

        my $pos = pos($mask);
        if ( $posm && $pos < $posm ) {
            ## shouldn't happen unless the pos was incorrectly set
            print STDERR "Infinite loop detected for s1=$s1_in s2=$s2_in\n";
            return;
        }

        # Special checks for two differences in a row..
        # we may have to reduce the difference count
        my $diff_inc = 1;
        my $c1p      = substr( $s1, $pos - 1, 1 );
        my $c2p      = substr( $s2, $pos - 1, 1 );
        if ( $posm && $posm + 1 == $pos ) {
            my $c1m = substr( $s1, $posm - 1, 1 );
            my $c2m = substr( $s2, $posm - 1, 1 );
            if ( $c1m eq $c2p ) {

                # Transposition: just count as one difference
                if ( $c1p eq $c2m ) {
                    $diff_inc = 0;
                }

                # Check for a missing character in $s1 (or extra in $s2)
                else {
                    if ( $pad1 > 0 && $c1p eq substr( $s2, $pos, 1 ) ) {

                        # Missing character: remove the ending space,
                        # then rotate it back into the missing character spot
                        $s1 = substr( $s1, 0, -1 );
                        substr( $s1, $posm - 1, 0, SPACE );
                        $pad1 -= 1;

                        # Update the mask and fix the count.
                        $mask = $s1 ^ $s2;
                        pos($mask) = $pos;
                        $diff_inc = 0;
                    }
                }
            }

            # Check for a missing character in $s2 (or extra in $s1)
            else {

                if (   $c1p eq $c2m
                    && $pad2 > 0
                    && $c2p eq substr( $s1, $pos, 1 ) )
                {

                    # Missing character: remove the ending space,
                    # then rotate it back into the missing character spot
                    $s2 = substr( $s2, 0, -1 );
                    substr( $s2, $posm - 1, 0, SPACE );
                    $pad2 -= 1;

                    # Update the mask and fix the count.
                    $mask = $s1 ^ $s2;
                    pos($mask) = $pos;
                    $diff_inc = 0;
                }
            }
        }
        $posm = $pos;

        # count repeated single character changes just 1 time
        if ( $diff_inc && $saw_change{ $c1p . $c2p }++ ) { $diff_inc = 0 }

        if ($diff_inc) { $diff_count += $diff_inc }
        if ( $diff_count > $max_diff ) {
            return;
        }
    } ## end while ( $mask =~ /[^\0]/g)

    # match
    return 1;
} ## end sub string_approximate_match

