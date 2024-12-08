#!/usr/bin/perl -w
use strict;

# Scan text and report
#  - lines with lengths > 80
#  - tabs
#  - lines with ending spaces
#  - non-ascii characters
my $too_long;
my $too_long_no_left_space;
my $has_tab;
my $has_ending_space;
my $non_ascii;
my $lno = 0;
while ( my $line = <> ) {
    $lno++;
    chomp $line;
    my $excess = length($line) - 80;
    if ( $excess > 0 ) {
        if ( $line =~ /^\s/ ) {
            print "$lno:x=$excess:$line\n";
            $too_long++;
        }
        else {
            $too_long_no_left_space++;
        }
    }
    if ( $line =~ /\t/ ) {
        print "$lno:tabs:$line\n";
        $has_tab++;
    }
    if ( $line =~ /\s+$/ ) {
        print "$lno:end spaces:$line\n";
        $has_ending_space++;
    }
    if ( $line =~ /[^[:ascii:]]/ ) {
        print "$lno:non-ascii:$line\n";
        $non_ascii++;
    }
}
print "\n";
if ($too_long) {
    print "$too_long lines with leading space exceed 80 chars\n";
}
if ($too_long_no_left_space) {
    print
"$too_long_no_left_space left adjusted lines exceed 80 chars(not shown)\n";
}
if ($has_tab) {
    print "$has_tab lines contain tabs\n";
}
if ($has_ending_space) {
    print "$has_ending_space lines end in a space\n";
}
if ($non_ascii) {
    print "$non_ascii lines have non-ascii characters\n";
}
