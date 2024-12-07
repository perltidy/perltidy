#!/usr/bin/perl -w
use strict;

# Perform some basic checks on text:
#  - report lines with lengths > 80
#  - report any tabs
#  - report lines with ending spaces
my $too_long;
my $has_tab;
my $has_ending_space;
my $lno = 0;
#while ( my $line = <$fh> ) {
while ( my $line = <> ) {
    $lno++;
    chomp $line;
    my $excess = length($line) - 80;
    if ( $excess > 0 && $line =~ /^\s/ ) {
        print "$lno:x=$excess:$line\n";
        $too_long++;
    }
    if ( $line =~ /\t/ ) {
        print "$lno:tabs:$line\n";
        $has_tab++;
    }
    if ( $line =~ /\s+$/ ) {
        print "$lno:end spaces:$line\n";
        $has_ending_space++;
    }
}
if ($too_long) {
    print "$too_long lines exceed 80 chars:\n";
}
if ($has_tab) {
    print "$has_tab lines contain tabs:\n";
}
if ($has_ending_space) {
    print "$has_ending_space lines end in a space:\n";
}
