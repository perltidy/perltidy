#!/usr/bin/perl -w
use strict;
my @files = @ARGV;
my %saw;
# Look at a number of .pro profiles and show ther common flags.
# This can help pinpoint the flags which are causing an issue.
foreach my $file (@files) {
    open( IN, "<", $file ) || die "cannot open $file: $!\n";
    while ( my $line = <IN> ) {
        chomp $line;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        next if ( $line =~ /#/ );
        $saw{$line}++;
    }
    close IN;
}
my $nfiles=@files;
foreach my $key(sort keys %saw) {
   next if ($saw{$key} != $nfiles);
   print $key,"\n";
}
