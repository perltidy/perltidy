#!/usr/bin/perl -w
use strict;

# check that the lines with fixed lengths in the perltidy pod are not too long
my $fname='perltidy';
my $fh;
if (!open($fh, '<', $fname)) {
   print "cannot open $fname: $!\n";
}
my @too_long;
my $lno=0;
while (my $line=<$fh>) {
   $lno++;
   my $excess=length($line)-80;
   next if ($excess<=0);
   next if ($line !~ /^\s/);
   print "$lno:$excess:$line";
   push @too_long, $lno;
}
if (@too_long) {
   my $num=@too_long;
   print "$num lines exceed 80 chars:\n";
}
