#!/usr/bin/perl -w
use strict;

my $usage = <<EOM;

Read one or more profile files written by perltidy_random_setup.pl and
write a reduced version which omits default and other non-essential
parameters.  This is an aid for debugging blinkers.

Usage:
   $0 profile.1 >profile.min

EOM

if ( !@ARGV == 1 ) {
    die $usage;
}
my $ifile       = $ARGV[0];
my $ris_default = get_defaults();

my $ris_non_essential = {};
my @q                 = qw(
  file-size-order
  nofile-size-order
  force-read-binary
  noforce-read-binary
  preserve-line-endings
  nopreserve-line-endings
  timestamp
  notimestamp
  profile
  noprofile
  npro
  no-profile
);
@{$ris_non_essential}{@q} = (1) x scalar(@q);

my @lines;
my $format = "tidy";
open( IN, "<", $ifile ) || die "cannot open $ifile: $!\n";
while ( my $line = <IN> ) {
    chomp $line;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    if    ( $line =~ /-?-format=html$/ ) { $format = 'html' }
    if    ( $line =~ /-?-format=html$/ ) { $format = 'tidy' }
    elsif ( $line =~ /-?-html$/ )        { $format = 'html' }
    elsif ( $line =~ /-?-tidy$/ )        { $format = 'tidy' }

    # filter out defaults
    my $key = $line;
    $key =~ s/^--//;
    next if ( $ris_default->{$key} );
    next if ( $key =~ /^perl-syntax-check-flags/ );

    # remove flags not related to formatting
    next if ( $ris_non_essential->{$key} );

    # Turn off iterations
    if ( $key =~ /^(iterations|it)=/ ) { $line = '##' . $line }
    if ( $key =~ /^(converge|conv)/ )  { $line = '##' . $line }

    push @lines, $line;
}
close IN;

# remove html and pod flags in tidy mode
if ( $format ne 'html' ) {
    my @newlines;
    foreach my $line (@lines) {
        my $key = $line;
        $key =~ s/^--//;

        #next if ( $key =~ /html/ || $key =~ /pod/ );
        next
          if ( $key =~
/^(html|nohtml|pod|nopod|backlink|cachedir|stylesheet|profile|libpods|frames|title)/
          );
        push @newlines, $line;
    }
    @lines = @newlines;
}

foreach my $line (@lines) {
    print STDOUT $line . "\n";
}

sub get_defaults {

    # get latest parameters from perltidy
    use File::Temp qw(tempfile);
    my %is_default;
    my ( $fout, $tmpnam ) = File::Temp::tempfile();
    if ( !$fout ) { die "cannot get tempfile\n" }
    system "perltidy --dump-defaults >$tmpnam";
    open( IN, "<", $tmpnam ) || die "cannot open $tmpnam: $!\n";

    while ( my $line = <IN> ) {
        next if $line =~ /#/;
        $line         =~ s/^\s+//;
        $line         =~ s/\s+$//;
        $is_default{$line} = 1;
    }
    close IN;
    unlink $tmpnam if ( -e $tmpnam );
    return \%is_default;
}
