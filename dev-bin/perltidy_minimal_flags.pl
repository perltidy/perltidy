#!/usr/bin/perl -w
use strict;

my $usage = <<EOM;

Read one or more profile files written by perltidy_random_setup.pl and
write a reduced version which omits default and other non-essential
parameters.  This is an aid for debugging blinkers.

Usage:
   $0 profile.1 [ profile.2 [ ...

Writes:
   profile.1.min [ profile.2.min [ ...

EOM

if (!@ARGV) {
   die $usage;
}
my @files       = @ARGV;
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

foreach my $file (@files) {
    my @lines;
    my $format = "tidy";
    open( IN, "<", $file ) || die "cannot open $file: $!\n";
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

    my $ofile = $file . ".min";
    open( OUT, ">", $ofile ) || die "cannot open $ofile: $!\n";
    foreach my $line (@lines) {
        print OUT $line . "\n";
    }
    close OUT;
}

sub get_defaults {

    # get latest parameters from perltidy
    use File::Temp qw(tempfile);
    my $ris_default = {};
    my ( $fout, $tmpnam ) = File::Temp::tempfile();
    if ( !$fout ) { die "cannot get tempfile\n" }
    my @parameters;
    system "perltidy --dump-defaults >$tmpnam";
    open( IN, "<", $tmpnam ) || die "cannot open $tmpnam: $!\n";

    while ( my $line = <IN> ) {
        next if $line =~ /#/;
        $line         =~ s/^\s+//;
        $line         =~ s/\s+$//;
        $ris_default->{$line} = 1;
    }
    close IN;
    unlink $tmpnam if ( -e $tmpnam );
    return $ris_default;
}
