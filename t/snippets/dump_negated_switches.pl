#!/usr/bin/perl -w
use strict;
use warnings;

# Dump a list of perltidy abbreviation flags which can be negated.
# This list goes in the man page section near the bottom titled
# SWITCHES WHICH MAY BE NEGATED
main();

sub main {

    # Get the long names which may be negated
    my $rlong_names = get_long_names();
    my @binary_long_names;
    foreach my $long_name ( @{$rlong_names} ) {
        my ( $name, $flag ) = ( "", "" );
        $long_name =~ s/\s+$//;
        if ( $long_name =~ /^([\w\-]+)([^\s]*)/ ) {
            $name = $1;
            $flag = $2;
            $flag = "" unless $flag;
            if ( $flag eq '!' ) { push @binary_long_names, $name }
        }
    }

    # Get a list of all short names
    my $rshort_names = get_short_names();
    my %abbrev;
    foreach my $short_name ( @{$rshort_names} ) {
        if ( $short_name =~ /^(\w+) --> ([\w\-]+)$/ ) {
            my $short = $1;
            my $long  = $2;
            $abbrev{$long} = $short;
        }
    }

    # delete these:
    # -check-syntax or -syn is accepted but no longer does anything
    # -fuzzy-line-length or -fll is accepted but no longer does anything
    my @skip = qw(
      check-syntax
      fuzzy-line-length
    );
    foreach (@skip) {
        if ( $abbrev{$_} ) { delete $abbrev{$_} }
    }

    # Select the short names which can be negated
    my @short_list;
    foreach my $long (@binary_long_names) {
        my $short = $abbrev{$long};
        if ( !defined($short) ) {
            next;
        }
        push @short_list, $short;
    }

    # special aliases not obtained automatically
    my @special = qw(
      oll
      dac
      tac
      html
      sob
      baa
      bbs
      kgb
      icp
      otr
      sot
      sct
      sac
      sobb
      conv
    );

    print <<EOM;
=head1 SWITCHES WHICH MAY BE NEGATED

The following list shows all short parameter names which allow a prefix
'n' to produce the negated form:

EOM

    my $FIELD_WIDTH    = 6;
    my $WORDS_PER_LINE = 10;
    my $line           = " ";
    my $count          = 0;
    foreach my $word ( sort ( @short_list, @special ) ) {
        my $len = length($word);
        my $nsp = $FIELD_WIDTH - $len + 1;
        $word .= " " x $nsp;
        $line .= $word;
        $count++;
        if ( $count == $WORDS_PER_LINE ) {
            $line =~ s/\s+$//;
            print "$line\n";
            $count = 0;
            $line  = " ";
        }
    }

    $line =~ s/\s+$//;
    print "$line\n";

    print <<EOM;

Equivalently, the prefix 'no' or 'no-' on the corresponding long names may be
used.

EOM
} ## end sub main

sub get_long_names {

    # get latest parameters from perltidy
    use File::Temp qw(tempfile);
    my ( $fout, $tmpnam ) = File::Temp::tempfile();
    if ( !$fout ) { die "cannot get tempfile\n" }
    my @parameters;
    system "perltidy --dump-long-names >$tmpnam";
    open( IN, "<", $tmpnam ) || die "cannot open $tmpnam: $!\n";
    while ( my $line = <IN> ) {
        next if $line =~ /#/;
        chomp $line, push @parameters, $line;
    }
    close IN;
    unlink $tmpnam if ( -e $tmpnam );
    return \@parameters;
} ## end sub get_long_names

sub get_short_names {

    # get latest parameters from perltidy
    use File::Temp qw(tempfile);
    my ( $fout, $tmpnam ) = File::Temp::tempfile();
    if ( !$fout ) { die "cannot get tempfile\n" }
    my @parameters;
    system "perltidy --dump-short-names >$tmpnam";
    open( IN, "<", $tmpnam ) || die "cannot open $tmpnam: $!\n";
    while ( my $line = <IN> ) {
        next if $line =~ /#/;
        chomp $line, push @parameters, $line;
    }
    close IN;
    unlink $tmpnam if ( -e $tmpnam );
    return \@parameters;
} ## end sub get_short_names
