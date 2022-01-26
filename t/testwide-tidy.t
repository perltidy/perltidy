use strict;
use warnings;
use utf8;

use FindBin qw($Bin);
use File::Temp qw(tempfile);
use Test::More;

BEGIN { unshift @INC, "./" }
use Perl::Tidy;

# This tests the -eos (--encode-output-strings) which was added for issue
# git #83 to fix an issue with tidyall.

# The test file is UTF-8 encoded

# The original version did hex compares of source and destination streams.  To
# just test the -eos flag, and avoid line ending issues, this version does
# line-by-line hex tests on chomped lines.

plan( tests => 6 );

test_all();

sub test_all {
    my $test_file = "$Bin/testwide-tidy.pl.src";
    my $tidy_file = "$Bin/testwide-tidy.pl.srctdy";
    my $tidy_str  = slurp_raw($tidy_file);
    test_file2file( $test_file, $tidy_str );
    test_scalar2scalar( $test_file, $tidy_str );
    test_scalararray2scalararray( $test_file, $tidy_str );
}

sub test_file2file {
    my $test_file = shift;
    my $tidy_str  = shift;
    my $tidy_hex  = unpack( 'H*', $tidy_str );

    my $tmp_file = File::Temp->new( TMPDIR => 1 );

    my $source      = $test_file;
    my $destination = $tmp_file->filename();

    note("Testing file2file: '$source' => '$destination'\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8',
        source      => $source,
        destination => $destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $destination_str = slurp_raw($destination);
    my $destination_hex = unpack( 'H*', $destination_str );

    note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
    ##ok($tidy_hex eq $destination_hex, 'file content compare');
    ok( hex_compare_by_lines( $tidy_str, $destination_str ),
        'file2file content compare' );
}

sub test_scalar2scalar {
    my $test_file = shift;
    my $tidy_str  = shift;
    my $tidy_hex  = unpack( 'H*', $tidy_str );

    my $source = slurp_raw($test_file);
    my $destination;

    note("Testing scalar2scalar\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos',
        source      => \$source,
        destination => \$destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $destination_hex = unpack( 'H*', $destination );

    note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
    ##ok($tidy_hex eq $destination_hex, 'scalar content compare');
    ok( hex_compare_by_lines( $tidy_str, $destination ),
        'scalar2scalar content compare' );
}

sub test_scalararray2scalararray {
    my $test_file = shift;
    my $tidy_str  = shift;
    my $tidy_hex  = unpack( 'H*', $tidy_str );

    my $source      = [ lines_raw($test_file) ];
    my $destination = [];

    note("Testing scalararray2scalararray\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos',
        source      => $source,
        destination => $destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $destination_str = join( '', @$destination );
    my $destination_hex = unpack( 'H*', $destination_str );

    note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
    ##ok($tidy_hex eq $destination_hex, 'scalararray content compare');
    ok( hex_compare_by_lines( $tidy_str, $destination_str ),
        'scalararray content compare' );
}

sub slurp_raw {
    my $filename = shift;

    open( TMP, '<', $filename );
    binmode( TMP, ':raw' );
    local $/;
    my $contents = <TMP>;
    close(TMP);

    return $contents;
}

sub lines_raw {
    my $filename = shift;

    open( TMP, '<', $filename );
    binmode( TMP, ':raw' );
    my @contents = <TMP>;
    close(TMP);

    return @contents;
}

sub hex_compare_by_lines {
    my ( $source_str, $destination_str ) = @_;

    my @source      = split /^/m, $source_str;
    my @destination = split /^/m, $destination_str;

    while (@source) {
        my $ss = pop(@source);
        my $dd = pop(@destination);
        chomp $ss;
        chomp $dd;
        $ss = unpack( 'H*', $ss );
        $dd = unpack( 'H*', $dd );
        last if $ss ne $dd;
    }
    return !@source && !@destination;
}
