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

# The test file has no tidying needs but is UTF-8 encoded, so all passes
# through perltidy should read/write identical contents (previously only
# file test behaved correctly)

# The original version did hex compares of source and destination streams.  To
# just test the -eos flag, and avoid line ending issues, this version does
# line-by-line hex tests on chomped lines.

plan( tests => 6 );

test_all();

sub test_all {
    my $test_file = "$Bin/testwide-passthrough.pl.src";
    test_file2file($test_file);
    test_scalar2scalar($test_file);
    test_scalararray2scalararray($test_file);
}

sub test_file2file {
    my $test_file = shift;

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

    my $source_str      = slurp_raw($source);
    my $destination_str = slurp_raw($destination);

    my $source_hex      = unpack( 'H*', $source_str );
    my $destination_hex = unpack( 'H*', $destination_str );
    note("Comparing contents:\n  $source_hex\n  $destination_hex\n");

    #ok($source_hex eq $destination_hex, 'file content compare');
    ok( hex_compare_by_lines( $source_str, $destination_str ) );
}

sub test_scalar2scalar {
    my $testfile = shift;

    my $source = slurp_raw($testfile);
    my $destination;

    note("Testing scalar2scalar\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos',
        source      => \$source,
        destination => \$destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $source_hex      = unpack( 'H*', $source );
    my $destination_hex = unpack( 'H*', $destination );

    note("Comparing contents:\n  $source_hex\n  $destination_hex\n");
    ##ok($source_hex eq $destination_hex, 'scalar content compare');
    ok( hex_compare_by_lines( $source, $destination ),
        'scalar content compare' );
}

sub test_scalararray2scalararray {
    my $testfile = shift;

    my $source      = [ lines_raw($testfile) ];
    my $destination = [];

    note("Testing scalararray2scalararray\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos',
        source      => $source,
        destination => $destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $source_str      = join( "", @$source );
    my $destination_str = join( "", @$destination );

    my $source_hex      = unpack( 'H*', $source_str );
    my $destination_hex = unpack( 'H*', $destination_str );

    note("Comparing contents:\n  $source_hex\n  $destination_hex\n");
    ##ok($source_hex eq $destination_hex, 'scalararray content compare');
    ok( hex_compare_by_lines( $source_str, $destination_str ),
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

