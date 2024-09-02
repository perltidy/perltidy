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

# NOTE: to prevent automatic conversion of line endings LF to CRLF under github
# Actions with Windows, which would cause test failure, it is essential that
# there be a file 't/.gitattributes' with the line:
# * -text

# The test file has no tidying needs but is UTF-8 encoded, so all passes
# through perltidy should read/write identical contents (previously only
# file test behaved correctly)

# Test::More in perl versions before 5.10 does not have sub note
# so just skip this test

plan( tests => 6 );

test_all();

sub my_note {
    my ($msg) = @_;

    # try to work around problem where sub Test::More::note does not exist
    # in older versions of perl
    if ($] > 5.010) {
       note($msg);
    }
    return;
}


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

    my_note("Testing file2file: '$source' => '$destination'\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -npro',
        source      => $source,
        destination => $destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $source_str      = slurp_raw($source);
    my $destination_str = slurp_raw($destination);

    my $source_hex      = unpack( 'H*', $source_str );
    my $destination_hex = unpack( 'H*', $destination_str );
    my_note("Comparing contents:\n  $source_hex\n  $destination_hex\n");

    ok( $source_hex eq $destination_hex, 'file content compare' );
}

sub test_scalar2scalar {
    my $testfile = shift;

    my $source = slurp_raw($testfile);
    my $destination;

    my_note("Testing scalar2scalar\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos -npro',
        source      => \$source,
        destination => \$destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $source_hex      = unpack( 'H*', $source );
    my $destination_hex = unpack( 'H*', $destination );

    my_note("Comparing contents:\n  $source_hex\n  $destination_hex\n");
    ok( $source_hex eq $destination_hex, 'scalar content compare' );
}

sub test_scalararray2scalararray {
    my $testfile = shift;

    my $source      = [ lines_raw($testfile) ];
    my $destination = [];

    my_note("Testing scalararray2scalararray\n");

    my $tidyresult = Perl::Tidy::perltidy(
        argv        => '-utf8 -eos -npro',
        source      => $source,
        destination => $destination
    );
    ok( !$tidyresult, 'perltidy' );

    my $source_str      = join( "", @$source );
    my $destination_str = join( "", @$destination );

    my $source_hex      = unpack( 'H*', $source_str );
    my $destination_hex = unpack( 'H*', $destination_str );

    my_note("Comparing contents:\n  $source_hex\n  $destination_hex\n");
    ok( $source_hex eq $destination_hex, 'scalararray content compare' );
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
