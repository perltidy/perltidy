use strict;
use warnings;
use utf8;

use FindBin qw($Bin);
use File::Temp qw(tempfile);
use Test::More;

BEGIN {unshift @INC, "./"}
use Perl::Tidy; 

# The test file is UTF-8 encoded

plan(tests => 6);

test_all();

sub test_all
{
	my $test_file = "$Bin/testwide-tidy.pl.src";
	my $tidy_file = "$Bin/testwide-tidy.pl.srctdy";
	my $tidy_hex = unpack('H*', slurp_raw($tidy_file));
	test_file2file($test_file, $tidy_hex);
	test_scalar2scalar($test_file, $tidy_hex);
	test_scalararray2scalararray($test_file, $tidy_hex);
}

sub test_file2file
{
	my $test_file = shift;
	my $tidy_hex = shift;

	my $tmp_file = File::Temp->new( TMPDIR => 1 );

	my $source      = $test_file;
	my $destination = $tmp_file->filename();

	note("Testing file2file: '$source' => '$destination'\n");

	my $tidyresult = Perl::Tidy::perltidy
									(
										argv   => '-utf8',
										source => $source,
										destination => $destination
									);
	ok(!$tidyresult, 'perltidy');

	my $destination_hex = unpack('H*', slurp_raw($destination));

	note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
	ok($tidy_hex eq $destination_hex, 'file content compare');
}

sub test_scalar2scalar
{
	my $test_file = shift;
	my $tidy_hex = shift;
	
	my $source      = slurp_raw($test_file);
	my $destination;

	note("Testing scalar2scalar\n");

	my $tidyresult = Perl::Tidy::perltidy
									(
										argv   => '-utf8 -eos',
										source => \$source,
										destination => \$destination
									);
	ok(!$tidyresult, 'perltidy');

	my $destination_hex = unpack('H*', $destination);

	note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
	ok($tidy_hex eq $destination_hex, 'scalar content compare');
}

sub test_scalararray2scalararray
{
	my $test_file = shift;
	my $tidy_hex = shift;
	
	my $source      = [ lines_raw($test_file) ];
	my $destination = [];

	note("Testing scalararray2scalararray\n");

	my $tidyresult = Perl::Tidy::perltidy
									(
										argv   => '-utf8 -eos',
										source => $source,
										destination => $destination
									);
	ok(!$tidyresult, 'perltidy');

	my $destination_hex = join('', map { unpack('H*', $_) } @$destination);

	note("Comparing contents:\n  $tidy_hex\n  $destination_hex\n");
	ok($tidy_hex eq $destination_hex, 'scalararray content compare');
}

sub slurp_raw
{
	my $filename = shift;
	
    open(TMP, '<', $filename);
    binmode(TMP, ':raw');
	local $/;
	my $contents = <TMP>;
	close(TMP);
	
	return $contents;
}

sub lines_raw
{
	my $filename = shift;
	
    open(TMP, '<', $filename);
    binmode(TMP, ':raw');
	my @contents = <TMP>;
	close(TMP);
	
	return @contents;
}
