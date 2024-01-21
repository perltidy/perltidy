# Created with: ./make_t.pl

# Contents:
#1 git125.git125
#2 vsn.def
#3 vsn.vsn1
#4 vsn.vsn2

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test::More;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    ###########################################
    # BEGIN SECTION 1: Parameter combinations #
    ###########################################
    $rparams = {
        'def'    => "",
        'git125' => "-ssp=0",
        'vsn1'   => <<'----------',
-vsn
-gnu
----------
        'vsn2' => <<'----------',
# turn off vsn with -vsnl
-vsn
-vsnl=0
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'git125' => <<'----------',
sub Add ( $x, $y );
sub Sub( $x, $y );
----------

        'vsn' => <<'----------',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'git125.git125' => {
            source => "git125",
            params => "git125",
            expect => <<'#1...........',
sub Add( $x, $y );
sub Sub( $x, $y );
#1...........
        },

        'vsn.def' => {
            source => "vsn",
            params => "def",
            expect => <<'#2...........',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
#2...........
        },

        'vsn.vsn1' => {
            source => "vsn",
            params => "vsn1",
            expect => <<'#3...........',
@data = (
         ["1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"],
         [ 1,     2,    5,     6,      3,     1.5,  -1,    -3,    -4],
         [-4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,     0],
         [ 9,     8,    9,     8.4,    7.1,   7.5,   8,     3,    -3],
         [ 0.1,   0.2,  0.5,   0.4,    0.3,   0.5,   0.1,   0,     0.4],
        );

$s->drawLine( 35,  0);
$s->drawLine( 0,   10);
$s->drawLine(-35,  0);
$s->drawLine( 0,  -10);
#3...........
        },

        'vsn.vsn2' => {
            source => "vsn",
            params => "vsn2",
            expect => <<'#4...........',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
#4...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

###############
# EXECUTE TESTS
###############

foreach my $key ( sort keys %{$rtests} ) {
    my $output;
    my $sname  = $rtests->{$key}->{source};
    my $expect = $rtests->{$key}->{expect};
    my $pname  = $rtests->{$key}->{params};
    my $source = $rsources->{$sname};
    my $params = defined($pname) ? $rparams->{$pname} : "";
    my $stderr_string;
    my $errorfile_string;
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        perltidyrc  => \$params,
        argv        => '',             # for safety; hide any ARGV from perltidy
        stderr      => \$stderr_string,
        errorfile   => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        print STDERR "Error output received for test '$key'\n";
        if ($err) {
            print STDERR "An error flag '$err' was returned\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            ok( !$errorfile_string );
        }
    }
    else {
        if ( !is( $output, $expect, $key ) ) {
            my $leno = length($output);
            my $lene = length($expect);
            if ( $leno == $lene ) {
                print STDERR
"#> Test '$key' gave unexpected output.  Strings differ but both have length $leno\n";
            }
            else {
                print STDERR
"#> Test '$key' gave unexpected output.  String lengths differ: output=$leno, expected=$lene\n";
            }
        }
    }
}
