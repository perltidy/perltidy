# Created with: ./make_t.pl

# Contents:
#1 btct.btct2
#2 btct.btct3
#3 btct.def
#4 c424.c424
#5 c424.def

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
        'btct2' => "-btct=1 -atc -wtc=1",
        'btct3' => "-btct=1 -atc -wtc=1",
        'c424'  => "-naws -qwaf",
        'def'   => "",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'btct' => <<'----------',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind( '<Page_Down>' => xx,);

$w->bind(
    '<Page_Down>' => xx
);

$w->bind( '<Page_Down>' => xx);

$lut = byte [ [ 0, 0, 0 ], [ 10, 1, 10 ], [ 2, 20, 20 ], [ 30, 30, 3 ], ];
----------

        'c424' => <<'----------',
my @chars = qw(   | / - \ | / - \    );
my @chars = qw(| / - \ | / - \ );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'btct.btct2' => {
            source => "btct",
            params => "btct2",
            expect => <<'#1...........',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$lut = byte [
    [
        0, 0, 0,
    ], [
        10, 1, 10,
    ], [
        2, 20, 20,
    ], [
        30, 30, 3,
    ],
];
#1...........
        },

        'btct.btct3' => {
            source => "btct",
            params => "btct3",
            expect => <<'#2...........',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$lut = byte [
    [
        0, 0, 0,
    ], [
        10, 1, 10,
    ], [
        2, 20, 20,
    ], [
        30, 30, 3,
    ],
];
#2...........
        },

        'btct.def' => {
            source => "btct",
            params => "def",
            expect => <<'#3...........',
$w->bind( '<Page_Down>' => xx, );

$w->bind( '<Page_Down>' => xx, );

$w->bind( '<Page_Down>' => xx );

$w->bind( '<Page_Down>' => xx );

$lut = byte [ [ 0, 0, 0 ], [ 10, 1, 10 ], [ 2, 20, 20 ], [ 30, 30, 3 ], ];
#3...........
        },

        'c424.c424' => {
            source => "c424",
            params => "c424",
            expect => <<'#4...........',
my @chars = qw( | / - \ | / - \ );
my @chars = qw(| / - \ | / - \ );
#4...........
        },

        'c424.def' => {
            source => "c424",
            params => "def",
            expect => <<'#5...........',
my @chars = qw(   | / - \ | / - \    );
my @chars = qw(| / - \ | / - \ );
#5...........
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
