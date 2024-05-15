# Created with: ./make_t.pl

# Contents:
#1 git143.def
#2 git143.git143

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
        'git143' => "-atc -wtc=h",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'git143' => <<'----------',
# include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'git143.def' => {
            source => "git143",
            params => "def",
            expect => <<'#1...........',
            # include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
#1...........
        },

        'git143.git143' => {
            source => "git143",
            params => "git143",
            expect => <<'#2...........',
            # include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ],
            );
#2...........
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
