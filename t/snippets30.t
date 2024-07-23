# Created with: ./make_t.pl

# Contents:
#1 git143.def
#2 git143.git143
#3 git146.def
#4 git146.git146
#5 altc.altc1
#6 altc.altc2
#7 altc.def
#8 dltc.def
#9 dltc.dltc1
#10 dltc.dltc2
#11 logical_xor.def

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
        'altc1'  => "-atc -wtc=m",
        'altc2'  => "-altc -atc -wtc=m",
        'def'    => "",
        'dltc1'  => "-dtc -wtc=0",
        'dltc2'  => "-dtc -wtc=0 -ndltc",
        'git143' => "-atc -wtc=h",
        'git146' => <<'----------',
# testing three dash parameters
---add-trailing-commas
---unknown-future-option
---wtc=h
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'altc' => <<'----------',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
----------

        'dltc' => <<'----------',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
----------

        'git143' => <<'----------',
# include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
----------

        'git146' => <<'----------',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
----------

        'logical_xor' => <<'----------',
$x^^$y and say "One of x or y is true, but not both";
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

        'git146.def' => {
            source => "git146",
            params => "def",
            expect => <<'#3...........',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
#3...........
        },

        'git146.git146' => {
            source => "git146",
            params => "git146",
            expect => <<'#4...........',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ],
            );
#4...........
        },

        'altc.altc1' => {
            source => "altc",
            params => "altc1",
            expect => <<'#5...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    }
);
#5...........
        },

        'altc.altc2' => {
            source => "altc",
            params => "altc2",
            expect => <<'#6...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
#6...........
        },

        'altc.def' => {
            source => "altc",
            params => "def",
            expect => <<'#7...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
#7...........
        },

        'dltc.def' => {
            source => "dltc",
            params => "def",
            expect => <<'#8...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
#8...........
        },

        'dltc.dltc1' => {
            source => "dltc",
            params => "dltc1",
            expect => <<'#9...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
#9...........
        },

        'dltc.dltc2' => {
            source => "dltc",
            params => "dltc2",
            expect => <<'#10...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    },
);
#10...........
        },

        'logical_xor.def' => {
            source => "logical_xor",
            params => "def",
            expect => <<'#11...........',
$x ^^ $y and say "One of x or y is true, but not both";
#11...........
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
