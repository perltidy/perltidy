# Created with: ./make_t.pl

# Contents:
#1 space6.def
#2 space6.space6

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
        'space6' => <<'----------',
-nwrs="+ - / *"
-nwls="+ - / *"
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'space6' => <<'----------',
# test some spacing rules at possible filehandles
my $z=$x/$y;     # ok to change spaces around both sides of the /
print $x / $y;   # do not remove space before or after / here
print $x/$y;     # do not add a space before the / here
print $x+$y;     # do not add a space before the + here
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'space6.def' => {
            source => "space6",
            params => "def",
            expect => <<'#1...........',
# test some spacing rules at possible filehandles
my $z = $x / $y;    # ok to change spaces around both sides of the /
print $x / $y;      # do not remove space before or after / here
print $x/ $y;       # do not add a space before the / here
print $x+ $y;       # do not add a space before the + here
#1...........
        },

        'space6.space6' => {
            source => "space6",
            params => "space6",
            expect => <<'#2...........',
# test some spacing rules at possible filehandles
my $z = $x/$y;    # ok to change spaces around both sides of the /
print $x / $y;    # do not remove space before or after / here
print $x/$y;      # do not add a space before the / here
print $x+$y;      # do not add a space before the + here
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
        errorfile => \$errorfile_string,    # not used when -se flag is set
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
