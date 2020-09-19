# Created with: ./make_t.pl

# Contents:
#1 here_long.here_long
#2 bbhb.bbhb2
#3 bbhb.bbhb3
#4 bbhb.def
#5 bbhb.bbhb4
#6 bbhb.bbhb5

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
        'bbhb2'     => "-bbhb=2 -bbp=2",
        'bbhb3'     => "-bbhb=3 -bbp=3",
        'bbhb4'     => "-bbhb=3 -bbp=3 -bbhbi=2 -bbpi=2",
        'bbhb5'     => "-bbhb=3 -bbp=3 -bbhbi=1 -bbpi=1",
        'def'       => "",
        'here_long' => "-l=33",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bbhb' => <<'----------',
my %temp = 
( 
supsup => 123, 
nested => { 
asdf => 456, 
yarg => 'yarp', 
}, );
----------

        'here_long' => <<'----------',
# must not break after here target regardless of maximum-line-length
$sth= $dbh->prepare (<<"END_OF_SELECT") or die "Couldn't prepare SQL" ;
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'here_long.here_long' => {
            source => "here_long",
            params => "here_long",
            expect => <<'#1...........',
# must not break after here target regardless of maximum-line-length
$sth = $dbh->prepare(
    <<"END_OF_SELECT") or die "Couldn't prepare SQL";
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

#1...........
        },

        'bbhb.bbhb2' => {
            source => "bbhb",
            params => "bbhb2",
            expect => <<'#2...........',
my %temp =
  (
    supsup => 123,
    nested =>
      {
        asdf => 456,
        yarg => 'yarp',
      },
  );
#2...........
        },

        'bbhb.bbhb3' => {
            source => "bbhb",
            params => "bbhb3",
            expect => <<'#3...........',
my %temp =
  (
    supsup => 123,
    nested =>
      {
        asdf => 456,
        yarg => 'yarp',
      },
  );
#3...........
        },

        'bbhb.def' => {
            source => "bbhb",
            params => "def",
            expect => <<'#4...........',
my %temp = (
    supsup => 123,
    nested => {
        asdf => 456,
        yarg => 'yarp',
    },
);
#4...........
        },

        'bbhb.bbhb4' => {
            source => "bbhb",
            params => "bbhb4",
            expect => <<'#5...........',
my %temp =
    (
    supsup => 123,
    nested =>
        {
        asdf => 456,
        yarg => 'yarp',
        },
    );
#5...........
        },

        'bbhb.bbhb5' => {
            source => "bbhb",
            params => "bbhb5",
            expect => <<'#6...........',
my %temp =
(
    supsup => 123,
    nested =>
    {
        asdf => 456,
        yarg => 'yarp',
    },
);
#6...........
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
