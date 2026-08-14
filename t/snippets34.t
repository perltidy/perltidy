# Created with: ./make_t.pl

# Contents:
#1 c622.c622
#2 c622.def
#3 hxs.def
#4 hxs.hxs1
#5 c636.c636
#6 c636.def

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
        'c622' => "-dws -naws",
        'c636' => <<'----------',
-hct=indented
-hiu
-hxs=4
----------
        'def'  => "",
        'hxs1' => <<'----------',
-hct=indented
-hiu
-hxs=4
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'c622' => <<'----------',
# Do not allow user to remove the space after the << (makes a here doc)
my $ONE = 1;
use constant two => 2;
print $ONE << two, "\n";
print "OK\n";
----------

        'c636' => <<'----------',
# test backslash removal
print <<"print \"[BYE!]\n\n\";";
name
rank
serial number

print "[BYE!]\n\n";
----------

        'hxs' => <<'----------',
sub demo {
    my $inside_block = <<'BLOCK';
one
two
BLOCK
}

my @outside_block = (
    <<'LIST',
alpha
beta
LIST
);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'c622.c622' => {
            source => "c622",
            params => "c622",
            expect => <<'#1...........',
# Do not allow user to remove the space after the << (makes a here doc)
my$ONE=1;
use constant two=>2;
print$ONE << two,"\n";
print"OK\n";
#1...........
        },

        'c622.def' => {
            source => "c622",
            params => "def",
            expect => <<'#2...........',
# Do not allow user to remove the space after the << (makes a here doc)
my $ONE = 1;
use constant two => 2;
print $ONE << two, "\n";
print "OK\n";
#2...........
        },

        'hxs.def' => {
            source => "hxs",
            params => "def",
            expect => <<'#3...........',
sub demo {
    my $inside_block = <<'BLOCK';
one
two
BLOCK
}

my @outside_block = (
    <<'LIST',
alpha
beta
LIST
);
#3...........
        },

        'hxs.hxs1' => {
            source => "hxs",
            params => "hxs1",
            expect => <<'#4...........',
sub demo {
    my $inside_block = <<~'BLOCK';
        one
        two
        BLOCK
}

my @outside_block = (
    <<~'LIST',
        alpha
        beta
        LIST
);
#4...........
        },

        'c636.c636' => {
            source => "c636",
            params => "c636",
            expect => <<'#5...........',
# test backslash removal
print <<~"print \"[BYE!]\n\n\";";
    name
    rank
    serial number

    print "[BYE!]\n\n";
#5...........
        },

        'c636.def' => {
            source => "c636",
            params => "def",
            expect => <<'#6...........',
# test backslash removal
print <<"print \"[BYE!]\n\n\";";
name
rank
serial number

print "[BYE!]\n\n";
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
