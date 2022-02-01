# Created with: ./make_t.pl

# Contents:
#1 bal.bal2
#2 bal.def
#3 lpxl.lpxl6

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
        'bal2'  => "-bal=2",
        'def'   => "",
        'lpxl6' => <<'----------',
# equivalent to -lpxl='{ [ F(2'
-lp -lpil='f(2'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bal' => <<'----------',
{
  L1:
  L2:
  L3: return;
};
----------

        'lpxl' => <<'----------',
# simple function call
my $loanlength = getLoanLength(
                                $borrower->{'categorycode'},    # sc1
                                $iteminformation->{'itemtype'},
                                $borrower->{'branchcode'}       # sc3
);

# function call, more than one level deep
my $o = very::long::class::name->new(
    {
        propA => "a",
        propB => "b",
        propC => "c",
    }
);

# function call with sublist
debug(
      "Connecting to DB.",
      "Extra-Parameters: " . join("<->", $extra_parms),
      "Config: " . join("<->", %config)
     );

# simple function call with code block
$m->command(-label   => 'Save',
            -command => sub { print "DOS\n"; save_dialog($win); });

# function call, ternary in list
return
  OptArgs2::Result->usage(
    $style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage',
    'usage: ' . $usage . "\n" );

# not a function call
%blastparam = (
    -run            => \%runparam,
    -file           => '',
    -parse          => 1,
    -signif         => 1e-5,
);

# 'local' is a keyword, not a user function
    local (
        $len,    $pts,      @colspec, $char, $cols,
        $repeat, $celldata, $at_text, $after_text
    );

# square bracket with sublists
$data = [
         ListElem->new(id => 0, val => 100),
         ListElem->new(id => 2, val => 50),
         ListElem->new(id => 1, val => 10),
        ];

# curly brace with sublists
$behaviour = {
              cat   => {nap    => "lap",   eat  => "meat"},
              dog   => {prowl  => "growl", pool => "drool"},
              mouse => {nibble => "kibble"},
             };
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'bal.bal2' => {
            source => "bal",
            params => "bal2",
            expect => <<'#1...........',
{
  L1: L2: L3: return;
};
#1...........
        },

        'bal.def' => {
            source => "bal",
            params => "def",
            expect => <<'#2...........',
{
  L1:
  L2:
  L3: return;
};
#2...........
        },

        'lpxl.lpxl6' => {
            source => "lpxl",
            params => "lpxl6",
            expect => <<'#3...........',
# simple function call
my $loanlength = getLoanLength(
                                $borrower->{'categorycode'},    # sc1
                                $iteminformation->{'itemtype'},
                                $borrower->{'branchcode'}       # sc3
);

# function call, more than one level deep
my $o = very::long::class::name->new(
    {
        propA => "a",
        propB => "b",
        propC => "c",
    }
);

# function call with sublist
debug(
    "Connecting to DB.",
    "Extra-Parameters: " . join( "<->", $extra_parms ),
    "Config: " . join( "<->", %config )
);

# simple function call with code block
$m->command(
    -label   => 'Save',
    -command => sub { print "DOS\n"; save_dialog($win); }
);

# function call, ternary in list
return OptArgs2::Result->usage(
    $style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage',
    'usage: ' . $usage . "\n" );

# not a function call
%blastparam = (
    -run    => \%runparam,
    -file   => '',
    -parse  => 1,
    -signif => 1e-5,
);

# 'local' is a keyword, not a user function
local (
    $len,    $pts,      @colspec, $char, $cols,
    $repeat, $celldata, $at_text, $after_text
);

# square bracket with sublists
$data = [
    ListElem->new( id => 0, val => 100 ),
    ListElem->new( id => 2, val => 50 ),
    ListElem->new( id => 1, val => 10 ),
];

# curly brace with sublists
$behaviour = {
    cat   => { nap    => "lap",   eat  => "meat" },
    dog   => { prowl  => "growl", pool => "drool" },
    mouse => { nibble => "kibble" },
};
#3...........
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
