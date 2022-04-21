# Created with: ./make_t.pl

# Contents:
#1 bal.bal2
#2 bal.def
#3 lpxl.lpxl6
#4 c133.c133
#5 c133.def
#6 git93.def
#7 git93.git93
#8 c139.def

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
        'c133'  => "-boc",
        'def'   => "",
        'git93' => <<'----------',
-vxl='q'
----------
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

        'c133' => <<'----------',
# this will make 1 line unless -boc is used
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)
);

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
----------

        'c139' => <<'----------',
# The '&' has trailing spaces
@l = &    
_  
( -49, -71 );

# This '$' has trailing spaces
my $    
b = 40;

# this arrow has trailing spaces
$r = $c->         
sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );

# spaces and blank line
@l = &    

_  
( -49, -71 );

# spaces and blank line
$r = $c->         

sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );
----------

        'git93' => <<'----------',
use Cwd qw[cwd];
use Carp qw(carp);
use IPC::Cmd qw{can_run run QUOTE};
use File::Path qw/mkpath/;
use File::Temp qw[tempdir];
use Params::Check qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue ) => 'blue' ],
    [ qw( brown blue blue blue ) => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
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

        'c133.c133' => {
            source => "c133",
            params => "c133",
            expect => <<'#4...........',
# this will make 1 line unless -boc is used
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)
);

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
#4...........
        },

        'c133.def' => {
            source => "c133",
            params => "def",
            expect => <<'#5...........',
# this will make 1 line unless -boc is used
return ( $x * cos($a) - $y * sin($a), $x * sin($a) + $y * cos($a) );

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
#5...........
        },

        'git93.def' => {
            source => "git93",
            params => "def",
            expect => <<'#6...........',
use Cwd                       qw[cwd];
use Carp                      qw(carp);
use IPC::Cmd                  qw{can_run run QUOTE};
use File::Path                qw/mkpath/;
use File::Temp                qw[tempdir];
use Params::Check             qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue )     => 'blue' ],
    [ qw( brown blue blue blue )    => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword       { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
#6...........
        },

        'git93.git93' => {
            source => "git93",
            params => "git93",
            expect => <<'#7...........',
use Cwd qw[cwd];
use Carp qw(carp);
use IPC::Cmd qw{can_run run QUOTE};
use File::Path qw/mkpath/;
use File::Temp qw[tempdir];
use Params::Check qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue )     => 'blue' ],
    [ qw( brown blue blue blue )    => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword       { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
#7...........
        },

        'c139.def' => {
            source => "c139",
            params => "def",
            expect => <<'#8...........',
# The '&' has trailing spaces
@l = &_( -49, -71 );

# This '$' has trailing spaces
my $b = 40;

# this arrow has trailing spaces
$r = $c->sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );

# spaces and blank line
@l = &

  _( -49, -71 );

# spaces and blank line
$r = $c->

  sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );
#8...........
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
