# Created with: ./make_t.pl

# Contents:
#1 git54.def
#2 git54.git54
#3 fpva.def
#4 fpva.fpva1
#5 fpva.fpva2
#6 lpxl.def
#7 lpxl.lpxl1
#8 lpxl.lpxl3
#9 lpxl.lpxl4
#10 lpxl.lpxl5

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
        'def'   => "",
        'fpva1' => "-sfp",
        'fpva2' => "-sfp -nfpva",
        'git54' => "-bbp=3 -bbpi=2 -ci=4 -lp",
        'lpxl1' => "-lp",
        'lpxl3' => <<'----------',
-lp -lpxl='{ [ ('
----------
        'lpxl4' => <<'----------',
-lp -lpxl='{ [ W(1'
----------
        'lpxl5' => <<'----------',
-lp -lpxl='{ [ F(2'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'fpva' => <<'----------',
log_something_with_long_function( 'This is a log message.', 2 );
Coro::AnyEvent::sleep( 3, 4 );
use Carp ();
use File::Spec ();
use File::Path ();
----------

        'git54' => <<'----------',
# testing sensitivity to excess commas
my $definition =>
    (
    {
        key1 => value1
    },
    {
        key2 => value2
    },
    );

my $definition =>
    (
    {
        key => value
    }
    );

my $definition =>
    (
    {
        key => value
    },
    );

my $definition =>
    (
    {
        key => value,
    },
    );

my $list =
    (
      {
        key => $value,
        key => $value,
        key => $value,
        key => $value,
        key => $value,
      },
    ) ;

my $list =
    (
      {
        key => $value,
        key => $value,
        key => $value,
        key => $value,
        key => $value,
      }
    ) ;
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

        'git54.def' => {
            source => "git54",
            params => "def",
            expect => <<'#1...........',
# testing sensitivity to excess commas
my $definition => (
    {
        key1 => value1
    },
    {
        key2 => value2
    },
);

my $definition => (
    {
        key => value
    }
);

my $definition => (
    {
        key => value
    },
);

my $definition => (
    {
        key => value,
    },
);

my $list = (
    {
        key => $value,
        key => $value,
        key => $value,
        key => $value,
        key => $value,
    },
);

my $list = (
    {
        key => $value,
        key => $value,
        key => $value,
        key => $value,
        key => $value,
    }
);
#1...........
        },

        'git54.git54' => {
            source => "git54",
            params => "git54",
            expect => <<'#2...........',
# testing sensitivity to excess commas
my $definition =>
    (
      {
         key1 => value1
      },
      {
         key2 => value2
      },
    );

my $definition =>
    (
      {
        key => value
      }
    );

my $definition =>
    (
      {
         key => value
      },
    );

my $definition =>
    (
      {
         key => value,
      },
    );

my $list =
    (
      {
         key => $value,
         key => $value,
         key => $value,
         key => $value,
         key => $value,
      },
    );

my $list =
    (
      {
        key => $value,
        key => $value,
        key => $value,
        key => $value,
        key => $value,
      }
    );
#2...........
        },

        'fpva.def' => {
            source => "fpva",
            params => "def",
            expect => <<'#3...........',
log_something_with_long_function( 'This is a log message.', 2 );
Coro::AnyEvent::sleep( 3, 4 );
use Carp       ();
use File::Spec ();
use File::Path ();
#3...........
        },

        'fpva.fpva1' => {
            source => "fpva",
            params => "fpva1",
            expect => <<'#4...........',
log_something_with_long_function ( 'This is a log message.', 2 );
Coro::AnyEvent::sleep            ( 3, 4 );
use Carp       ();
use File::Spec ();
use File::Path ();
#4...........
        },

        'fpva.fpva2' => {
            source => "fpva",
            params => "fpva2",
            expect => <<'#5...........',
log_something_with_long_function ( 'This is a log message.', 2 );
Coro::AnyEvent::sleep ( 3, 4 );
use Carp ();
use File::Spec ();
use File::Path ();
#5...........
        },

        'lpxl.def' => {
            source => "lpxl",
            params => "def",
            expect => <<'#6...........',
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
#6...........
        },

        'lpxl.lpxl1' => {
            source => "lpxl",
            params => "lpxl1",
            expect => <<'#7...........',
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
$m->command( -label   => 'Save',
             -command => sub { print "DOS\n"; save_dialog($win); } );

# function call, ternary in list
return
  OptArgs2::Result->usage(
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
#7...........
        },

        'lpxl.lpxl3' => {
            source => "lpxl",
            params => "lpxl3",
            expect => <<'#8...........',
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
    -command => sub { print "DOS\n"; save_dialog($win); } );

# function call, ternary in list
return
  OptArgs2::Result->usage(
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
#8...........
        },

        'lpxl.lpxl4' => {
            source => "lpxl",
            params => "lpxl4",
            expect => <<'#9...........',
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
$m->command( -label   => 'Save',
             -command => sub { print "DOS\n"; save_dialog($win); } );

# function call, ternary in list
return
  OptArgs2::Result->usage(
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
#9...........
        },

        'lpxl.lpxl5' => {
            source => "lpxl",
            params => "lpxl5",
            expect => <<'#10...........',
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
    -command => sub { print "DOS\n"; save_dialog($win); } );

# function call, ternary in list
return
  OptArgs2::Result->usage(
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
#10...........
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
