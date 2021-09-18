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
#11 git63.def
#12 align35.def
#13 rt136417.def
#14 rt136417.rt136417
#15 numbers.def
#16 code_skipping.def
#17 git51.def
#18 git51.git51
#19 pretok.def

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
        'fpva2' => <<'----------',
-sfp -wls='->' -wrs='->' -nfpva
----------
        'git51' => <<'----------',
--maximum-line-length=120
--converge
--tabs
--entab-leading-whitespace=4
--continuation-indentation=4
--extended-continuation-indentation
--no-delete-old-newlines
--no-outdent-long-lines
--no-outdent-labels
--novalign
--no-logical-padding
--opening-sub-brace-on-new-line
--square-bracket-tightness=2
--paren-tightness=2
--brace-tightness=2
--opening-token-right

-sal='first any sum sum0 reduce'
----------
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
        'rt136417' => "-vtc=3",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align35' => <<'----------',
# different module names, do not align commas (fixes rt136416)
use File::Spec::Functions 'catfile', 'catdir';
use Mojo::Base 'Mojolicious', '-signatures';

# same module names, align fat commas
use constant PI => 4 * atan2 1, 1;
use constant TWOPI => 2 * PI;
use constant FOURPI => 4 * PI;

# same module names, align commas
use TestCounter '3rd-party', 0, '3rd-party no longer visible';
use TestCounter 'replace', 1, 'replacement now visible';
use TestCounter 'root';

# same module name, align fat commas but not commas
use constant COUNTDOWN => scalar reverse 1, 2, 3, 4, 5;
use constant COUNTUP => reverse 1, 2, 3, 4, 5;
use constant COUNTDOWN => scalar reverse 1, 2, 3, 4, 5;
----------

        'code_skipping' => <<'----------',
%Hdr=%U2E=%E2U=%Fallback=();
$in_charmap=$nerror=$nwarning=0;
$.=0;
#<<V  code skipping: perltidy will pass this verbatim without error checking

    }}} {{{

#>>V
my $self=shift;
my $cloning=shift;
----------

        'fpva' => <<'----------',
log_something_with_long_function( 'This is a log message.', 2 );
Coro::AnyEvent::sleep( 3, 4 );
use Carp ();
use File::Spec ();
use File::Path ();
$self -> method ( 'parameter_0', 'parameter_1' );
$self -> method_with_long_name ( 'parameter_0', 'parameter_1' );
----------

        'git51' => <<'----------',
Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
		), # <--- brace here
);
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

        'git63' => <<'----------',
my $fragment = $parser-> #parse_html_string
  parse_balanced_chunk($I);
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

        'numbers' => <<'----------',
# valid numbers
my @vals = (

    12345,
    12345.67,
    .23E-10,
    3.14_15_92,
    4_294_967_296,
    0xff,
    0xdead_beef,
    0377,
    0b011011,
    0x1.999ap-4,
    1e34,
    1e+34,
    1e+034,
    -1e+034,
    0.00000000000000000000000000000000000000000000000000000000000000000001,
    0Xabcdef,
    0B1101,
    0o12_345,  # optional 'o' and 'O' added in perl v5.33.5
    0O12_345,
);
----------

        'pretok' => <<'----------',
# test sub split_pretoken
my$s1=$^??"def":"not def";
my$s2=$^ ?"def":"not def";
my$s3=$^if($s2);
my$s4=$^Oeq"linux";
my$s5=$  ^One"linux";
my$s6=$
  ^One"linux";
my$s7=%^O;
my$s8='hi'.'s'x10if(1);
my$s9='merci'x0.1e4.$s8;
----------

        'rt136417' => <<'----------',
function(
  #
  a, b, c);

%hash = (
  a => b,
  c => d,
);
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
$self->method( 'parameter_0', 'parameter_1' );
$self->method_with_long_name( 'parameter_0', 'parameter_1' );
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
$self->method                ( 'parameter_0', 'parameter_1' );
$self->method_with_long_name ( 'parameter_0', 'parameter_1' );
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
$self -> method ( 'parameter_0', 'parameter_1' );
$self -> method_with_long_name ( 'parameter_0', 'parameter_1' );
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

        'git63.def' => {
            source => "git63",
            params => "def",
            expect => <<'#11...........',
my $fragment = $parser->    #parse_html_string
  parse_balanced_chunk($I);
#11...........
        },

        'align35.def' => {
            source => "align35",
            params => "def",
            expect => <<'#12...........',
# different module names, do not align commas (fixes rt136416)
use File::Spec::Functions 'catfile', 'catdir';
use Mojo::Base 'Mojolicious', '-signatures';

# same module names, align fat commas
use constant PI     => 4 * atan2 1, 1;
use constant TWOPI  => 2 * PI;
use constant FOURPI => 4 * PI;

# same module names, align commas
use TestCounter '3rd-party', 0, '3rd-party no longer visible';
use TestCounter 'replace',   1, 'replacement now visible';
use TestCounter 'root';

# same module name, align fat commas but not commas
use constant COUNTDOWN => scalar reverse 1, 2, 3, 4, 5;
use constant COUNTUP   => reverse 1, 2, 3, 4, 5;
use constant COUNTDOWN => scalar reverse 1, 2, 3, 4, 5;
#12...........
        },

        'rt136417.def' => {
            source => "rt136417",
            params => "def",
            expect => <<'#13...........',
function(
    #
    a, b, c
);

%hash = (
    a => b,
    c => d,
);
#13...........
        },

        'rt136417.rt136417' => {
            source => "rt136417",
            params => "rt136417",
            expect => <<'#14...........',
function(
    #
    a, b, c );

%hash = (
    a => b,
    c => d,
);
#14...........
        },

        'numbers.def' => {
            source => "numbers",
            params => "def",
            expect => <<'#15...........',
# valid numbers
my @vals = (

    12345,
    12345.67,
    .23E-10,
    3.14_15_92,
    4_294_967_296,
    0xff,
    0xdead_beef,
    0377,
    0b011011,
    0x1.999ap-4,
    1e34,
    1e+34,
    1e+034,
    -1e+034,
    0.00000000000000000000000000000000000000000000000000000000000000000001,
    0Xabcdef,
    0B1101,
    0o12_345,    # optional 'o' and 'O' added in perl v5.33.5
    0O12_345,
);
#15...........
        },

        'code_skipping.def' => {
            source => "code_skipping",
            params => "def",
            expect => <<'#16...........',
%Hdr        = %U2E    = %E2U      = %Fallback = ();
$in_charmap = $nerror = $nwarning = 0;
$.          = 0;
#<<V  code skipping: perltidy will pass this verbatim without error checking

    }}} {{{

#>>V
my $self    = shift;
my $cloning = shift;
#16...........
        },

        'git51.def' => {
            source => "git51",
            params => "def",
            expect => <<'#17...........',
Type::Libraries->setup_class(
    __PACKAGE__,
    qw(
      Types::Standard
      Types::Common::Numeric
    ),    # <--- brace here
);
#17...........
        },

        'git51.git51' => {
            source => "git51",
            params => "git51",
            expect => <<'#18...........',
Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
	),    # <--- brace here
);
#18...........
        },

        'pretok.def' => {
            source => "pretok",
            params => "def",
            expect => <<'#19...........',
# test sub split_pretoken
my $s1 = $^? ? "def" : "not def";
my $s2 = $^  ? "def" : "not def";
my $s3 = $^ if ($s2);
my $s4 = $^O eq "linux";
my $s5 = $^O ne "linux";
my $s6 = $^O ne "linux";
my $s7 = %^O;
my $s8 = 'hi' . 's' x 10 if (1);
my $s9 = 'merci' x 0.1e4 . $s8;
#19...........
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
