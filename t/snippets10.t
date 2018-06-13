# **This script was automatically generated**
# Created with: ./make_t.pl
# Tue Jun 12 19:09:24 2018

# To locate test #13 for example, search for the string '#13'

use strict;
use Test;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    #####################################
    # SECTION 1: Parameter combinations #
    #####################################
    $rparams = {
        'def'    => "",
        'sil'    => "-sil=0",
        'style1' => <<'----------',
-b
-se
-w
-i=2
-l=100
-nolq
-bbt=1
-bt=2
-pt=2
-nsfs
-sbt=2
-sbvt=2
-nhsc
-isbc
-bvt=2
-pvt=2
-wbb="% + - * / x != == >= <= =~ < > | & **= += *= &= <<= &&= -= /= |= >>= ||= .= %= ^= x="
-mbl=2
----------
        'style2' => <<'----------',
-bt=2
-nwls=".."
-nwrs=".."
-pt=2
-nsfs
-sbt=2
-cuddled-blocks
-bar
-nsbl
-nbbc
----------
        'style3' => <<'----------',
-l=160
-cbi=1
-cpi=1
-csbi=1
-lp
-nolq
-csci=20
-csct=40
-csc
-isbc
-cuddled-blocks
-nsbl
-dcsc
----------
        'style4' => <<'----------',
-bt=2
-pt=2
-sbt=2
-cuddled-blocks
-bar
----------
        'style5' => <<'----------',
-b
-bext="~"
-et=8
-l=77
-cbi=2
-cpi=2
-csbi=2
-ci=4
-nolq
-nasc
-bt=2
-ndsm
-nwls="++ -- ?"
-nwrs="++ --"
-pt=2
-nsfs
-nsts
-sbt=2
-sbvt=1
-wls="= .= =~ !~ :"
-wrs="= .= =~ !~ ? :"
-ncsc
-isbc
-msc=2
-nolc
-bvt=1
-bl
-sbl
-pvt=1
-wba="% + - * / x != == >= <= =~ !~ < > | & >= < = **= += *= &= <<= &&= -= /= |= >>= ||= .= %= ^= x= . << >> -> && ||"
-wbb=" "
-cab=1
-mbl=2
----------
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

        'side_comments1' => <<'----------',
    # side comments at different indentation levels should not be aligned
    { { { { { ${msg} = "Hello World!"; print "My message: ${msg}\n"; } } #end level 4
            } # end level 3
        } # end level 2
    } # end level 1
----------

        'sil1' => <<'----------',
#############################################################
        # This will walk to the left because of bad -sil guess
      SKIP: {
#############################################################
        }

# This will walk to the right if it is the first line of a file.

     ov_method mycan( $package, '(""' ),       $package
  or ov_method mycan( $package, '(0+' ),       $package
  or ov_method mycan( $package, '(bool' ),     $package
  or ov_method mycan( $package, '(nomethod' ), $package;

----------

        'slashslash' => <<'----------',
$home = $ENV{HOME} // $ENV{LOGDIR} // ( getpwuid($<) )[7]
  // die "You're homeless!\n";
defined( $x // $y );
$version = 'v' . join '.', map ord, split //, $version->PV;
foreach ( split( //, $lets ) )  { }
foreach ( split( //, $input ) ) { }
'xyz' =~ //;
----------

        'smart' => <<'----------',
\&foo !~~ \&foo;
\&foo ~~ \&foo;
\&foo ~~ \&foo;
\&foo ~~ sub {};
sub {} ~~ \&foo;
\&foo ~~ \&bar;
\&bar ~~ \&foo;
1 ~~ sub{shift};
sub{shift} ~~ 1;
0 ~~ sub{shift};
sub{shift} ~~ 0;
1 ~~ sub{scalar @_};
sub{scalar @_} ~~ 1;
[] ~~ \&bar;
\&bar ~~ [];
{} ~~ \&bar;
\&bar ~~ {};
qr// ~~ \&bar;
\&bar ~~ qr//;
a_const ~~ "a constant";
"a constant" ~~ a_const;
a_const ~~ a_const;
a_const ~~ a_const;
a_const ~~ b_const;
b_const ~~ a_const;
{} ~~ {};
{} ~~ {};
{} ~~ {1 => 2};
{1 => 2} ~~ {};
{1 => 2} ~~ {1 => 2};
{1 => 2} ~~ {1 => 2};
{1 => 2} ~~ {1 => 3};
{1 => 3} ~~ {1 => 2};
{1 => 2} ~~ {2 => 3};
{2 => 3} ~~ {1 => 2};
\%main:: ~~ {map {$_ => 'x'} keys %main::};
{map {$_ => 'x'} keys %main::} ~~ \%main::;
\%hash ~~ \%tied_hash;
\%tied_hash ~~ \%hash;
\%tied_hash ~~ \%tied_hash;
\%tied_hash ~~ \%tied_hash;
\%:: ~~ [keys %main::];
[keys %main::] ~~ \%::;
\%:: ~~ [];
[] ~~ \%::;
{"" => 1} ~~ [undef];
[undef] ~~ {"" => 1};
{foo => 1} ~~ qr/^(fo[ox])$/;
qr/^(fo[ox])$/ ~~ {foo => 1};
+{0..100} ~~ qr/[13579]$/;
qr/[13579]$/ ~~ +{0..100};
+{foo => 1, bar => 2} ~~ "foo";
"foo" ~~ +{foo => 1, bar => 2};
+{foo => 1, bar => 2} ~~ "baz";
"baz" ~~ +{foo => 1, bar => 2};
[] ~~ [];
[] ~~ [];
[] ~~ [1];
[1] ~~ [];
[["foo"], ["bar"]] ~~ [qr/o/, qr/a/];
[qr/o/, qr/a/] ~~ [["foo"], ["bar"]];
["foo", "bar"] ~~ [qr/o/, qr/a/];
[qr/o/, qr/a/] ~~ ["foo", "bar"];
$deep1 ~~ $deep1;
$deep1 ~~ $deep1;
$deep1 ~~ $deep2;
$deep2 ~~ $deep1;
\@nums ~~ \@tied_nums;
\@tied_nums ~~ \@nums;
[qw(foo bar baz quux)] ~~ qr/x/;
qr/x/ ~~ [qw(foo bar baz quux)];
[qw(foo bar baz quux)] ~~ qr/y/;
qr/y/ ~~ [qw(foo bar baz quux)];
[qw(1foo 2bar)] ~~ 2;
2 ~~ [qw(1foo 2bar)];
[qw(1foo 2bar)] ~~ "2";
"2" ~~ [qw(1foo 2bar)];
2 ~~ 2;
2 ~~ 2;
2 ~~ 3;
3 ~~ 2;
2 ~~ "2";
"2" ~~ 2;
2 ~~ "2.0";
"2.0" ~~ 2;
2 ~~ "2bananas";
"2bananas" ~~ 2;
2_3 ~~ "2_3";
"2_3" ~~ 2_3;
qr/x/ ~~ "x";
"x" ~~ qr/x/;
qr/y/ ~~ "x";
"x" ~~ qr/y/;
12345 ~~ qr/3/;
qr/3/ ~~ 12345;
@nums ~~ 7;
7 ~~ @nums;
@nums ~~ \@nums;
\@nums ~~ @nums;
@nums ~~ \\@nums;
\\@nums ~~ @nums;
@nums ~~ [1..10];
[1..10] ~~ @nums;
@nums ~~ [0..9];
[0..9] ~~ @nums;
%hash ~~ "foo";
"foo" ~~ %hash;
%hash ~~ /bar/;
/bar/ ~~ %hash;
----------

        'space1' => <<'----------',
    # We usually want a space at '} (', for example:
    map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );

    # But not others:
    &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );

    # remove unwanted spaces after $ and -> here
    &{ $ _ -> [1] }( delete $ _ [$#_   ]{ $_   ->     [0] } );
----------

        'space2' => <<'----------',
# space before this opening paren
for$i(0..20){}

# retain any space between '-' and bare word
$myhash{USER-NAME}='steve';
----------

        'space3' => <<'----------',
# Treat newline as a whitespace. Otherwise, we might combine
# 'Send' and '-recipients' here 
my $msg = new Fax::Send
     -recipients => $to,
     -data => $data;
----------

        'space4' => <<'----------',
# first prototype line will cause space between 'redirect' and '(' to close
sub html::redirect($);        #<-- temporary prototype; 
use html;
print html::redirect ('http://www.glob.com.au/');
----------

        'space5' => <<'----------',
# first prototype line commented out; space after 'redirect' remains
#sub html::redirect($);        #<-- temporary prototype;
use html;
print html::redirect ('http://www.glob.com.au/');

----------

        'structure1' => <<'----------',
push@contents,$c->table({-width=>'100%'},$c->Tr($c->td({-align=>'left'},"The emboldened field names are mandatory, ","the remainder are optional",),$c->td({-align=>'right'},$c->a({-href=>'help.cgi',-target=>'_blank'},"What are the various fields?"))));
----------

        'style' => <<'----------',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
    my(@order) = ($hslabel_frame, $km_frame, $speed_frame[0],
		  $power_frame[0], $wind_frame, $percent_frame, $temp_frame,
		  @speed_frame[1..$#speed_frame],
		  @power_frame[1..$#power_frame],
		 );
    my(@col)   = (0, 1, 3, 4+$#speed_frame, 5+$#speed_frame+$#power_frame,
		  2, 6+$#speed_frame+$#power_frame,
		  4..3+$#speed_frame,
		  5+$#speed_frame..4+$#speed_frame+$#power_frame);
    $top->idletasks;
    my $width = 0;
    my(%gridslaves) = map {($_, 1)} $top_frame->gridSlaves;
    for(my $i = 0; $i <= $#order; $i++) {
	my $w = $order[$i];
	next unless Tk::Exists($w);
	my $col = $col[$i] || 0;
	$width += $w->reqwidth;
	if ($gridslaves{$w}) {
	    $w->gridForget;
	}
	if ($width <= $top->width) {
	    $w->grid(-row => 0,
		     -column => $col,
		     -sticky => 'nsew'); # XXX
	}
    }
}

----------

        'sub1' => <<'----------',
my::doit();
join::doit();
for::doit();
sub::doit();
package::doit();
__END__::doit();
__DATA__::doit();
package my;
sub doit{print"Hello My\n";}package join;
sub doit{print"Hello Join\n";}package for;
sub doit{print"Hello for\n";}package package;
sub doit{print"Hello package\n";}package sub;
sub doit{print"Hello sub\n";}package __END__;
sub doit{print"Hello __END__\n";}package __DATA__;
sub doit{print"Hello __DATA__\n";}
----------

        'sub2' => <<'----------',
my $selector;

# leading atrribute separator:
$a = 
  sub  
  : locked {
    print "Hello, World!\n";
  };
$a->();

# colon as both ?/: and attribute separator
$a = $selector
  ? sub  : locked {
    print "Hello, World!\n";
  }
  : sub : locked {
    print "GOODBYE!\n";
  };
$a->();
----------

        'switch1' => <<'----------',
sub classify_digit($digit)
  { switch($digit)
    { case 0 { return 'zero' } case [ 2, 4, 6, 8 ]{ return 'even' }
        case [ 1, 3, 4, 7, 9 ]{ return 'odd' } case /[A-F]/i { return 'hex' } }
  }
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'side_comments1.def' => {
            source => "side_comments1",
            params => "def",
            expect => <<'#1...........',
    # side comments at different indentation levels should not be aligned
    {
        {
            {
                {
                    { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
                }    #end level 4
            }    # end level 3
        }    # end level 2
    }    # end level 1
#1...........
        },

        'sil1.def' => {
            source => "sil1",
            params => "def",
            expect => <<'#2...........',
#############################################################
        # This will walk to the left because of bad -sil guess
      SKIP: {
#############################################################
        }

        # This will walk to the right if it is the first line of a file.

             ov_method mycan( $package, '(""' ),       $package
          or ov_method mycan( $package, '(0+' ),       $package
          or ov_method mycan( $package, '(bool' ),     $package
          or ov_method mycan( $package, '(nomethod' ), $package;

#2...........
        },

        'sil1.sil' => {
            source => "sil1",
            params => "sil",
            expect => <<'#3...........',
#############################################################
# This will walk to the left because of bad -sil guess
SKIP: {
#############################################################
}

# This will walk to the right if it is the first line of a file.

     ov_method mycan( $package, '(""' ),       $package
  or ov_method mycan( $package, '(0+' ),       $package
  or ov_method mycan( $package, '(bool' ),     $package
  or ov_method mycan( $package, '(nomethod' ), $package;

#3...........
        },

        'slashslash.def' => {
            source => "slashslash",
            params => "def",
            expect => <<'#4...........',
$home = $ENV{HOME} // $ENV{LOGDIR} // ( getpwuid($<) )[7]
  // die "You're homeless!\n";
defined( $x // $y );
$version = 'v' . join '.', map ord, split //, $version->PV;
foreach ( split( //, $lets ) )  { }
foreach ( split( //, $input ) ) { }
'xyz' =~ //;
#4...........
        },

        'smart.def' => {
            source => "smart",
            params => "def",
            expect => <<'#5...........',
\&foo !~~ \&foo;
\&foo ~~ \&foo;
\&foo ~~ \&foo;
\&foo ~~ sub { };
sub { } ~~ \&foo;
\&foo ~~ \&bar;
\&bar ~~ \&foo;
1 ~~ sub { shift };
sub { shift } ~~ 1;
0 ~~ sub { shift };
sub { shift } ~~ 0;
1 ~~ sub { scalar @_ };
sub { scalar @_ } ~~ 1;
[]           ~~ \&bar;
\&bar        ~~ [];
{}           ~~ \&bar;
\&bar        ~~ {};
qr//         ~~ \&bar;
\&bar        ~~ qr//;
a_const      ~~ "a constant";
"a constant" ~~ a_const;
a_const      ~~ a_const;
a_const      ~~ a_const;
a_const      ~~ b_const;
b_const      ~~ a_const;
{}           ~~ {};
{}           ~~ {};
{}           ~~ { 1 => 2 };
{ 1 => 2 } ~~ {};
{ 1 => 2 } ~~ { 1 => 2 };
{ 1 => 2 } ~~ { 1 => 2 };
{ 1 => 2 } ~~ { 1 => 3 };
{ 1 => 3 } ~~ { 1 => 2 };
{ 1 => 2 } ~~ { 2 => 3 };
{ 2 => 3 } ~~ { 1 => 2 };
\%main:: ~~ { map { $_ => 'x' } keys %main:: };
{
    map { $_ => 'x' } keys %main::
}
~~ \%main::;
\%hash           ~~ \%tied_hash;
\%tied_hash      ~~ \%hash;
\%tied_hash      ~~ \%tied_hash;
\%tied_hash      ~~ \%tied_hash;
\%::             ~~ [ keys %main:: ];
[ keys %main:: ] ~~ \%::;
\%::             ~~ [];
[]               ~~ \%::;
{ "" => 1 } ~~ [undef];
[undef] ~~ { "" => 1 };
{ foo => 1 } ~~ qr/^(fo[ox])$/;
qr/^(fo[ox])$/ ~~ { foo => 1 };
+{ 0 .. 100 }  ~~ qr/[13579]$/;
qr/[13579]$/   ~~ +{ 0 .. 100 };
+{ foo => 1, bar => 2 } ~~ "foo";
"foo" ~~ +{ foo => 1, bar => 2 };
+{ foo => 1, bar => 2 } ~~ "baz";
"baz" ~~ +{ foo => 1, bar => 2 };
[]    ~~ [];
[]    ~~ [];
[]    ~~ [1];
[1]   ~~ [];
[ ["foo"], ["bar"] ] ~~ [ qr/o/, qr/a/ ];
[ qr/o/, qr/a/ ] ~~ [ ["foo"], ["bar"] ];
[ "foo", "bar" ] ~~ [ qr/o/, qr/a/ ];
[ qr/o/, qr/a/ ] ~~ [ "foo", "bar" ];
$deep1                 ~~ $deep1;
$deep1                 ~~ $deep1;
$deep1                 ~~ $deep2;
$deep2                 ~~ $deep1;
\@nums                 ~~ \@tied_nums;
\@tied_nums            ~~ \@nums;
[qw(foo bar baz quux)] ~~ qr/x/;
qr/x/                  ~~ [qw(foo bar baz quux)];
[qw(foo bar baz quux)] ~~ qr/y/;
qr/y/                  ~~ [qw(foo bar baz quux)];
[qw(1foo 2bar)]        ~~ 2;
2                      ~~ [qw(1foo 2bar)];
[qw(1foo 2bar)]        ~~ "2";
"2"                    ~~ [qw(1foo 2bar)];
2                      ~~ 2;
2                      ~~ 2;
2                      ~~ 3;
3                      ~~ 2;
2                      ~~ "2";
"2"                    ~~ 2;
2                      ~~ "2.0";
"2.0"                  ~~ 2;
2                      ~~ "2bananas";
"2bananas"             ~~ 2;
2_3                    ~~ "2_3";
"2_3"                  ~~ 2_3;
qr/x/                  ~~ "x";
"x"                    ~~ qr/x/;
qr/y/                  ~~ "x";
"x"                    ~~ qr/y/;
12345                  ~~ qr/3/;
qr/3/                  ~~ 12345;
@nums                  ~~ 7;
7                      ~~ @nums;
@nums                  ~~ \@nums;
\@nums                 ~~ @nums;
@nums                  ~~ \\@nums;
\\@nums                ~~ @nums;
@nums                  ~~ [ 1 .. 10 ];
[ 1 .. 10 ]            ~~ @nums;
@nums                  ~~ [ 0 .. 9 ];
[ 0 .. 9 ]             ~~ @nums;
%hash                  ~~ "foo";
"foo"                  ~~ %hash;
%hash                  ~~ /bar/;
/bar/                  ~~ %hash;
#5...........
        },

        'space1.def' => {
            source => "space1",
            params => "def",
            expect => <<'#6...........',
    # We usually want a space at '} (', for example:
    map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );

    # But not others:
    &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );

    # remove unwanted spaces after $ and -> here
    &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );
#6...........
        },

        'space2.def' => {
            source => "space2",
            params => "def",
            expect => <<'#7...........',
# space before this opening paren
for $i ( 0 .. 20 ) { }

# retain any space between '-' and bare word
$myhash{ USER-NAME } = 'steve';
#7...........
        },

        'space3.def' => {
            source => "space3",
            params => "def",
            expect => <<'#8...........',
# Treat newline as a whitespace. Otherwise, we might combine
# 'Send' and '-recipients' here
my $msg = new Fax::Send
  -recipients => $to,
  -data       => $data;
#8...........
        },

        'space4.def' => {
            source => "space4",
            params => "def",
            expect => <<'#9...........',
# first prototype line will cause space between 'redirect' and '(' to close
sub html::redirect($);    #<-- temporary prototype;
use html;
print html::redirect('http://www.glob.com.au/');
#9...........
        },

        'space5.def' => {
            source => "space5",
            params => "def",
            expect => <<'#10...........',
# first prototype line commented out; space after 'redirect' remains
#sub html::redirect($);        #<-- temporary prototype;
use html;
print html::redirect ('http://www.glob.com.au/');

#10...........
        },

        'structure1.def' => {
            source => "structure1",
            params => "def",
            expect => <<'#11...........',
push @contents,
  $c->table(
    { -width => '100%' },
    $c->Tr(
        $c->td(
            { -align => 'left' },
            "The emboldened field names are mandatory, ",
            "the remainder are optional",
        ),
        $c->td(
            { -align => 'right' },
            $c->a(
                { -href => 'help.cgi', -target => '_blank' },
                "What are the various fields?"
            )
        )
    )
  );
#11...........
        },

        'style.def' => {
            source => "style",
            params => "def",
            expect => <<'#12...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
    my (@order) = (
        $hslabel_frame,
        $km_frame,
        $speed_frame[0],
        $power_frame[0],
        $wind_frame,
        $percent_frame,
        $temp_frame,
        @speed_frame[ 1 .. $#speed_frame ],
        @power_frame[ 1 .. $#power_frame ],
    );
    my (@col) = (
        0,
        1,
        3,
        4 + $#speed_frame,
        5 + $#speed_frame + $#power_frame,
        2,
        6 + $#speed_frame + $#power_frame,
        4 .. 3 + $#speed_frame,
        5 + $#speed_frame .. 4 + $#speed_frame + $#power_frame
    );
    $top->idletasks;
    my $width = 0;
    my (%gridslaves) = map { ( $_, 1 ) } $top_frame->gridSlaves;
    for ( my $i = 0 ; $i <= $#order ; $i++ ) {
        my $w = $order[$i];
        next unless Tk::Exists($w);
        my $col = $col[$i] || 0;
        $width += $w->reqwidth;
        if ( $gridslaves{$w} ) {
            $w->gridForget;
        }
        if ( $width <= $top->width ) {
            $w->grid(
                -row    => 0,
                -column => $col,
                -sticky => 'nsew'
            );    # XXX
        }
    }
}

#12...........
        },

        'style.style1' => {
            source => "style",
            params => "style1",
            expect => <<'#13...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
  my (@order) = (
    $hslabel_frame, $km_frame, $speed_frame[0],
    $power_frame[0], $wind_frame, $percent_frame, $temp_frame,
    @speed_frame[1 .. $#speed_frame],
    @power_frame[1 .. $#power_frame],
  );
  my (@col) = (
    0, 1, 3,
    4 + $#speed_frame,
    5 + $#speed_frame + $#power_frame,
    2,
    6 + $#speed_frame + $#power_frame,
    4 .. 3 + $#speed_frame,
    5 + $#speed_frame .. 4 + $#speed_frame + $#power_frame
  );
  $top->idletasks;
  my $width = 0;
  my (%gridslaves) = map { ($_, 1) } $top_frame->gridSlaves;
  for (my $i = 0; $i <= $#order; $i++) {
    my $w = $order[$i];
    next unless Tk::Exists($w);
    my $col = $col[$i] || 0;
    $width += $w->reqwidth;
    if ($gridslaves{$w}) {
      $w->gridForget;
    }
    if ($width <= $top->width) {
      $w->grid(
        -row    => 0,
        -column => $col,
        -sticky => 'nsew'
      );    # XXX
    }
  }
}

#13...........
        },

        'style.style2' => {
            source => "style",
            params => "style2",
            expect => <<'#14...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
    my (@order) = (
        $hslabel_frame,  $km_frame,
        $speed_frame[0], $power_frame[0],
        $wind_frame,     $percent_frame,
        $temp_frame,     @speed_frame[1..$#speed_frame],
        @power_frame[1..$#power_frame],
    );
    my (@col) = (
        0,
        1,
        3,
        4 + $#speed_frame,
        5 + $#speed_frame + $#power_frame,
        2,
        6 + $#speed_frame + $#power_frame,
        4..3 + $#speed_frame,
        5 + $#speed_frame..4 + $#speed_frame + $#power_frame
    );
    $top->idletasks;
    my $width = 0;
    my (%gridslaves) = map { ($_, 1) } $top_frame->gridSlaves;
    for (my $i = 0; $i <= $#order; $i++) {
        my $w = $order[$i];
        next unless Tk::Exists($w);
        my $col = $col[$i] || 0;
        $width += $w->reqwidth;
        if ($gridslaves{$w}) {
            $w->gridForget;
        }
        if ($width <= $top->width) {
            $w->grid(
                -row    => 0,
                -column => $col,
                -sticky => 'nsew'
            );    # XXX
        }
    }
}

#14...........
        },

        'style.style3' => {
            source => "style",
            params => "style3",
            expect => <<'#15...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
    my (@order) = (
                    $hslabel_frame, $km_frame, $speed_frame[0], $power_frame[0], $wind_frame, $percent_frame, $temp_frame,
                    @speed_frame[ 1 .. $#speed_frame ],
                    @power_frame[ 1 .. $#power_frame ],
                  );
    my (@col) = (
                  0, 1, 3,
                  4 + $#speed_frame,
                  5 + $#speed_frame + $#power_frame,
                  2,
                  6 + $#speed_frame + $#power_frame,
                  4 .. 3 + $#speed_frame,
                  5 + $#speed_frame .. 4 + $#speed_frame + $#power_frame
                );
    $top->idletasks;
    my $width = 0;
    my (%gridslaves) = map { ( $_, 1 ) } $top_frame->gridSlaves;
    for ( my $i = 0 ; $i <= $#order ; $i++ ) {
        my $w = $order[$i];
        next unless Tk::Exists($w);
        my $col = $col[$i] || 0;
        $width += $w->reqwidth;
        if ( $gridslaves{$w} ) {
            $w->gridForget;
        }
        if ( $width <= $top->width ) {
            $w->grid(
                      -row    => 0,
                      -column => $col,
                      -sticky => 'nsew'
                    );    # XXX
        }
    }
} ## end sub arrange_topframe

#15...........
        },

        'style.style4' => {
            source => "style",
            params => "style4",
            expect => <<'#16...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe {
    my (@order) = (
        $hslabel_frame,  $km_frame,
        $speed_frame[0], $power_frame[0],
        $wind_frame,     $percent_frame,
        $temp_frame,     @speed_frame[1 .. $#speed_frame],
        @power_frame[1 .. $#power_frame],
    );
    my (@col) = (
        0,
        1,
        3,
        4 + $#speed_frame,
        5 + $#speed_frame + $#power_frame,
        2,
        6 + $#speed_frame + $#power_frame,
        4 .. 3 + $#speed_frame,
        5 + $#speed_frame .. 4 + $#speed_frame + $#power_frame
    );
    $top->idletasks;
    my $width = 0;
    my (%gridslaves) = map { ($_, 1) } $top_frame->gridSlaves;
    for (my $i = 0 ; $i <= $#order ; $i++) {
        my $w = $order[$i];
        next unless Tk::Exists($w);
        my $col = $col[$i] || 0;
        $width += $w->reqwidth;
        if ($gridslaves{$w}) {
            $w->gridForget;
        }
        if ($width <= $top->width) {
            $w->grid(
                -row    => 0,
                -column => $col,
                -sticky => 'nsew'
            );    # XXX
        }
    }
}

#16...........
        },

        'style.style5' => {
            source => "style",
            params => "style5",
            expect => <<'#17...........',
# This test snippet is from package bbbike v3.214 by Slaven Rezic; GPL 2.0 licence
sub arrange_topframe
{
    my (@order) = (
	$hslabel_frame,  $km_frame,
	$speed_frame[0], $power_frame[0],
	$wind_frame,     $percent_frame,
	$temp_frame,     @speed_frame[1 .. $#speed_frame],
	@power_frame[1 .. $#power_frame],
	);
    my (@col) = (
	0,
	1,
	3,
	4 + $#speed_frame,
	5 + $#speed_frame + $#power_frame,
	2,
	6 + $#speed_frame + $#power_frame,
	4 .. 3 + $#speed_frame,
	5 + $#speed_frame .. 4 + $#speed_frame + $#power_frame
	);
    $top->idletasks;
    my $width = 0;
    my (%gridslaves) = map { ($_, 1) } $top_frame->gridSlaves;
    for (my $i = 0; $i <= $#order; $i++)
    {
	my $w = $order[$i];
	next unless Tk::Exists($w);
	my $col = $col[$i] || 0;
	$width += $w->reqwidth;
	if ($gridslaves{$w})
	{
	    $w->gridForget;
	}
	if ($width <= $top->width)
	{
	    $w->grid(
		-row    => 0,
		-column => $col,
		-sticky => 'nsew'
		);  # XXX
	}
    }
}

#17...........
        },

        'sub1.def' => {
            source => "sub1",
            params => "def",
            expect => <<'#18...........',
my::doit();
join::doit();
for::doit();
sub::doit();
package::doit();
__END__::doit();
__DATA__::doit();

package my;
sub doit { print "Hello My\n"; }

package join;
sub doit { print "Hello Join\n"; }

package for;
sub doit { print "Hello for\n"; }

package package;
sub doit { print "Hello package\n"; }

package sub;
sub doit { print "Hello sub\n"; }

package __END__;
sub doit { print "Hello __END__\n"; }

package __DATA__;
sub doit { print "Hello __DATA__\n"; }
#18...........
        },

        'sub2.def' => {
            source => "sub2",
            params => "def",
            expect => <<'#19...........',
my $selector;

# leading atrribute separator:
$a = sub
  : locked {
    print "Hello, World!\n";
  };
$a->();

# colon as both ?/: and attribute separator
$a = $selector
  ? sub : locked {
    print "Hello, World!\n";
  }
  : sub : locked {
    print "GOODBYE!\n";
  };
$a->();
#19...........
        },

        'switch1.def' => {
            source => "switch1",
            params => "def",
            expect => <<'#20...........',
sub classify_digit($digit) {
    switch ($digit) {
        case 0 { return 'zero' }
        case [ 2, 4, 6, 8 ]{ return 'even' }
        case [ 1, 3, 4, 7, 9 ]{ return 'odd' }
        case /[A-F]/i { return 'hex' }
    }
}
#20...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

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
        if ($err) {
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$errorfile_string );
        }
    }
    else {
        ok( $output, $expect );
    }
}
