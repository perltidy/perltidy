# **This script was automatically generated**
# Created with: ./make_t.pl
# Thu Apr  5 07:31:23 2018

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
        'def'      => "",
        'rt125012' => <<'----------',
-mangle
-dac
----------
        'scl'    => "-scl=12",
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
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

        'rt125012' => <<'----------',
++$_ for
#one space before eol:
values %_;
system
#one space before eol:
qq{};
----------

        'rt94338' => <<'----------',
# for-loop in a parenthesized block-map triggered an error message
map( { foreach my $item ( '0', '1' ) { print $item} } qw(a b c) );
----------

        'rt96101' => <<'----------',
# Example for rt.cpan.org #96101; Perltidy not properly formatting subroutine
# references inside subroutine execution.

# closing brace of second sub should get outdented here
sub startup {
    my $self = shift;
    $self->plugin(
        'authentication' => {
            'autoload_user' => 1,
            'session_key'   => rand(),
            'load_user'     => sub {
                return HaloVP::Users->load(@_);
            },
            'validate_user' => sub {
                return HaloVP::Users->login(@_);
            }
        }
    );
}

----------

        'scl' => <<'----------',
    # try -scl=12 to see '$returns' joined with the previous line
    $format = "format STDOUT =\n" . &format_line('Function:       @') . '$name' . "\n" . &format_line('Arguments:      @') . '$args' . "\n" . &format_line('Returns:        @') . '$returns' . "\n" . &format_line('             ~~ ^') . '$desc' . "\n.\n";
----------

        'semicolon2' => <<'----------',
	# will not add semicolon for this block type
        $highest = List::Util::reduce { Sort::Versions::versioncmp( $a, $b ) > 0 ? $a : $b }
----------

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
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'rt125012.def' => {
            source => "rt125012",
            params => "def",
            expect => <<'#1...........',
++$_ for

  #one space before eol:
  values %_;
system

  #one space before eol:
  qq{};
#1...........
        },

        'rt125012.rt125012' => {
            source => "rt125012",
            params => "rt125012",
            expect => <<'#2...........',
++$_ for values%_;
system qq{};
#2...........
        },

        'rt94338.def' => {
            source => "rt94338",
            params => "def",
            expect => <<'#3...........',
# for-loop in a parenthesized block-map triggered an error message
map( {
        foreach my $item ( '0', '1' ) {
            print $item;
        }
} qw(a b c) );
#3...........
        },

        'rt96101.def' => {
            source => "rt96101",
            params => "def",
            expect => <<'#4...........',
# Example for rt.cpan.org #96101; Perltidy not properly formatting subroutine
# references inside subroutine execution.

# closing brace of second sub should get outdented here
sub startup {
    my $self = shift;
    $self->plugin(
        'authentication' => {
            'autoload_user' => 1,
            'session_key'   => rand(),
            'load_user'     => sub {
                return HaloVP::Users->load(@_);
            },
            'validate_user' => sub {
                return HaloVP::Users->login(@_);
            }
        }
    );
}

#4...........
        },

        'scl.def' => {
            source => "scl",
            params => "def",
            expect => <<'#5...........',
    # try -scl=12 to see '$returns' joined with the previous line
    $format =
        "format STDOUT =\n"
      . &format_line('Function:       @') . '$name' . "\n"
      . &format_line('Arguments:      @') . '$args' . "\n"
      . &format_line('Returns:        @')
      . '$returns' . "\n"
      . &format_line('             ~~ ^') . '$desc' . "\n.\n";
#5...........
        },

        'scl.scl' => {
            source => "scl",
            params => "scl",
            expect => <<'#6...........',
    # try -scl=12 to see '$returns' joined with the previous line
    $format =
        "format STDOUT =\n"
      . &format_line('Function:       @') . '$name' . "\n"
      . &format_line('Arguments:      @') . '$args' . "\n"
      . &format_line('Returns:        @') . '$returns' . "\n"
      . &format_line('             ~~ ^') . '$desc' . "\n.\n";
#6...........
        },

        'semicolon2.def' => {
            source => "semicolon2",
            params => "def",
            expect => <<'#7...........',
        # will not add semicolon for this block type
        $highest = List::Util::reduce {
            Sort::Versions::versioncmp( $a, $b ) > 0 ? $a : $b
        }
#7...........
        },

        'side_comments1.def' => {
            source => "side_comments1",
            params => "def",
            expect => <<'#8...........',
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
#8...........
        },

        'sil1.def' => {
            source => "sil1",
            params => "def",
            expect => <<'#9...........',
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

#9...........
        },

        'sil1.sil' => {
            source => "sil1",
            params => "sil",
            expect => <<'#10...........',
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

#10...........
        },

        'slashslash.def' => {
            source => "slashslash",
            params => "def",
            expect => <<'#11...........',
$home = $ENV{HOME} // $ENV{LOGDIR} // ( getpwuid($<) )[7]
  // die "You're homeless!\n";
defined( $x // $y );
$version = 'v' . join '.', map ord, split //, $version->PV;
foreach ( split( //, $lets ) )  { }
foreach ( split( //, $input ) ) { }
'xyz' =~ //;
#11...........
        },

        'smart.def' => {
            source => "smart",
            params => "def",
            expect => <<'#12...........',
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
#12...........
        },

        'space1.def' => {
            source => "space1",
            params => "def",
            expect => <<'#13...........',
    # We usually want a space at '} (', for example:
    map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );

    # But not others:
    &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );

    # remove unwanted spaces after $ and -> here
    &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );
#13...........
        },

        'space2.def' => {
            source => "space2",
            params => "def",
            expect => <<'#14...........',
# space before this opening paren
for $i ( 0 .. 20 ) { }

# retain any space between '-' and bare word
$myhash{ USER-NAME } = 'steve';
#14...........
        },

        'space3.def' => {
            source => "space3",
            params => "def",
            expect => <<'#15...........',
# Treat newline as a whitespace. Otherwise, we might combine
# 'Send' and '-recipients' here
my $msg = new Fax::Send
  -recipients => $to,
  -data       => $data;
#15...........
        },

        'space4.def' => {
            source => "space4",
            params => "def",
            expect => <<'#16...........',
# first prototype line will cause space between 'redirect' and '(' to close
sub html::redirect($);    #<-- temporary prototype;
use html;
print html::redirect('http://www.glob.com.au/');
#16...........
        },

        'space5.def' => {
            source => "space5",
            params => "def",
            expect => <<'#17...........',
# first prototype line commented out; space after 'redirect' remains
#sub html::redirect($);        #<-- temporary prototype;
use html;
print html::redirect ('http://www.glob.com.au/');

#17...........
        },

        'structure1.def' => {
            source => "structure1",
            params => "def",
            expect => <<'#18...........',
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
#18...........
        },

        'style.def' => {
            source => "style",
            params => "def",
            expect => <<'#19...........',
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

#19...........
        },

        'style.style1' => {
            source => "style",
            params => "style1",
            expect => <<'#20...........',
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
