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
        'def'    => "",
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
        'tso' => "-tso",
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

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

        'syntax1' => <<'----------',
# Caused trouble:
print $x **2;
----------

        'syntax2' => <<'----------',
# ? was taken as pattern
my $case_flag = File::Spec->case_tolerant ? '(?i)' : '';
----------

        'ternary1' => <<'----------',
my $flags =
  ( $_ & 1 ) ? ( $_ & 4 ) ? $THRf_DEAD : $THRf_ZOMBIE :
  ( $_ & 4 ) ? $THRf_R_DETACHED : $THRf_R_JOINABLE;
----------

        'ternary2' => <<'----------',
my $a=($b) ? ($c) ? ($d) ? $d1
                         : $d2
                  : ($e) ? $e1
                         : $e2
           : ($f) ? ($g) ? $g1
                         : $g2
                  : ($h) ? $h1
                         : $h2;
----------

        'tick1' => <<'----------',
sub a'this { $p'u'a = "mooo\n"; print $p::u::a; }
a::this();       # print "mooo"
print $p'u'a;    # print "mooo"
sub a::that {
    $p't'u = "wwoo\n";
    return sub { print $p't'u}
}
$a'that = a'that();
$a'that->();     # print "wwoo"
$a'that  = a'that();
$p::t::u = "booo\n";
$a'that->();     # print "booo"
----------

        'trim_quote' => <<'----------',
# space after quote will get trimmed
    push @m, '
all :: pure_all manifypods
	' . $self->{NOECHO} . '$(NOOP)
' 
      unless $self->{SKIPHASH}{'all'};
----------

        'tso1' => <<'----------',
print 0+ '42 EUR';    # 42
----------

        'tutor' => <<'----------',
#!/usr/bin/perl
$y=shift||5;for $i(1..10){$l[$i]="T";$w[$i]=999999;}while(1){print"Name:";$u=<STDIN>;$t=50;$a=time;for(0..9){$x="";for(1..$y){$x.=chr(int(rand(126-33)+33));}while($z ne $x){print"\r\n$x\r\n";$z=<STDIN>;chomp($z);$t-=5;}}$b=time;$t-=($b-$a)*2;$t=0-$t;$z=1;@q=@l;@p=@w;print "You scored $t points\r\nTopTen\r\n";for $i(1..10){if ($t<$p[$z]){$l[$i]=$u;chomp($l[$i]);$w[$i]=$t;$t=1000000}else{$l[$i]=$q[$z];$w[$i]=$p[$z];$z++;}print $l[$i],"\t",$w[$i],"\r\n";}}
----------

        'undoci1' => <<'----------',
        $rinfo{deleteStyle} = [
            -fill      => 'red',
              -stipple => '@' . Tk->findINC('demos/images/grey.25'),
        ];
----------

        'use1' => <<'----------',
# previously this caused an incorrect error message after '2.42'
use lib "$Common::global::gInstallRoot/lib";
use CGI 2.42 qw(fatalsToBrowser);
use RRDs 1.000101;

# the 0666 must expect an operator
use constant MODE => do { 0666 & ( 0777 & ~umask ) };

use IO::File ();
----------

        'use2' => <<'----------',
# Keep the space before the '()' here:
use Foo::Bar ();
use Foo::Bar ();
use Foo::Bar 1.0 ();
use Foo::Bar qw(baz);
use Foo::Bar 1.0 qw(baz);
----------

        'version1' => <<'----------',
# VERSION statement unbroken, no semicolon added; 
our $VERSION = do { my @r = ( q$Revision: 2.2 $ =~ /\d+/g ); sprintf "%d." . "%02d" x $#r, @r }
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'style.style2' => {
            source => "style",
            params => "style2",
            expect => <<'#1...........',
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

#1...........
        },

        'style.style3' => {
            source => "style",
            params => "style3",
            expect => <<'#2...........',
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

#2...........
        },

        'style.style4' => {
            source => "style",
            params => "style4",
            expect => <<'#3...........',
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

#3...........
        },

        'style.style5' => {
            source => "style",
            params => "style5",
            expect => <<'#4...........',
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

#4...........
        },

        'sub1.def' => {
            source => "sub1",
            params => "def",
            expect => <<'#5...........',
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
#5...........
        },

        'sub2.def' => {
            source => "sub2",
            params => "def",
            expect => <<'#6...........',
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
#6...........
        },

        'switch1.def' => {
            source => "switch1",
            params => "def",
            expect => <<'#7...........',
sub classify_digit($digit) {
    switch ($digit) {
        case 0 { return 'zero' }
        case [ 2, 4, 6, 8 ]{ return 'even' }
        case [ 1, 3, 4, 7, 9 ]{ return 'odd' }
        case /[A-F]/i { return 'hex' }
    }
}
#7...........
        },

        'syntax1.def' => {
            source => "syntax1",
            params => "def",
            expect => <<'#8...........',
# Caused trouble:
print $x **2;
#8...........
        },

        'syntax2.def' => {
            source => "syntax2",
            params => "def",
            expect => <<'#9...........',
# ? was taken as pattern
my $case_flag = File::Spec->case_tolerant ? '(?i)' : '';
#9...........
        },

        'ternary1.def' => {
            source => "ternary1",
            params => "def",
            expect => <<'#10...........',
my $flags =
    ( $_ & 1 )
  ? ( $_ & 4 )
      ? $THRf_DEAD
      : $THRf_ZOMBIE
  : ( $_ & 4 ) ? $THRf_R_DETACHED
  :              $THRf_R_JOINABLE;
#10...........
        },

        'ternary2.def' => {
            source => "ternary2",
            params => "def",
            expect => <<'#11...........',
my $a =
    ($b)
  ? ($c)
      ? ($d)
          ? $d1
          : $d2
      : ($e) ? $e1
    : $e2
  : ($f) ? ($g)
      ? $g1
      : $g2
  : ($h) ? $h1
  :        $h2;
#11...........
        },

        'tick1.def' => {
            source => "tick1",
            params => "def",
            expect => <<'#12...........',
sub a'this { $p'u'a = "mooo\n"; print $p::u::a; }
a::this();       # print "mooo"
print $p'u'a;    # print "mooo"

sub a::that {
    $p't'u = "wwoo\n";
    return sub { print $p't'u}
}
$a'that = a'that();
$a'that->();     # print "wwoo"
$a'that  = a'that();
$p::t::u = "booo\n";
$a'that->();     # print "booo"
#12...........
        },

        'trim_quote.def' => {
            source => "trim_quote",
            params => "def",
            expect => <<'#13...........',
    # space after quote will get trimmed
    push @m, '
all :: pure_all manifypods
	' . $self->{NOECHO} . '$(NOOP)
'
      unless $self->{SKIPHASH}{'all'};
#13...........
        },

        'tso1.def' => {
            source => "tso1",
            params => "def",
            expect => <<'#14...........',
print 0 + '42 EUR';    # 42
#14...........
        },

        'tso1.tso' => {
            source => "tso1",
            params => "tso",
            expect => <<'#15...........',
print 0+ '42 EUR';    # 42
#15...........
        },

        'tutor.def' => {
            source => "tutor",
            params => "def",
            expect => <<'#16...........',
#!/usr/bin/perl
$y = shift || 5;
for $i ( 1 .. 10 ) { $l[$i] = "T"; $w[$i] = 999999; }
while (1) {
    print "Name:";
    $u = <STDIN>;
    $t = 50;
    $a = time;
    for ( 0 .. 9 ) {
        $x = "";
        for ( 1 .. $y ) { $x .= chr( int( rand( 126 - 33 ) + 33 ) ); }
        while ( $z ne $x ) {
            print "\r\n$x\r\n";
            $z = <STDIN>;
            chomp($z);
            $t -= 5;
        }
    }
    $b = time;
    $t -= ( $b - $a ) * 2;
    $t = 0 - $t;
    $z = 1;
    @q = @l;
    @p = @w;
    print "You scored $t points\r\nTopTen\r\n";

    for $i ( 1 .. 10 ) {
        if ( $t < $p[$z] ) {
            $l[$i] = $u;
            chomp( $l[$i] );
            $w[$i] = $t;
            $t = 1000000;
        }
        else { $l[$i] = $q[$z]; $w[$i] = $p[$z]; $z++; }
        print $l[$i], "\t", $w[$i], "\r\n";
    }
}
#16...........
        },

        'undoci1.def' => {
            source => "undoci1",
            params => "def",
            expect => <<'#17...........',
        $rinfo{deleteStyle} = [
            -fill    => 'red',
            -stipple => '@' . Tk->findINC('demos/images/grey.25'),
        ];
#17...........
        },

        'use1.def' => {
            source => "use1",
            params => "def",
            expect => <<'#18...........',
# previously this caused an incorrect error message after '2.42'
use lib "$Common::global::gInstallRoot/lib";
use CGI 2.42 qw(fatalsToBrowser);
use RRDs 1.000101;

# the 0666 must expect an operator
use constant MODE => do { 0666 & ( 0777 & ~umask ) };

use IO::File ();
#18...........
        },

        'use2.def' => {
            source => "use2",
            params => "def",
            expect => <<'#19...........',
# Keep the space before the '()' here:
use Foo::Bar ();
use Foo::Bar ();
use Foo::Bar 1.0 ();
use Foo::Bar qw(baz);
use Foo::Bar 1.0 qw(baz);
#19...........
        },

        'version1.def' => {
            source => "version1",
            params => "def",
            expect => <<'#20...........',
# VERSION statement unbroken, no semicolon added;
our $VERSION = do { my @r = ( q$Revision: 2.2 $ =~ /\d+/g ); sprintf "%d." . "%02d" x $#r, @r }
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
