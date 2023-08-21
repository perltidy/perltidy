# Created with: ./make_t.pl

# Contents:
#1 sub1.def
#2 sub2.def
#3 switch1.def
#4 syntax1.def
#5 syntax2.def
#6 ternary1.def
#7 ternary2.def
#8 tick1.def
#9 trim_quote.def
#10 tso1.def
#11 tso1.tso
#12 tutor.def
#13 undoci1.def
#14 use1.def
#15 use2.def
#16 version1.def
#17 version2.def
#18 vert.def
#19 vmll.def
#20 vmll.vmll

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
        'def'  => "",
        'tso'  => "-tso",
        'vmll' => <<'----------',
-vmll
-bbt=2
-bt=2
-pt=2
-sbt=2
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

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
sub classify_digit ($digit)
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

        'version2' => <<'----------',
# On one line so MakeMaker will see it.
require Exporter; our $VERSION = $Exporter::VERSION;
----------

        'vert' => <<'----------',
# if $w->vert is tokenized as type 'U' then the ? will start a quote
# and an error will occur.
sub vert {
}
sub Restore {
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}
----------

        'vmll' => <<'----------',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {is_deeply(\@init_metas_called, [1]) || diag(Dumper(\@init_metas_called))}

    This has the comma on the next line
    exception {Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)},
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'sub1.def' => {
            source => "sub1",
            params => "def",
            expect => <<'#1...........',
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
#1...........
        },

        'sub2.def' => {
            source => "sub2",
            params => "def",
            expect => <<'#2...........',
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
#2...........
        },

        'switch1.def' => {
            source => "switch1",
            params => "def",
            expect => <<'#3...........',
sub classify_digit ($digit) {
    switch ($digit) {
        case 0 { return 'zero' }
        case [ 2, 4, 6, 8 ]{ return 'even' }
        case [ 1, 3, 4, 7, 9 ]{ return 'odd' }
        case /[A-F]/i { return 'hex' }
    }
}
#3...........
        },

        'syntax1.def' => {
            source => "syntax1",
            params => "def",
            expect => <<'#4...........',
# Caused trouble:
print $x **2;
#4...........
        },

        'syntax2.def' => {
            source => "syntax2",
            params => "def",
            expect => <<'#5...........',
# ? was taken as pattern
my $case_flag = File::Spec->case_tolerant ? '(?i)' : '';
#5...........
        },

        'ternary1.def' => {
            source => "ternary1",
            params => "def",
            expect => <<'#6...........',
my $flags =
    ( $_ & 1 )
  ? ( $_ & 4 )
      ? $THRf_DEAD
      : $THRf_ZOMBIE
  : ( $_ & 4 ) ? $THRf_R_DETACHED
  :              $THRf_R_JOINABLE;
#6...........
        },

        'ternary2.def' => {
            source => "ternary2",
            params => "def",
            expect => <<'#7...........',
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
#7...........
        },

        'tick1.def' => {
            source => "tick1",
            params => "def",
            expect => <<'#8...........',
sub a'this { $p'u'a = "mooo\n"; print $p::u::a; }
a::this();       # print "mooo"
print $p'u'a;    # print "mooo"

sub a::that {
    $p't'u = "wwoo\n";
    return sub { print $p't'u }
}
$a'that = a'that();
$a'that->();     # print "wwoo"
$a'that  = a'that();
$p::t::u = "booo\n";
$a'that->();     # print "booo"
#8...........
        },

        'trim_quote.def' => {
            source => "trim_quote",
            params => "def",
            expect => <<'#9...........',
    # space after quote will get trimmed
    push @m, '
all :: pure_all manifypods
	' . $self->{NOECHO} . '$(NOOP)
'
      unless $self->{SKIPHASH}{'all'};
#9...........
        },

        'tso1.def' => {
            source => "tso1",
            params => "def",
            expect => <<'#10...........',
print 0 + '42 EUR';    # 42
#10...........
        },

        'tso1.tso' => {
            source => "tso1",
            params => "tso",
            expect => <<'#11...........',
print 0+ '42 EUR';    # 42
#11...........
        },

        'tutor.def' => {
            source => "tutor",
            params => "def",
            expect => <<'#12...........',
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
#12...........
        },

        'undoci1.def' => {
            source => "undoci1",
            params => "def",
            expect => <<'#13...........',
        $rinfo{deleteStyle} = [
            -fill    => 'red',
            -stipple => '@' . Tk->findINC('demos/images/grey.25'),
        ];
#13...........
        },

        'use1.def' => {
            source => "use1",
            params => "def",
            expect => <<'#14...........',
# previously this caused an incorrect error message after '2.42'
use lib "$Common::global::gInstallRoot/lib";
use CGI 2.42 qw(fatalsToBrowser);
use RRDs 1.000101;

# the 0666 must expect an operator
use constant MODE => do { 0666 & ( 0777 & ~umask ) };

use IO::File ();
#14...........
        },

        'use2.def' => {
            source => "use2",
            params => "def",
            expect => <<'#15...........',
# Keep the space before the '()' here:
use Foo::Bar     ();
use Foo::Bar     ();
use Foo::Bar 1.0 ();
use Foo::Bar     qw(baz);
use Foo::Bar 1.0 qw(baz);
#15...........
        },

        'version1.def' => {
            source => "version1",
            params => "def",
            expect => <<'#16...........',
# VERSION statement unbroken, no semicolon added;
our $VERSION = do { my @r = ( q$Revision: 2.2 $ =~ /\d+/g ); sprintf "%d." . "%02d" x $#r, @r }
#16...........
        },

        'version2.def' => {
            source => "version2",
            params => "def",
            expect => <<'#17...........',
# On one line so MakeMaker will see it.
require Exporter; our $VERSION = $Exporter::VERSION;
#17...........
        },

        'vert.def' => {
            source => "vert",
            params => "def",
            expect => <<'#18...........',
# if $w->vert is tokenized as type 'U' then the ? will start a quote
# and an error will occur.
sub vert {
}

sub Restore {
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}
#18...........
        },

        'vmll.def' => {
            source => "vmll",
            params => "def",
            expect => <<'#19...........',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {
        is_deeply( \@init_metas_called, [1] )
          || diag( Dumper( \@init_metas_called ) );
    }

    This has the comma on the next line exception {
        Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)
    },
#19...........
        },

        'vmll.vmll' => {
            source => "vmll",
            params => "vmll",
            expect => <<'#20...........',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {is_deeply(\@init_metas_called, [1]) || diag(Dumper(\@init_metas_called))}

    This has the comma on the next line exception {
        Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)
    },
#20...........
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
