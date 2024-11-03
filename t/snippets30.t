# Created with: ./make_t.pl

# Contents:
#1 git143.def
#2 git143.git143
#3 git146.def
#4 git146.git146
#5 altc.altc1
#6 altc.altc2
#7 altc.def
#8 dltc.def
#9 dltc.dltc1
#10 dltc.dltc2
#11 logical_xor.def
#12 csc.csc3
#13 git159.def
#14 git159.git159
#15 git162.def
#16 git162.git162
#17 qwaf.def
#18 qwaf.qwaf
#19 btct.btct1

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
        'altc1'  => "-naltc -atc -wtc=m",
        'altc2'  => "-altc -atc -wtc=m",
        'btct1'  => "-btct=1",
        'csc3'   => "-csc -csci=2 -ncscb",
        'def'    => "",
        'dltc1'  => "-dtc -wtc=0",
        'dltc2'  => "-dtc -wtc=0 -ndltc",
        'git143' => "-atc -wtc=h",
        'git146' => <<'----------',
# testing three dash parameters
---add-trailing-commas
---unknown-future-option
---wtc=h
----------
        'git159' => "-bl -nsbl",
        'git162' => "-nwrs=A",
        'qwaf'   => <<'----------',
# git164
-qwaf
-sfp
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'altc' => <<'----------',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
----------

        'btct' => <<'----------',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind( '<Page_Down>' => xx,);

$w->bind(
    '<Page_Down>' => xx
);

$w->bind( '<Page_Down>' => xx);

$lut = byte [ [ 0, 0, 0 ], [ 10, 1, 10 ], [ 2, 20, 20 ], [ 30, 30, 3 ], ];
----------

        'csc' => <<'----------',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        } ## end sub message

        my $message =sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        };
----------

        'dltc' => <<'----------',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
----------

        'git143' => <<'----------',
# include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
----------

        'git146' => <<'----------',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
----------

        'git159' => <<'----------',
sub example {
    my $ex = 0;
    if ($ex)
    {
        print "yay\n";
    }
}
----------

        'git162' => <<'----------',
if ( $x in : eq @some_strings ) {
    say "x is one of the given strings";
}

match( $n : == ) {
    case (1) { say "It's one" }
    case (2) { say "It's two" }
    case (3) { say "It's three" }
    case (4), case (5) { say "It's four or five" }
    case if ( $n < 10 ) { say "It's less than ten" }
    default             { say "It's something else" }
}
----------

        'logical_xor' => <<'----------',
$x^^$y and say "One of x or y is true, but not both";
----------

        'qwaf' => <<'----------',
use Digest::MD5 qw( md5_hex );

@fields = qw( $st_dev	   $st_ino    $st_mode
      $st_nlink   $st_uid    $st_gid
      $st_rdev    $st_size
      $st_atime   $st_mtime  $st_ctime
      $st_blksize $st_blocks
);

@hdr_colors = qw(
        CadetBlue1
        MediumPurple1
        turquoise1
        PaleTurquoise1
        SlateBlue1
    );

# has blank line, so keep line breaks
@hdr_colors = qw(

        CadetBlue1
        MediumPurple1
        turquoise1
        PaleTurquoise1
        SlateBlue1
    );

@list = qw( \ );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'git143.def' => {
            source => "git143",
            params => "def",
            expect => <<'#1...........',
            # include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
#1...........
        },

        'git143.git143' => {
            source => "git143",
            params => "git143",
            expect => <<'#2...........',
            # include '=>' in comma count to allow adding trailing comma here
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ],
            );
#2...........
        },

        'git146.def' => {
            source => "git146",
            params => "def",
            expect => <<'#3...........',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ]
            );
#3...........
        },

        'git146.git146' => {
            source => "git146",
            params => "git146",
            expect => <<'#4...........',
            my %strips = (
                1 => [
                    [ [ 1750, 150, ], [ 1850, 150, ], ],
                    [ [ 1950, 150, ], [ 2050, 150, ], ],
                ],
            );
#4...........
        },

        'altc.altc1' => {
            source => "altc",
            params => "altc1",
            expect => <<'#5...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    }
);
#5...........
        },

        'altc.altc2' => {
            source => "altc",
            params => "altc2",
            expect => <<'#6...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
#6...........
        },

        'altc.def' => {
            source => "altc",
            params => "def",
            expect => <<'#7...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
#7...........
        },

        'dltc.def' => {
            source => "dltc",
            params => "def",
            expect => <<'#8...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version,
    },
);
#8...........
        },

        'dltc.dltc1' => {
            source => "dltc",
            params => "dltc1",
            expect => <<'#9...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    }
);
#9...........
        },

        'dltc.dltc2' => {
            source => "dltc",
            params => "dltc2",
            expect => <<'#10...........',
$self->make_grammar(
    {
        iterator => $self->_iterator,
        parser   => $self,
        version  => $self->version
    },
);
#10...........
        },

        'logical_xor.def' => {
            source => "logical_xor",
            params => "def",
            expect => <<'#11...........',
$x ^^ $y and say "One of x or y is true, but not both";
#11...........
        },

        'csc.csc3' => {
            source => "csc",
            params => "csc3",
            expect => <<'#12...........',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            } ## end if ( !defined( $_[0] ))
            else {
                print( $_[0], "\n" );
            } ## end else [ if ( !defined( $_[0] ))
        } ## end sub message

        my $message = sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            } ## end if ( !defined( $_[0] ))
            else {
                print( $_[0], "\n" );
            } ## end else [ if ( !defined( $_[0] ))
        }; ## end $message = sub
#12...........
        },

        'git159.def' => {
            source => "git159",
            params => "def",
            expect => <<'#13...........',
sub example {
    my $ex = 0;
    if ($ex) {
        print "yay\n";
    }
}
#13...........
        },

        'git159.git159' => {
            source => "git159",
            params => "git159",
            expect => <<'#14...........',
sub example {
    my $ex = 0;
    if ($ex)
    {
        print "yay\n";
    }
}
#14...........
        },

        'git162.def' => {
            source => "git162",
            params => "def",
            expect => <<'#15...........',
if ( $x in : eq @some_strings ) {
    say "x is one of the given strings";
}

match( $n : == ) {
    case (1) { say "It's one" }
    case (2) { say "It's two" }
    case (3) { say "It's three" }
    case (4), case (5) { say "It's four or five" }
    case if ( $n < 10 ) { say "It's less than ten" }
    default             { say "It's something else" }
}
#15...........
        },

        'git162.git162' => {
            source => "git162",
            params => "git162",
            expect => <<'#16...........',
if ( $x in :eq @some_strings ) {
    say "x is one of the given strings";
}

match( $n :== ) {
    case (1) { say "It's one" }
    case (2) { say "It's two" }
    case (3) { say "It's three" }
    case (4), case (5) { say "It's four or five" }
    case if ( $n < 10 ) { say "It's less than ten" }
    default             { say "It's something else" }
}
#16...........
        },

        'qwaf.def' => {
            source => "qwaf",
            params => "def",
            expect => <<'#17...........',
use Digest::MD5 qw( md5_hex );

@fields = qw( $st_dev	   $st_ino    $st_mode
  $st_nlink   $st_uid    $st_gid
  $st_rdev    $st_size
  $st_atime   $st_mtime  $st_ctime
  $st_blksize $st_blocks
);

@hdr_colors = qw(
  CadetBlue1
  MediumPurple1
  turquoise1
  PaleTurquoise1
  SlateBlue1
);

# has blank line, so keep line breaks
@hdr_colors = qw(

  CadetBlue1
  MediumPurple1
  turquoise1
  PaleTurquoise1
  SlateBlue1
);

@list = qw( \ );
#17...........
        },

        'qwaf.qwaf' => {
            source => "qwaf",
            params => "qwaf",
            expect => <<'#18...........',
use Digest::MD5 qw(md5_hex);

@fields = qw(
    $st_dev  $st_ino  $st_mode  $st_nlink $st_uid   $st_gid
    $st_rdev $st_size $st_atime $st_mtime $st_ctime $st_blksize
    $st_blocks
);

@hdr_colors =
  qw( CadetBlue1 MediumPurple1 turquoise1 PaleTurquoise1 SlateBlue1 );

# has blank line, so keep line breaks
@hdr_colors = qw(

    CadetBlue1
    MediumPurple1
    turquoise1
    PaleTurquoise1
    SlateBlue1
);

@list = qw( \ );
#18...........
        },

        'btct.btct1' => {
            source => "btct",
            params => "btct1",
            expect => <<'#19...........',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind( '<Page_Down>' => xx );

$w->bind( '<Page_Down>' => xx );

$lut = byte [
    [ 0, 0, 0 ], [ 10, 1, 10 ], [ 2, 20, 20 ], [ 30, 30, 3 ],
];
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
