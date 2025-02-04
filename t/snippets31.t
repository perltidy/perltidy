# Created with: ./make_t.pl

# Contents:
#1 btct.btct2
#2 btct.btct3
#3 btct.def
#4 c424.c424
#5 c424.def
#6 ils.def
#7 ils.ils
#8 mutt.def
#9 mutt.mutt1
#10 c446.c446
#11 c446.def
#12 pot.def
#13 pot.pot1
#14 kblx.def
#15 kblx.kblx1
#16 kblx.kblx2
#17 kblx.kblx3
#18 kblx.kblx4
#19 kblx.kblx5

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
        'btct2' => "-btct=1 -atc -wtc=1",
        'btct3' => "-btct=1 -atc -wtc=1",
        'c424'  => "-naws -qwaf",
        'c446'  => <<'----------',
--qw-as-function
--paren-tightness=2
----------
        'def'   => "",
        'ils'   => "-nils -bos",
        'kblx1' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='b{' # inverse of -bbb
----------
        'kblx2' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='b}' # inverse of -blbc
----------
        'kblx3' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='{b' # inverse of -blao
----------
        'kblx4' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='}b' # not an inverse
----------
        'kblx5' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='bp' # inverse of -blbp
----------
        'mutt1' => <<'----------',
-mutt='q*'
----------
        'pot1' => <<'----------',
-pot='->'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

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

        'c424' => <<'----------',
my @chars = qw(   | / - \ | / - \    );
my @chars = qw(| / - \ | / - \ );
----------

        'c446' => <<'----------',
push @paths, qw(
    ../\
    c:.\\../\
    c:/\..//
    c://.\/./\
    \\.\\../\
    //\..//
    //.\/./\
);
----------

        'ils' => <<'----------',
$z = sqrt( $x**2 + $y**2 )
;
----------

        'kblx' => <<'----------',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
----------

        'mutt' => <<'----------',
my $rlist = [qw(alpha beta gamma)];
$aqx->appendChild(
        $parser->parse_balanced_chunk(qq(<param name="skv">$skv</param>)) );
----------

        'pot' => <<'----------',
sub bla_p( $value = 42 ) {
    return Mojo::Promise->resolve($value)->then( sub { shift() / 2 } ) ->then( sub { shift() + 6 } )->then( sub { shift() / 2 } ) ->catch( sub { warn shift } );
}
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'btct.btct2' => {
            source => "btct",
            params => "btct2",
            expect => <<'#1...........',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$lut = byte [
    [
        0, 0, 0,
    ], [
        10, 1, 10,
    ], [
        2, 20, 20,
    ], [
        30, 30, 3,
    ],
];
#1...........
        },

        'btct.btct3' => {
            source => "btct",
            params => "btct3",
            expect => <<'#2...........',
$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$w->bind(
    '<Page_Down>' => xx,
);

$lut = byte [
    [
        0, 0, 0,
    ], [
        10, 1, 10,
    ], [
        2, 20, 20,
    ], [
        30, 30, 3,
    ],
];
#2...........
        },

        'btct.def' => {
            source => "btct",
            params => "def",
            expect => <<'#3...........',
$w->bind( '<Page_Down>' => xx, );

$w->bind( '<Page_Down>' => xx, );

$w->bind( '<Page_Down>' => xx );

$w->bind( '<Page_Down>' => xx );

$lut = byte [ [ 0, 0, 0 ], [ 10, 1, 10 ], [ 2, 20, 20 ], [ 30, 30, 3 ], ];
#3...........
        },

        'c424.c424' => {
            source => "c424",
            params => "c424",
            expect => <<'#4...........',
my @chars = qw( | / - \ | / - \ );
my @chars = qw(| / - \ | / - \ );
#4...........
        },

        'c424.def' => {
            source => "c424",
            params => "def",
            expect => <<'#5...........',
my @chars = qw(   | / - \ | / - \    );
my @chars = qw(| / - \ | / - \ );
#5...........
        },

        'ils.def' => {
            source => "ils",
            params => "def",
            expect => <<'#6...........',
$z = sqrt( $x**2 + $y**2 );
#6...........
        },

        'ils.ils' => {
            source => "ils",
            params => "ils",
            expect => <<'#7...........',
$z = sqrt( $x**2 + $y**2 )
;
#7...........
        },

        'mutt.def' => {
            source => "mutt",
            params => "def",
            expect => <<'#8...........',
my $rlist = [qw(alpha beta gamma)];
$aqx->appendChild(
    $parser->parse_balanced_chunk(qq(<param name="skv">$skv</param>)) );
#8...........
        },

        'mutt.mutt1' => {
            source => "mutt",
            params => "mutt1",
            expect => <<'#9...........',
my $rlist = [ qw(alpha beta gamma) ];
$aqx->appendChild(
    $parser->parse_balanced_chunk( qq(<param name="skv">$skv</param>) ) );
#9...........
        },

        'c446.c446' => {
            source => "c446",
            params => "c446",
            expect => <<'#10...........',
push @paths,
  qw( ../\ c:.\\../\ c:/\..// c://.\/./\ \\.\\../\ //\..// //.\/./\ );
#10...........
        },

        'c446.def' => {
            source => "c446",
            params => "def",
            expect => <<'#11...........',
push @paths, qw(
  ../\
  c:.\\../\
  c:/\..//
  c://.\/./\
  \\.\\../\
  //\..//
  //.\/./\
);
#11...........
        },

        'pot.def' => {
            source => "pot",
            params => "def",
            expect => <<'#12...........',
sub bla_p( $value = 42 ) {
    return Mojo::Promise->resolve($value)
      ->then( sub { shift() / 2 } )
      ->then( sub { shift() + 6 } )
      ->then( sub { shift() / 2 } )
      ->catch( sub { warn shift } );
}
#12...........
        },

        'pot.pot1' => {
            source => "pot",
            params => "pot1",
            expect => <<'#13...........',
sub bla_p( $value = 42 ) {
    return Mojo::Promise->resolve($value)->then( sub { shift() / 2 } )
      ->then( sub { shift() + 6 } )->then( sub { shift() / 2 } )
      ->catch( sub { warn shift } );
}
#13...........
        },

        'kblx.def' => {
            source => "kblx",
            params => "def",
            expect => <<'#14...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
#14...........
        },

        'kblx.kblx1' => {
            source => "kblx",
            params => "kblx1",
            expect => <<'#15...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;
    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];
    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
#15...........
        },

        'kblx.kblx2' => {
            source => "kblx",
            params => "kblx2",
            expect => <<'#16...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;
}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;
}    ## static side comment

# coment before side comment

# comment after blank and comment
#16...........
        },

        'kblx.kblx3' => {
            source => "kblx",
            params => "kblx3",
            expect => <<'#17...........',
package A::B;

sub write_line {
    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {
    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
#17...........
        },

        'kblx.kblx4' => {
            source => "kblx",
            params => "kblx4",
            expect => <<'#18...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}
package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment
# coment before side comment

# comment after blank and comment
#18...........
        },

        'kblx.kblx5' => {
            source => "kblx",
            params => "kblx5",
            expect => <<'#19...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}
package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
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
