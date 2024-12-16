# Created with: ./make_t.pl

# Contents:
#1 gnu5.gnu
#2 wngnu1.def
#3 olbs.def
#4 olbs.olbs0
#5 olbs.olbs2
#6 break_old_methods.break_old_methods
#7 break_old_methods.def
#8 bom1.bom
#9 bom1.def
#10 align28.def
#11 align29.def
#12 align30.def
#13 git09.def
#14 git09.git09
#15 git14.def
#16 sal.def
#17 sal.sal
#18 spp.def
#19 spp.spp0

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
        'bom'               => "-bom -wn",
        'break_old_methods' => "--break-at-old-method-breakpoints",
        'def'               => "",
        'git09'             => "-ce -cbl=map,sort,grep",
        'gnu'               => "-gnu",
        'olbs0'             => "-olbs=0",
        'olbs2'             => "-olbs=2",
        'sal'               => <<'----------',
-sal='method fun'
----------
        'spp0' => "-spp=0",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align28' => <<'----------',
# tests for 'delete_needless_parens'
# align all '='s; but do not align parens
my $w = $columns * $cell_w + ( $columns + 1 ) * $border;
my $h = $rows * $cell_h + ( $rows + 1 ) * $border;
my $img = new Gimp::Image( $w, $h, RGB );

# keep leading paren after if as alignment for padding
eval {
    if   ( $a->{'abc'} eq 'ABC' ) { no_op(23) }
    else                          { no_op(42) }
};
----------

        'align29' => <<'----------',
# alignment with lots of commas
is( floor(1.23441242), 1, "Basic floor(1.23441242) test" );
is( fmod( 3.5, 2.0 ), 1.5, "Basic fmod(3.5, 2.0) test" );
is( join( " ", frexp(1) ), "0.5 1", "Basic frexp(1) test" );
is( ldexp( 0, 1 ), 0, "Basic ldexp(0,1) test" );
is( log10(1),  0, "Basic log10(1) test" );
----------

        'align30' => <<'----------',
# commas on lhs align, commas on rhs do not (different subs)
($x,$y,$z)=spherical_to_cartesian($rho,$theta,$phi);
($rho_c,$theta,$z)=spherical_to_cylindrical($rho_s,$theta,$phi);
( $r2, $theta2, $z2 )=cartesian_to_cylindrical( $x1, $y1, $z1 );

# two-line if/elsif gets aligned 
if($i==$depth){$_++;}
elsif($i>$depth){$_=0;}
----------

        'bom1' => <<'----------',
# keep cuddled call chain with -bom
return Mojo::Promise->resolve(
    $query_params
)->then(
    &_reveal_event
)->then(sub ($code) {
    return $c->render(text => '', status => $code);
})->catch(sub {
    # 1. return error
    return $c->render(json => {}, status => 400);
});
----------

        'break_old_methods' => <<'----------',
my $q = $rs
   ->related_resultset('CDs')
   ->related_resultset('Tracks')
   ->search({
      'track.id' => { -ident => 'none_search.id' },
   })
   ->as_query;
----------

        'git09' => <<'----------',
# no one-line block for first map with -ce -cbl=map,sort,grep
@sorted = map {
    $_->[0]
} sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0] 
} map {
    [$_, length($_)]
} @unsorted;
----------

        'git14' => <<'----------',
# git#14; do not break at trailing 'or'
$second = {
    key1 => 'aaa',
    key2 => 'bbb',
} if $flag1 or $flag2;
----------

        'gnu5' => <<'----------',
        # side comments limit gnu type formatting with l=80; note extra comma
        push @tests, [
            "Lowest code point requiring 13 bytes to represent",    # 2**36
            "\xff\x80\x80\x80\x80\x80\x81\x80\x80\x80\x80\x80\x80",
            ($::is64bit) ? 0x1000000000 : -1,    # overflows on 32bit
          ],
          ;
----------

        'olbs' => <<'----------',
for $x ( 1, 2 ) { s/(.*)/+$1/ }
for $x ( 1, 2 ) { s/(.*)/+$1/ }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked" }
for $x ( 1, 2 ) { s/(.*)/+$1/; }
for $x ( 1, 2 ) { s/(.*)/+$1/; }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked"; }
----------

        'sal' => <<'----------',
sub get_val () {

}

method get_value () {

}

fun get_other_value () {

}
----------

        'spp' => <<'----------',
sub get_val() { }

sub get_Val  () { }

sub Get_val		() { }
my $sub1=sub                     () { };
my $sub2=sub () { };
----------

        'wngnu1' => <<'----------',
    # test with -wn -gnu
    foreach my $parameter (
        qw(
        set_themes
        add_themes
        severity
        maximum_violations_per_document
        _non_public_data
        )
      )
    {
        is(
            $config->get($parameter),
            undef,
            qq<"$parameter" is not defined via get() for $policy_short_name.>,
        );
    }
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'gnu5.gnu' => {
            source => "gnu5",
            params => "gnu",
            expect => <<'#1...........',
        # side comments limit gnu type formatting with l=80; note extra comma
        push @tests, [
                 "Lowest code point requiring 13 bytes to represent",    # 2**36
                 "\xff\x80\x80\x80\x80\x80\x81\x80\x80\x80\x80\x80\x80",
                 ($::is64bit) ? 0x1000000000 : -1,    # overflows on 32bit
                     ],
          ;
#1...........
        },

        'wngnu1.def' => {
            source => "wngnu1",
            params => "def",
            expect => <<'#2...........',
    # test with -wn -gnu
    foreach my $parameter (
        qw(
        set_themes
        add_themes
        severity
        maximum_violations_per_document
        _non_public_data
        )
      )
    {
        is(
            $config->get($parameter),
            undef,
            qq<"$parameter" is not defined via get() for $policy_short_name.>,
        );
    }
#2...........
        },

        'olbs.def' => {
            source => "olbs",
            params => "def",
            expect => <<'#3...........',
for $x ( 1, 2 ) { s/(.*)/+$1/ }
for $x ( 1, 2 ) { s/(.*)/+$1/ }     # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked" }
for $x ( 1, 2 ) { s/(.*)/+$1/; }
for $x ( 1, 2 ) { s/(.*)/+$1/; }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked"; }
#3...........
        },

        'olbs.olbs0' => {
            source => "olbs",
            params => "olbs0",
            expect => <<'#4...........',
for $x ( 1, 2 ) { s/(.*)/+$1/ }
for $x ( 1, 2 ) { s/(.*)/+$1/ }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked" }
for $x ( 1, 2 ) { s/(.*)/+$1/ }
for $x ( 1, 2 ) { s/(.*)/+$1/ }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked"; }
#4...........
        },

        'olbs.olbs2' => {
            source => "olbs",
            params => "olbs2",
            expect => <<'#5...........',
for $x ( 1, 2 ) { s/(.*)/+$1/; }
for $x ( 1, 2 ) { s/(.*)/+$1/; }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked"; }
for $x ( 1, 2 ) { s/(.*)/+$1/; }
for $x ( 1, 2 ) { s/(.*)/+$1/; }    # side comment
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked"; }
#5...........
        },

        'break_old_methods.break_old_methods' => {
            source => "break_old_methods",
            params => "break_old_methods",
            expect => <<'#6...........',
my $q = $rs
  ->related_resultset('CDs')
  ->related_resultset('Tracks')
  ->search(
    {
        'track.id' => { -ident => 'none_search.id' },
    }
  )->as_query;
#6...........
        },

        'break_old_methods.def' => {
            source => "break_old_methods",
            params => "def",
            expect => <<'#7...........',
my $q = $rs->related_resultset('CDs')->related_resultset('Tracks')->search(
    {
        'track.id' => { -ident => 'none_search.id' },
    }
)->as_query;
#7...........
        },

        'bom1.bom' => {
            source => "bom1",
            params => "bom",
            expect => <<'#8...........',
# keep cuddled call chain with -bom
return Mojo::Promise->resolve(
    $query_params
)->then(
    &_reveal_event
)->then( sub ($code) {
    return $c->render( text => '', status => $code );
} )->catch( sub {

    # 1. return error
    return $c->render( json => {}, status => 400 );
} );
#8...........
        },

        'bom1.def' => {
            source => "bom1",
            params => "def",
            expect => <<'#9...........',
# keep cuddled call chain with -bom
return Mojo::Promise->resolve($query_params)->then(&_reveal_event)->then(
    sub ($code) {
        return $c->render( text => '', status => $code );
    }
)->catch(
    sub {
        # 1. return error
        return $c->render( json => {}, status => 400 );
    }
);
#9...........
        },

        'align28.def' => {
            source => "align28",
            params => "def",
            expect => <<'#10...........',
# tests for 'delete_needless_parens'
# align all '='s; but do not align parens
my $w   = $columns * $cell_w + ( $columns + 1 ) * $border;
my $h   = $rows * $cell_h + ( $rows + 1 ) * $border;
my $img = new Gimp::Image( $w, $h, RGB );

# keep leading paren after if as alignment for padding
eval {
    if   ( $a->{'abc'} eq 'ABC' ) { no_op(23) }
    else                          { no_op(42) }
};
#10...........
        },

        'align29.def' => {
            source => "align29",
            params => "def",
            expect => <<'#11...........',
# alignment with lots of commas
is( floor(1.23441242),     1,       "Basic floor(1.23441242) test" );
is( fmod( 3.5, 2.0 ),      1.5,     "Basic fmod(3.5, 2.0) test" );
is( join( " ", frexp(1) ), "0.5 1", "Basic frexp(1) test" );
is( ldexp( 0, 1 ),         0,       "Basic ldexp(0,1) test" );
is( log10(1),              0,       "Basic log10(1) test" );
#11...........
        },

        'align30.def' => {
            source => "align30",
            params => "def",
            expect => <<'#12...........',
# commas on lhs align, commas on rhs do not (different subs)
( $x,     $y,      $z )  = spherical_to_cartesian( $rho, $theta, $phi );
( $rho_c, $theta,  $z )  = spherical_to_cylindrical( $rho_s, $theta, $phi );
( $r2,    $theta2, $z2 ) = cartesian_to_cylindrical( $x1, $y1, $z1 );

# two-line if/elsif gets aligned
if    ( $i == $depth ) { $_++; }
elsif ( $i > $depth )  { $_ = 0; }
#12...........
        },

        'git09.def' => {
            source => "git09",
            params => "def",
            expect => <<'#13...........',
# no one-line block for first map with -ce -cbl=map,sort,grep
@sorted =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0] }
  map  { [ $_, length($_) ] } @unsorted;
#13...........
        },

        'git09.git09' => {
            source => "git09",
            params => "git09",
            expect => <<'#14...........',
# no one-line block for first map with -ce -cbl=map,sort,grep
@sorted = map {
    $_->[0]
  } sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]
  } map {
    [ $_, length($_) ]
  } @unsorted;
#14...........
        },

        'git14.def' => {
            source => "git14",
            params => "def",
            expect => <<'#15...........',
# git#14; do not break at trailing 'or'
$second = {
    key1 => 'aaa',
    key2 => 'bbb',
} if $flag1 or $flag2;
#15...........
        },

        'sal.def' => {
            source => "sal",
            params => "def",
            expect => <<'#16...........',
sub get_val () {

}

method get_value () {

}

fun get_other_value() {

}
#16...........
        },

        'sal.sal' => {
            source => "sal",
            params => "sal",
            expect => <<'#17...........',
sub get_val () {

}

method get_value () {

}

fun get_other_value () {

}
#17...........
        },

        'spp.def' => {
            source => "spp",
            params => "def",
            expect => <<'#18...........',
sub get_val() { }

sub get_Val () { }

sub Get_val () { }
my $sub1 = sub () { };
my $sub2 = sub () { };
#18...........
        },

        'spp.spp0' => {
            source => "spp",
            params => "spp0",
            expect => <<'#19...........',
sub get_val() { }

sub get_Val() { }

sub Get_val() { }
my $sub1 = sub() { };
my $sub2 = sub() { };
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
