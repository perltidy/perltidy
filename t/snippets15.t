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

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test;
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
        'gnu'               => "-gnu",
        'olbs0'             => "-olbs=0",
        'olbs2'             => "-olbs=2",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

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
            "Lowest code point requiring 13 bytes to represent",      # 2**36
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
for $x ( 1, 2 ) { s/(.*)/+$1/ }    # side comment
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
