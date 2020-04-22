# Created with: ./make_t.pl

# Contents:
#1 misc_tests.misc_tests
#2 outdent.def
#3 outdent.outdent1
#4 sbq.def
#5 sbq.sbq0
#6 sbq.sbq2
#7 tightness.def
#8 tightness.tightness1
#9 tightness.tightness2
#10 tightness.tightness3

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
        'def'        => "",
        'misc_tests' => <<'----------',
-sts -ssc -sfs -nsak="my for" -ndsm
----------
        'outdent1'   => "-nola -okw",
        'sbq0'       => "-sbq=0",
        'sbq2'       => "-sbq=2",
        'tightness1' => "-pt=0 -sbt=0 -bt=0 -bbt=0",
        'tightness2' => <<'----------',
-pt=1 -sbt=1 -bt=1 -bbt=1

----------
        'tightness3' => <<'----------',
-pt=2 -sbt=2 -bt=2 -bbt=2

----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'misc_tests' => <<'----------',
for ( @a = @$ap, $u = shift @a; @a; $u = $v ) { ... } # test -sfs 
$i = 1 ;     #  test -sts
$i = 0;    ##  =1;  test -ssc
;;;; # test -ndsm
my ( $a, $b, $c ) = @_;    # test -nsak="my for"
----------

        'outdent' => <<'----------',
# test -nola -okw
        my $i;
      LOOP: while ( $i = <FOTOS> ) {
            chomp($i);
            next unless $i;
            fixit($i);
        }

----------

        'sbq' => <<'----------',
       $str1=\"string1";
       $str2=\ 'string2';
----------

        'tightness' => <<'----------',
if (( my $len_tab = length( $tabstr )  ) > 0) {  }  # test -pt
$width = $col[ $j + $k ] - $col[ $j ]; # test -sbt
$obj->{ $parsed_sql->{ 'table' }[0] };  # test -bt
%bf = map { $_ => -M $_ } grep { /\.deb$/ } dirents '.';  # test -bbt
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'misc_tests.misc_tests' => {
            source => "misc_tests",
            params => "misc_tests",
            expect => <<'#1...........',
for( @a = @$ap, $u = shift @a ; @a ; $u = $v ) { ...  }    # test -sfs
$i = 1 ;                                                   #  test -sts
$i = 0 ; ##  =1;  test -ssc
;
;
;
;                                                          # test -ndsm
my( $a, $b, $c ) = @_ ;                                    # test -nsak="my for"
#1...........
        },

        'outdent.def' => {
            source => "outdent",
            params => "def",
            expect => <<'#2...........',
        # test -nola -okw
        my $i;
      LOOP: while ( $i = <FOTOS> ) {
            chomp($i);
            next unless $i;
            fixit($i);
        }

#2...........
        },

        'outdent.outdent1' => {
            source => "outdent",
            params => "outdent1",
            expect => <<'#3...........',
        # test -nola -okw
        my $i;
        LOOP: while ( $i = <FOTOS> ) {
            chomp($i);
          next unless $i;
            fixit($i);
        }

#3...........
        },

        'sbq.def' => {
            source => "sbq",
            params => "def",
            expect => <<'#4...........',
    $str1 = \"string1";
    $str2 = \ 'string2';
#4...........
        },

        'sbq.sbq0' => {
            source => "sbq",
            params => "sbq0",
            expect => <<'#5...........',
    $str1 = \"string1";
    $str2 = \'string2';
#5...........
        },

        'sbq.sbq2' => {
            source => "sbq",
            params => "sbq2",
            expect => <<'#6...........',
    $str1 = \ "string1";
    $str2 = \ 'string2';
#6...........
        },

        'tightness.def' => {
            source => "tightness",
            params => "def",
            expect => <<'#7...........',
if ( ( my $len_tab = length($tabstr) ) > 0 ) { }    # test -pt
$width = $col[ $j + $k ] - $col[$j];                # test -sbt
$obj->{ $parsed_sql->{'table'}[0] };                # test -bt
%bf = map { $_ => -M $_ } grep { /\.deb$/ } dirents '.';    # test -bbt
#7...........
        },

        'tightness.tightness1' => {
            source => "tightness",
            params => "tightness1",
            expect => <<'#8...........',
if ( ( my $len_tab = length( $tabstr ) ) > 0 ) { }    # test -pt
$width = $col[ $j + $k ] - $col[ $j ];                # test -sbt
$obj->{ $parsed_sql->{ 'table' }[ 0 ] };              # test -bt
%bf = map { $_ => -M $_ } grep { /\.deb$/ } dirents '.';    # test -bbt
#8...........
        },

        'tightness.tightness2' => {
            source => "tightness",
            params => "tightness2",
            expect => <<'#9...........',
if ( ( my $len_tab = length($tabstr) ) > 0 ) { }    # test -pt
$width = $col[ $j + $k ] - $col[$j];                # test -sbt
$obj->{ $parsed_sql->{'table'}[0] };                # test -bt
%bf = map { $_ => -M $_ } grep {/\.deb$/} dirents '.';    # test -bbt
#9...........
        },

        'tightness.tightness3' => {
            source => "tightness",
            params => "tightness3",
            expect => <<'#10...........',
if ((my $len_tab = length($tabstr)) > 0) { }    # test -pt
$width = $col[$j + $k] - $col[$j];              # test -sbt
$obj->{$parsed_sql->{'table'}[0]};              # test -bt
%bf = map {$_ => -M $_} grep {/\.deb$/} dirents '.';    # test -bbt
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
