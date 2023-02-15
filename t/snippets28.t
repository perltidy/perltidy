# Created with: ./make_t.pl

# Contents:
#1 olbxl.olbxl2
#2 recombine5.def
#3 recombine6.def
#4 recombine7.def
#5 recombine8.def

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
        'def'    => "",
        'olbxl2' => <<'----------',
-olbxl='*'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'olbxl' => <<'----------',
            eval {
               require Ace };

            @list = map {
                $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_);
            } @list;

            $color = join(
                '/',
                sort {
                    $color_value{$::a} <=> $color_value{$::b};
                } keys %colors
            );

            @sorted = sort {
                $SortDir * $PageTotal{$a} <=> $SortDir * $PageTotal{$b}
                };
----------

        'recombine5' => <<'----------',
# recombine uses reverse optimization
$rotate = Math::MatrixReal->new_from_string( "[ " . cos($theta) . " " . -sin($theta) . " ]\n" . "[ " . sin($theta) . " " . cos($theta) . " ]\n" );
----------

        'recombine6' => <<'----------',
# recombine operation uses forward optimization
	$filecol =
	    (/^$/)     ? $filecol					 :
	    (s/^\+//)  ? $filecol  + $_					 :
	    (s/^\-//)  ? $filecol  - $_					 :
	    (s/^>//)   ? ($filecol + $_) % $pages			 :
	    (s/^]//)   ? (($filecol + $_ >= $pages) ? 0 : $filecol + $_) :
	    (s/^<//)   ? ($filecol - $_) % $pages			 :
	    (s/^\[//)  ? (($filecol == 0) ? $pages - ($pages % $_ || $_) :
			  ($filecol - $_ < 0) ? 0 : $filecol - $_)	 :
	    (/^\d/)    ? $_ - 1						 :
	    (s/^\\?//) ? (($col{$_}, $row{$_}) = &pageto($_))[0]	 : 0;
----------

        'recombine7' => <<'----------',
    # recombine uses forward optimization, must recombine at =
    my $J = int( 365.25 * ( $y + 4712 ) ) +
      int( ( 30.6 * $m ) + 0.5 ) + 59 + $d - 0.5;
----------

        'recombine8' => <<'----------',
# recombine uses normal forward mode
$v_gb = -1*(eval($pmt_gb))*(-1+((((-1+(1/((eval($i_gb)/100)+1))**  ((eval($n_gb)-1)))))/(eval($i_gb)/100)));
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'olbxl.olbxl2' => {
            source => "olbxl",
            params => "olbxl2",
            expect => <<'#1...........',
            eval {
                require Ace;
            };

            @list = map {
                $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_);
            } @list;

            $color = join(
                '/',
                sort {
                    $color_value{$::a} <=> $color_value{$::b};
                } keys %colors
            );

            @sorted = sort {
                $SortDir * $PageTotal{$a} <=> $SortDir * $PageTotal{$b}
            };
#1...........
        },

        'recombine5.def' => {
            source => "recombine5",
            params => "def",
            expect => <<'#2...........',
# recombine uses reverse optimization
$rotate =
  Math::MatrixReal->new_from_string( "[ "
      . cos($theta) . " "
      . -sin($theta) . " ]\n" . "[ "
      . sin($theta) . " "
      . cos($theta)
      . " ]\n" );
#2...........
        },

        'recombine6.def' => {
            source => "recombine6",
            params => "def",
            expect => <<'#3...........',
        # recombine operation uses forward optimization
        $filecol =
            (/^$/)    ? $filecol
          : (s/^\+//) ? $filecol + $_
          : (s/^\-//) ? $filecol - $_
          : (s/^>//)  ? ( $filecol + $_ ) % $pages
          : (s/^]//)  ? ( ( $filecol + $_ >= $pages ) ? 0 : $filecol + $_ )
          : (s/^<//)  ? ( $filecol - $_ ) % $pages
          : (s/^\[//) ? (
              ( $filecol == 0 )     ? $pages - ( $pages % $_ || $_ )
            : ( $filecol - $_ < 0 ) ? 0
            :                         $filecol - $_
          )
          : (/^\d/)    ? $_ - 1
          : (s/^\\?//) ? ( ( $col{$_}, $row{$_} ) = &pageto($_) )[0]
          :              0;
#3...........
        },

        'recombine7.def' => {
            source => "recombine7",
            params => "def",
            expect => <<'#4...........',
    # recombine uses forward optimization, must recombine at =
    my $J = int( 365.25 * ( $y + 4712 ) ) +
      int( ( 30.6 * $m ) + 0.5 ) + 59 + $d - 0.5;
#4...........
        },

        'recombine8.def' => {
            source => "recombine8",
            params => "def",
            expect => <<'#5...........',
# recombine uses normal forward mode
$v_gb = -1 * ( eval($pmt_gb) ) * (
    -1 + (
        (
            (
                (
                    -1 + ( 1 / ( ( eval($i_gb) / 100 ) + 1 ) )
                      **( ( eval($n_gb) - 1 ) )
                )
            )
        ) / ( eval($i_gb) / 100 )
    )
);
#5...........
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
